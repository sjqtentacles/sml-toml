(* Tests for sml-toml. *)

structure TomlTests =
struct
  open Harness
  open Toml

  (* Look up a key in a parsed root Table; raises on anything unexpected so a
     mis-shaped parse surfaces as a failed (raising) check rather than a silent
     wrong value. *)
  fun parseOk s =
    case parse s of
        Ok v => v
      | Err e => raise Fail ("parse failed: " ^ e)

  fun field (Table kvs) k =
        (case List.find (fn (k', _) => k' = k) kvs of
             SOME (_, v) => v
           | NONE => raise Fail ("missing key: " ^ k))
    | field _ _ = raise Fail "not a table"

  (* Structural equality on `value`. `value` is not an equality type because of
     `Float of real`, so we compare reals with `Real.==` and recurse. *)
  fun valEq (Str a, Str b) = a = b
    | valEq (Int a, Int b) = a = b
    | valEq (Float a, Float b) = Real.== (a, b)
    | valEq (Bool a, Bool b) = a = b
    | valEq (Datetime a, Datetime b) = a = b
    | valEq (Array a, Array b) =
        length a = length b andalso ListPair.all valEq (a, b)
    | valEq (Table a, Table b) =
        length a = length b
        andalso ListPair.all (fn ((ka, va), (kb, vb)) =>
                               ka = kb andalso valEq (va, vb)) (a, b)
    | valEq _ = false

  (* `toString` then re-`parse` should recover an equal value. We pass values
     already in canonical order (non-tables sorted first, then tables sorted)
     so the order-sensitive `valEq` matches the canonicalizing serializer. *)
  fun roundTrips v = valEq (v, parseOk (toString v))

  fun run () =
    let
      val () = section "scalars"

      val () = checkBool "string value"
        (true, valEq (field (parseOk "s = \"hello\"") "s", Str "hello"))
      val () = checkBool "integer value"
        (true, valEq (field (parseOk "n = 42") "n", Int 42))
      val () = checkBool "negative integer"
        (true, valEq (field (parseOk "n = -7") "n", Int ~7))
      val () = checkBool "positive-signed integer"
        (true, valEq (field (parseOk "n = +7") "n", Int 7))
      val () = checkBool "float value"
        (true, valEq (field (parseOk "f = 3.14") "f", Float 3.14))
      val () = checkBool "float with exponent"
        (true, valEq (field (parseOk "f = 1e3") "f", Float 1000.0))
      val () = checkBool "bool true"
        (true, valEq (field (parseOk "b = true") "b", Bool true))
      val () = checkBool "bool false"
        (true, valEq (field (parseOk "b = false") "b", Bool false))
      val () = checkBool "underscores in integer"
        (true, valEq (field (parseOk "n = 1_000") "n", Int 1000))
      (* TOML integers are 64-bit; parsed as arbitrary-precision IntInf so they
         are lossless and identical on MLton (32-bit int) and Poly/ML (63-bit) --
         the old `Int.fromString` raised Overflow under MLton past 2^31. *)
      val () = checkBool "large integer past 2^31 parses losslessly"
        (true, valEq (field (parseOk "n = 1700000000000") "n", Int 1700000000000))
      val () = checkBool "max 64-bit TOML integer parses losslessly"
        (true, valEq (field (parseOk "n = 9223372036854775807") "n",
                      Int (valOf (IntInf.fromString "9223372036854775807"))))

      val () = section "comments and blank lines"

      val doc1 =
        "# a leading comment\n\nx = 1   # trailing comment\n\n# another\ny = 2\n"
      val () = checkBool "value before comment" (true, valEq (field (parseOk doc1) "x", Int 1))
      val () = checkBool "value after blank lines" (true, valEq (field (parseOk doc1) "y", Int 2))
      val () = checkBool "comment-only doc is empty table"
        (true, valEq (parseOk "# just a comment\n\n", Table []))

      val () = section "tables and dotted keys"

      (* dotted key a.b.c = 1 builds nested tables *)
      val r3a = parseOk "a.b.c = 1\n"
      val () = checkBool "dotted key nests"
        (true, valEq (field (field (field r3a "a") "b") "c", Int 1))

      (* [table] header *)
      val r3b = parseOk "[server]\nhost = \"localhost\"\nport = 8080\n"
      val () = checkBool "[table] host"
        (true, valEq (field (field r3b "server") "host", Str "localhost"))
      val () = checkBool "[table] port"
        (true, valEq (field (field r3b "server") "port", Int 8080))

      (* dotted [a.b] header nests *)
      val r3c = parseOk "[a.b]\nk = 42\n"
      val () = checkBool "[a.b] nested header"
        (true, valEq (field (field (field r3c "a") "b") "k", Int 42))

      (* two sibling tables under a shared dotted prefix *)
      val r3d = parseOk "[a.b]\nx = 1\n[a.c]\ny = 2\n"
      val () = checkBool "shared prefix b.x"
        (true, valEq (field (field (field r3d "a") "b") "x", Int 1))
      val () = checkBool "shared prefix c.y"
        (true, valEq (field (field (field r3d "a") "c") "y", Int 2))

      val () = section "arrays and arrays-of-tables"

      val () = checkBool "int array"
        (true, valEq (field (parseOk "xs = [1, 2, 3]") "xs",
                      Array [Int 1, Int 2, Int 3]))
      val () = checkBool "empty array"
        (true, valEq (field (parseOk "xs = []") "xs", Array []))
      val () = checkBool "trailing comma array"
        (true, valEq (field (parseOk "xs = [1, 2, 3,]") "xs",
                      Array [Int 1, Int 2, Int 3]))
      val () = checkBool "nested array"
        (true, valEq (field (parseOk "xs = [[1, 2], [3]]") "xs",
                      Array [Array [Int 1, Int 2], Array [Int 3]]))
      val () = checkBool "heterogeneous array (documented allowed)"
        (true, valEq (field (parseOk "xs = [1, \"two\", true]") "xs",
                      Array [Int 1, Str "two", Bool true]))
      (* multiline array with comments between elements *)
      val multiArr = "xs = [\n  1, # one\n  2,\n  3,\n]\n"
      val () = checkBool "multiline array"
        (true, valEq (field (parseOk multiArr) "xs", Array [Int 1, Int 2, Int 3]))

      (* [[array of tables]]: two blocks -> Array of two Tables *)
      val aot = "[[products]]\nname = \"Hammer\"\n\n[[products]]\nname = \"Nail\"\n"
      val r4 = parseOk aot
      val () = checkBool "array-of-tables is an Array of 2 Tables"
        (true, valEq (field r4 "products",
                      Array [ Table [("name", Str "Hammer")],
                              Table [("name", Str "Nail")] ]))

      val () = section "inline tables"

      val () = checkBool "inline table"
        (true, valEq (field (parseOk "p = { x = 1, y = 2 }") "p",
                      Table [("x", Int 1), ("y", Int 2)]))
      val () = checkBool "empty inline table"
        (true, valEq (field (parseOk "p = {}") "p", Table []))
      val () = checkBool "nested inline table"
        (true, valEq (field (parseOk "p = { name = \"a\", pos = { x = 1, y = 2 } }") "p",
                      Table [("name", Str "a"),
                             ("pos", Table [("x", Int 1), ("y", Int 2)])]))
      val () = checkBool "dotted key inside inline table nests"
        (true, valEq (field (parseOk "p = { a.b = 1 }") "p",
                      Table [("a", Table [("b", Int 1)])]))

      val () = section "string escapes and literal strings"

      val () = checkBool "newline escape"
        (true, valEq (field (parseOk "s = \"a\\nb\"") "s", Str "a\nb"))
      val () = checkBool "tab escape"
        (true, valEq (field (parseOk "s = \"a\\tb\"") "s", Str "a\tb"))
      val () = checkBool "quote escape"
        (true, valEq (field (parseOk "s = \"a\\\"b\"") "s", Str "a\"b"))
      val () = checkBool "backslash escape"
        (true, valEq (field (parseOk "s = \"a\\\\b\"") "s", Str "a\\b"))
      val () = checkBool "unicode escape \\u0041 = A"
        (true, valEq (field (parseOk "s = \"\\u0041\"") "s", Str "A"))
      val () = checkBool "literal string keeps backslashes"
        (true, valEq (field (parseOk "s = 'a\\nb'") "s", Str "a\\nb"))

      val () = section "duplicate keys"

      val () = checkBool "duplicate key -> Err"
        (true, case parse "a = 1\na = 2\n" of Err _ => true | Ok _ => false)
      val () = checkBool "duplicate key in inline table -> Err"
        (true, case parse "p = { x = 1, x = 2 }" of Err _ => true | Ok _ => false)
      val () = checkBool "duplicate dotted leaf -> Err"
        (true, case parse "a.b = 1\na.b = 2\n" of Err _ => true | Ok _ => false)
      val () = checkBool "non-duplicate still Ok"
        (true, case parse "a = 1\nb = 2\n" of Ok _ => true | Err _ => false)

      val () = section "datetime (captured verbatim)"
      val () = checkBool "datetime lexeme captured"
        (true, valEq (field (parseOk "d = 1979-05-27T07:32:00Z") "d",
                      Datetime "1979-05-27T07:32:00Z"))

      val () = section "realistic fixture"

      val fixture = String.concat
        [ "# config\n",
          "title = \"sml-toml demo\"\n",
          "enabled = true\n",
          "\n",
          "[owner]\n",
          "name = \"Ada\"\n",
          "ports = [8000, 8001, 8002]\n",
          "\n",
          "[database]\n",
          "server = \"192.168.1.1\"\n",
          "connection_max = 5000\n",
          "ratio = 0.75\n",
          "\n",
          "[[servers]]\n",
          "name = \"alpha\"\n",
          "\n",
          "[[servers]]\n",
          "name = \"beta\"\n" ]
      val rf = parseOk fixture
      val () = checkBool "fixture title" (true, valEq (field rf "title", Str "sml-toml demo"))
      val () = checkBool "fixture enabled" (true, valEq (field rf "enabled", Bool true))
      val () = checkBool "fixture owner.name"
        (true, valEq (field (field rf "owner") "name", Str "Ada"))
      val () = checkBool "fixture owner.ports"
        (true, valEq (field (field rf "owner") "ports", Array [Int 8000, Int 8001, Int 8002]))
      val () = checkBool "fixture database.ratio"
        (true, valEq (field (field rf "database") "ratio", Float 0.75))
      val () = checkBool "fixture servers is array of 2 tables"
        (true, valEq (field rf "servers",
                      Array [ Table [("name", Str "alpha")],
                              Table [("name", Str "beta")] ]))

      val () = section "serializer: exact output"

      (* A small document serializes to exactly this TOML (non-table pairs
         first, then `[header]` sections; keys sorted ascending). *)
      val () = checkString "exact serialized document"
        ("title = \"TOML\"\n\n[owner]\nname = \"Tom\"\n",
         toString (Table [("title", Str "TOML"),
                          ("owner", Table [("name", Str "Tom")])]))

      (* Empty table -> empty document, which re-parses to an empty table. *)
      val () = checkString "empty table serializes empty" ("", toString (Table []))

      val () = section "serializer: round-trips"

      val () = checkBool "round-trip scalars"
        (true, roundTrips (Table [("a", Int 1), ("b", Bool true),
                                  ("c", Str "hi")]))
      val () = checkBool "round-trip negative integer"
        (true, roundTrips (Table [("n", Int ~7)]))
      val () = checkBool "round-trip array"
        (true, roundTrips (Table [("xs", Array [Int 1, Int 2, Int 3])]))
      val () = checkBool "round-trip empty array and empty table"
        (true, roundTrips (Table [("a", Array []), ("z", Table [])]))
      val () = checkBool "round-trip header + nested table"
        (true, roundTrips (Table [("title", Str "TOML"),
                                  ("owner", Table [("name", Str "Tom")])]))
      val () = checkBool "round-trip deeply nested tables"
        (true, roundTrips (Table [("pkg",
                  Table [("dep",
                    Table [("name", Str "x"), ("ver", Str "1.0")])])]))
      val () = checkBool "round-trip array of tables (inline)"
        (true, roundTrips (Table [("servers",
                  Array [Table [("name", Str "alpha")],
                         Table [("name", Str "beta")]])]))
      val () = checkBool "round-trip datetime lexeme"
        (true, roundTrips (Table [("d", Datetime "1979-05-27T07:32:00Z")]))
      val () = checkBool "round-trip quoted (non-bare) key"
        (true, roundTrips (Table [("ns.key", Int 1)]))

      (* Serializing then parsing is idempotent (stable) even when the source
         document lists keys in a non-canonical, mixed order. *)
      val () = checkString "serialization is idempotent on the fixture"
        (toString (parseOk fixture),
         toString (parseOk (toString (parseOk fixture))))
      val () = checkBool "fixture re-serialization re-parses"
        (true, case parse (toString (parseOk fixture)) of
                   Ok _ => true | Err _ => false)

      val () = section "serializer: floats and escaping"

      (* Floats always carry a decimal point and use "-" (never "~"). *)
      val fNeg = toString (Table [("x", Float ~2.5)])
      val () = checkString "negative float exact" ("x = -2.5\n", fNeg)
      val () = checkBool "float keeps a decimal point"
        (true, String.isSubstring "." fNeg)
      val () = checkBool "float never emits a tilde"
        (true, not (String.isSubstring "~" fNeg))
      val () = checkBool "round-trip floats"
        (true, roundTrips (Table [("a", Float 0.75), ("b", Float 3.14),
                                  ("c", Float ~2.5), ("d", Float 1000.0)]))

      (* A string with a quote and a newline escapes correctly and round-trips. *)
      val esc = Table [("s", Str "he said \"hi\"\nbye")]
      val escOut = toString esc
      val () = checkBool "escaped string round-trips" (true, roundTrips esc)
      val () = checkBool "output escapes the quote"
        (true, String.isSubstring "\\\"" escOut)
      val () = checkBool "output escapes the newline"
        (true, String.isSubstring "\\n" escOut)
    in
      ()
    end
end
