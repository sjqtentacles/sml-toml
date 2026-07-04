(* toml.sml -- TOML AST + parser, built on the vendored sml-parsec. *)

structure Toml :> TOML =
struct
  datatype value =
      Str      of string
    | Int      of IntInf.int              (* TOML integers are 64-bit; arbitrary precision here *)
    | Float    of real
    | Bool     of bool
    | Datetime of string
    | Array    of value list
    | Table    of (string * value) list

  datatype ('ok, 'err) result = Ok of 'ok | Err of 'err

  (* Aliases captured before `open CharParsec` (which introduces its own
     `Ok`/`Err`) so `parse` can still build OUR result type. *)
  fun resOk x  = Ok x
  fun resErr e = Err e

  (* A document is parsed first into a flat list of directives, then folded
     into a nested `Table`. This two-phase design keeps the grammar simple and
     localizes the duplicate-key / table-merge semantics in one place. *)
  datatype directive =
      Pair   of string list * value   (* key path (dotted) = value          *)
    | Std    of string list           (* [a.b]   standard table header       *)
    | ArrTab of string list           (* [[a.b]] array-of-tables header      *)

  (* Raised during the fold phase with a human-readable message. Caught by
     `parse` and turned into an `Error`. *)
  exception Toml of string

  local
    open CharParsec
    infix 1 >>= >>
    infix 1 <*
    infix 4 <*> <$>
    infixr 1 <|>
    infix 0 <?>

    (* ---- table assembly helpers (pure; no parsing) --------------------- *)

    (* Insert (path, v) into an association list `tbl`, creating intermediate
       tables for dotted paths. Rejects redefining an existing leaf or
       shadowing a non-table with a dotted path. Order is preserved. *)
    fun insertPath (_, [], _) = raise Toml "empty key"
      | insertPath (tbl, [k], v) =
          if List.exists (fn (k', _) => k' = k) tbl
          then raise Toml ("duplicate key: " ^ k)
          else tbl @ [(k, v)]
      | insertPath (tbl, k :: ks, v) =
          let
            fun go [] = [(k, Table (insertPath ([], ks, v)))]
              | go ((k', sub) :: rest) =
                  if k' = k then
                    case sub of
                        Table t => (k', Table (insertPath (t, ks, v))) :: rest
                      | _ => raise Toml ("key is not a table: " ^ k)
                  else (k', sub) :: go rest
          in
            go tbl
          end

    fun foldPairs ps =
      List.foldl (fn ((path, v), acc) => insertPath (acc, path, v)) [] ps

    (* ---- whitespace & comments ---------------------------------------- *)

    (* Inline whitespace only: spaces and tabs, NOT newlines. Newlines are
       structural in TOML (they terminate key/value pairs and headers). *)
    val isBlank = fn c => c = #" " orelse c = #"\t"
    val blanks  = skipMany (sat isBlank)

    val comment = char #"#" >> skipMany (sat (fn c => c <> #"\n")) >> return ()

    (* Skip inline whitespace then an optional comment, staying on this line. *)
    val sp = blanks >> (comment <|> return ())

    (* A newline (LF or CRLF). *)
    val newline = (string "\r\n" >> return ()) <|> (char #"\n" >> return ())

    (* Skip any run of blank lines / comment-only lines / inter-line space. *)
    val skipLines =
      skipMany (sat isBlank <|> (char #"\n" >> return #"\n")
                <|> (char #"\r" >> return #"\r")
                <|> (char #"#" >> skipMany (sat (fn c => c <> #"\n")) >> return #"#"))

    (* lexeme: run p, then consume trailing inline whitespace + comment. Used
       for tokens that live within a single logical line. *)
    fun lex p = p <* sp

    (* ---- keys ---------------------------------------------------------- *)

    val isBareKey =
      fn c => Char.isAlphaNum c orelse c = #"_" orelse c = #"-"

    val bareKey = many1 (sat isBareKey) >>= (fn cs => return (implode cs))

    (* A single key component: a bare key, or a quoted (basic/literal) key. The
       quoted forms reuse the string parsers defined below via `delay`. *)
    fun keyComponent () =
      bareKey
      <|> delay basicStringRaw
      <|> delay literalStringRaw

    (* Dotted key path: key ( ws '.' ws key )*. Whitespace is allowed around
       the dots. Returns the path as a list of components. *)
    and dottedKey () =
      lex (delay keyComponent) >>= (fn k =>
        many (lex (char #".") >> lex (delay keyComponent)) >>= (fn ks =>
          return (k :: ks)))

    (* ---- strings ------------------------------------------------------- *)

    and hexDigit () = sat Char.isHexDigit

    and hexVal c =
      if c >= #"0" andalso c <= #"9" then ord c - ord #"0"
      else if c >= #"a" andalso c <= #"f" then ord c - ord #"a" + 10
      else ord c - ord #"A" + 10

    (* Encode a Unicode scalar value as UTF-8 (TOML documents are UTF-8). *)
    and utf8 code =
      if code < 0x80 then String.str (chr code)
      else if code < 0x800 then
        implode [ chr (0xC0 + (code div 0x40)),
                  chr (0x80 + (code mod 0x40)) ]
      else if code < 0x10000 then
        implode [ chr (0xE0 + (code div 0x1000)),
                  chr (0x80 + ((code div 0x40) mod 0x40)),
                  chr (0x80 + (code mod 0x40)) ]
      else
        implode [ chr (0xF0 + (code div 0x40000)),
                  chr (0x80 + ((code div 0x1000) mod 0x40)),
                  chr (0x80 + ((code div 0x40) mod 0x40)),
                  chr (0x80 + (code mod 0x40)) ]

    and uEscape n =
      count n (delay hexDigit) >>= (fn hs =>
        return (utf8 (List.foldl (fn (c, acc) => acc * 16 + hexVal c) 0 hs)))

    and basicEscape () =
      char #"\\" >>
      ((char #"\"" >> return "\"")
       <|> (char #"\\" >> return "\\")
       <|> (char #"/"  >> return "/")
       <|> (char #"b"  >> return "\b")
       <|> (char #"f"  >> return "\f")
       <|> (char #"n"  >> return "\n")
       <|> (char #"r"  >> return "\r")
       <|> (char #"t"  >> return "\t")
       <|> (char #"u"  >> uEscape 4)
       <|> (char #"U"  >> uEscape 8)
       <?> "string escape")

    and basicChar () =
      (delay basicEscape)
      <|> (sat (fn c => c <> #"\"" andalso c <> #"\\" andalso c <> #"\n")
           >>= (fn c => return (str c)))

    (* Raw basic string (no whitespace skipping, no Str wrapper). *)
    and basicStringRaw () =
      char #"\"" >> many (delay basicChar) >>= (fn parts =>
      char #"\"" >> return (String.concat parts))

    (* Raw literal string: single quotes, no escapes at all. *)
    and literalStringRaw () =
      char #"'" >> many (sat (fn c => c <> #"'" andalso c <> #"\n")) >>= (fn cs =>
      char #"'" >> return (implode cs))

    (* ---- numbers ------------------------------------------------------- *)

    (* Optional leading sign as an SML-syntax prefix ("" or "~"). *)
    and sign () =
      (char #"+" >> return "")
      <|> (char #"-" >> return "~")
      <|> return ""

    (* A run of digits possibly separated by single underscores: 1_000 -> 1000.
       Underscores are stripped; we do not enforce that they sit between
       digits (the surrounding grammar already guarantees a leading digit). *)
    and digits () =
      digit >>= (fn d =>
        many (digit <|> (char #"_" >> delay (fn () => digit))) >>= (fn ds =>
          return (implode (d :: ds))))

    and fracPart () =
      char #"." >> (delay digits) >>= (fn ds => return ("." ^ ds))

    and expPart () =
      oneOf "eE" >> (delay sign) >>= (fn sg =>
        (delay digits) >>= (fn ds =>
          return ("e" ^ (if sg = "~" then "~" else "") ^ ds)))

    (* number := sign digits (frac exp? | exp)? ; frac/exp present => Float. *)
    and number () =
      (delay sign) >>= (fn sg =>
        (delay digits) >>= (fn ip =>
          optional (delay fracPart) >>= (fn fp =>
            optional (delay expPart) >>= (fn ep =>
              let
                val fps = Option.getOpt (fp, "")
                val eps = Option.getOpt (ep, "")
              in
                case (fp, ep) of
                    (NONE, NONE) =>
                      (* TOML integers are 64-bit; parse via `IntInf` (never
                         overflows) so a large valid integer is lossless and
                         identical on MLton (32-bit `int`) and Poly/ML (63-bit)
                         instead of raising `Overflow`. *)
                      (case IntInf.fromString (sg ^ ip) of
                           SOME n => return (Int n)
                         | NONE => fail "malformed integer")
                  | _ =>
                      (case Real.fromString (sg ^ ip ^ fps ^ eps) of
                           SOME r => return (Float r)
                         | NONE => fail "malformed float")
              end))))

    (* ---- datetime (captured verbatim, not interpreted) ----------------- *)

    (* A pragmatic date-time recognizer: starts with 4 digits, a dash, then a
       run of date/time characters. We capture the lexeme as-is. This is tried
       before plain numbers in `scalar`. *)
    and datetime () =
      try (count 4 digit >>= (fn y =>
           char #"-" >>
           many1 (sat (fn c => Char.isDigit c orelse c = #"-" orelse c = #":"
                               orelse c = #"T" orelse c = #"t" orelse c = #" "
                               orelse c = #"." orelse c = #"+" orelse c = #"Z"
                               orelse c = #"z")) >>= (fn rest =>
             return (Datetime (implode y ^ "-" ^ implode rest)))))

    (* ---- values -------------------------------------------------------- *)

    and boolean () =
      (string "true"  >> return (Bool true))
      <|> (string "false" >> return (Bool false))

    and stringValue () =
      (delay basicStringRaw   >>= (fn s => return (Str s)))
      <|> (delay literalStringRaw >>= (fn s => return (Str s)))

    and arrayValue () =
      (* Arrays may span lines and contain comments/blank lines between
         elements; `skipLines` soaks those up around brackets and commas. *)
      char #"[" >> skipLines >>
      sepEndBy (delay valueExpr <* skipLines) (char #"," >> skipLines) >>= (fn xs =>
      char #"]" >> return (Array xs))

    and inlineTable () =
      char #"{" >> blanks >>
      sepBy (delay inlinePair) (lex (char #",")) >>= (fn ps =>
      blanks >> char #"}" >>
        (* Inline tables are built by the same nesting logic as a document, so
           dotted keys inside `{ a.b = 1 }` produce nested tables. *)
        return (Table (foldPairs ps)))

    and inlinePair () =
      blanks >> (delay dottedKey) >>= (fn path =>
        lex (char #"=") >> (delay valueExpr) >>= (fn v =>
          return (path, v)))

    (* A bare value (no surrounding whitespace handling beyond what each form
       needs). Order matters: datetime before number (both can start with
       digits); strings/bools/containers are unambiguous on their first char. *)
    and valueExpr () =
      blanks >>
      ((delay datetime)
       <|> (delay number)
       <|> (delay boolean)
       <|> (delay stringValue)
       <|> (delay arrayValue)
       <|> (delay inlineTable)
       <?> "value")

    (* ---- directives (one logical line each) ---------------------------- *)

    and stdHeader () =
      try (lex (char #"[") >> (delay dottedKey) >>= (fn path =>
           lex (char #"]") >> return (Std path)))

    and arrHeader () =
      try (lex (string "[[") >> (delay dottedKey) >>= (fn path =>
           lex (string "]]") >> return (ArrTab path)))

    and pairLine () =
      (delay dottedKey) >>= (fn path =>
        lex (char #"=") >> (delay valueExpr) >>= (fn v =>
          (sp >> return (Pair (path, v)))))

    and directive () =
      (delay arrHeader)        (* [[ before [ : longest match wins *)
      <|> (delay stdHeader)
      <|> (delay pairLine)

    val document =
      skipLines >>
      sepEndBy (directive ()) (many1 newline >> skipLines) >>= (fn ds =>
        skipLines >> eof >> return ds)

    (* ---- folding directives into a nested Table ------------------------ *)

    (* The document fold threads a root table plus the "current context" path
       (set by [a.b] / [[a.b]] headers). Standard headers create (once) a
       table at the path; array-of-tables headers append a fresh table to an
       array at the path and make it current. *)

    (* Navigate to / create the table at `path`, returning a zipper-free
       updater. We model the root as `value` and rebuild on the way out. *)
    fun getField (tbl, k) =
      List.find (fn (k', _) => k' = k) tbl

    (* Ensure a Std table exists at `path` within `root`, erroring on
       duplicate explicit definition. Returns the new root. We track which
       paths were explicitly created to detect duplicate [a] [a]. *)
    fun defineStd (root, path) =
      let
        fun go (tbl, [k]) =
              (case getField (tbl, k) of
                   NONE => tbl @ [(k, Table [])]
                 | SOME (_, Table _) => tbl  (* already there (e.g. via dotted) *)
                 | SOME _ => raise Toml ("key is not a table: " ^ k))
          | go (tbl, k :: ks) =
              (case getField (tbl, k) of
                   NONE => tbl @ [(k, Table (go ([], ks)))]
                 | SOME (_, Table t) =>
                     List.map (fn (k', sub) =>
                                 if k' = k then (k', Table (go (t, ks))) else (k', sub)) tbl
                 | SOME (_, Array _) =>
                     (* descend into the LAST element of an array-of-tables *)
                     List.map (fn (k', sub) =>
                                 if k' = k then (k', descendArray (sub, ks)) else (k', sub)) tbl
                 | SOME _ => raise Toml ("key is not a table: " ^ k))
          | go (_, []) = raise Toml "empty header"
      in
        case root of
            Table t => Table (go (t, path))
          | _ => raise Toml "root is not a table"
      end

    and descendArray (Array xs, ks) =
          (case List.rev xs of
               (Table last) :: front =>
                 Array (List.rev ((Table (defineStdRaw (last, ks))) :: front))
             | _ => raise Toml "array of tables: last element not a table")
      | descendArray _ = raise Toml "expected array of tables"

    and defineStdRaw (tbl, [k]) =
          (case getField (tbl, k) of
               NONE => tbl @ [(k, Table [])]
             | SOME (_, Table _) => tbl
             | SOME _ => raise Toml ("key is not a table: " ^ k))
      | defineStdRaw (tbl, k :: ks) =
          (case getField (tbl, k) of
               NONE => tbl @ [(k, Table (defineStdRaw ([], ks)))]
             | SOME (_, Table t) =>
                 List.map (fn (k', sub) =>
                             if k' = k then (k', Table (defineStdRaw (t, ks))) else (k', sub)) tbl
             | SOME (_, Array _) =>
                 List.map (fn (k', sub) =>
                             if k' = k then (k', descendArray (sub, ks)) else (k', sub)) tbl
             | SOME _ => raise Toml ("key is not a table: " ^ k))
      | defineStdRaw (_, []) = raise Toml "empty header"

    (* Append a fresh empty table to the array-of-tables at `path`, creating
       the array if needed. Returns the new root. *)
    fun appendArrTab (root, path) =
      let
        fun go (tbl, [k]) =
              (case getField (tbl, k) of
                   NONE => tbl @ [(k, Array [Table []])]
                 | SOME (_, Array xs) =>
                     List.map (fn (k', sub) =>
                                 if k' = k then (k', Array (xs @ [Table []])) else (k', sub)) tbl
                 | SOME _ => raise Toml ("key is not an array of tables: " ^ k))
          | go (tbl, k :: ks) =
              (case getField (tbl, k) of
                   NONE => tbl @ [(k, Table (go ([], ks)))]
                 | SOME (_, Table t) =>
                     List.map (fn (k', sub) =>
                                 if k' = k then (k', Table (go (t, ks))) else (k', sub)) tbl
                 | SOME (_, Array _) =>
                     (* descend into last element of an outer array-of-tables *)
                     List.map (fn (k', sub) =>
                                 if k' = k then (k', descendArrayAppend (sub, ks)) else (k', sub)) tbl
                 | SOME _ => raise Toml ("key is not a table: " ^ k))
          | go (_, []) = raise Toml "empty header"
      in
        case root of
            Table t => Table (go (t, path))
          | _ => raise Toml "root is not a table"
      end

    and descendArrayAppend (Array xs, ks) =
          (case List.rev xs of
               (Table last) :: front =>
                 Array (List.rev ((appendArrTabRaw (last, ks)) :: front))
             | _ => raise Toml "array of tables: last element not a table")
      | descendArrayAppend _ = raise Toml "expected array of tables"

    and appendArrTabRaw (tbl, [k]) =
          (case getField (tbl, k) of
               NONE => Table (tbl @ [(k, Array [Table []])])
             | SOME (_, Array xs) =>
                 Table (List.map (fn (k', sub) =>
                          if k' = k then (k', Array (xs @ [Table []])) else (k', sub)) tbl)
             | SOME _ => raise Toml ("key is not an array of tables: " ^ k))
      | appendArrTabRaw (tbl, k :: ks) =
          (case getField (tbl, k) of
               NONE => Table (tbl @ [(k, appendArrTabRaw ([], ks))])
             | SOME (_, Table t) =>
                 Table (List.map (fn (k', sub) =>
                          if k' = k then (k', appendArrTabRaw (t, ks)) else (k', sub)) tbl)
             | SOME _ => raise Toml ("key is not a table: " ^ k))
      | appendArrTabRaw (_, []) = raise Toml "empty header"

    (* Insert a key/value `Pair` at `ctx ++ path` within `root`. *)
    fun insertAt (root, ctx, path, v) =
      let
        fun atTable (tbl, []) = insertPath (tbl, path, v)
          | atTable (tbl, k :: ks) =
              (case getField (tbl, k) of
                   SOME (_, Table t) =>
                     List.map (fn (k', sub) =>
                                 if k' = k then (k', Table (atTable (t, ks))) else (k', sub)) tbl
                 | SOME (_, Array _) =>
                     List.map (fn (k', sub) =>
                                 if k' = k then (k', insertIntoLastArray (sub, ks)) else (k', sub)) tbl
                 | SOME _ => raise Toml ("key is not a table: " ^ k)
                 | NONE => raise Toml ("no such table: " ^ k))
        and insertIntoLastArray (Array xs, ks) =
              (case List.rev xs of
                   (Table last) :: front =>
                     Array (List.rev (Table (atTable' (last, ks)) :: front))
                 | _ => raise Toml "array of tables: last element not a table")
          | insertIntoLastArray _ = raise Toml "expected array of tables"
        and atTable' (tbl, []) = insertPath (tbl, path, v)
          | atTable' (tbl, k :: ks) =
              (case getField (tbl, k) of
                   SOME (_, Table t) =>
                     List.map (fn (k', sub) =>
                                 if k' = k then (k', Table (atTable' (t, ks))) else (k', sub)) tbl
                 | SOME (_, Array _) =>
                     List.map (fn (k', sub) =>
                                 if k' = k then (k', insertIntoLastArray (sub, ks)) else (k', sub)) tbl
                 | SOME _ => raise Toml ("key is not a table: " ^ k)
                 | NONE => raise Toml ("no such table: " ^ k))
      in
        case root of
            Table t => Table (atTable (t, ctx))
          | _ => raise Toml "root is not a table"
      end

    fun build ds =
      let
        fun step (Std path, (root, _)) = (defineStd (root, path), path)
          | step (ArrTab path, (root, _)) = (appendArrTab (root, path), path)
          | step (Pair (path, v), (root, ctx)) =
              (insertAt (root, ctx, path, v), ctx)
        val (root, _) = List.foldl step (Table [], []) ds
      in
        root
      end
  in
    fun parse input =
      (case runParser document input of
           CharParsec.Err e => resErr (errorToString e)
         | CharParsec.Ok ds => resOk (build ds))
      handle Toml msg => resErr msg
  end

  (* ---- serialization (additive; the inverse of `parse`) ------------------

     A `value` is rendered back to TOML text. The root table's non-table pairs
     are emitted first as `key = value` lines (keys sorted ascending), then
     each nested table as a `[dotted.header]` section (also sorted), sections
     separated by a single blank line. Arrays — including arrays whose elements
     are tables — are rendered inline (`[1, 2]`, `[{ name = "a" }]`); the
     parser reads those back to the same AST, so round-trips hold without
     needing `[[array-of-tables]]` headers. Floats use a forced-decimal
     formatter that always shows a decimal point and a leading "-" (never
     SML's "~"), so both compilers emit byte-identical text. *)
  local
    fun isBare k =
      size k > 0 andalso
      List.all (fn c => Char.isAlphaNum c orelse c = #"_" orelse c = #"-")
               (explode k)

    fun hex4 n =
      let
        fun pad s = if size s >= 4 then s else pad ("0" ^ s)
      in pad (Int.fmt StringCvt.HEX n) end

    fun escChar c =
      case c of
          #"\"" => "\\\""
        | #"\\" => "\\\\"
        | #"\n" => "\\n"
        | #"\t" => "\\t"
        | #"\r" => "\\r"
        | #"\b" => "\\b"
        | #"\f" => "\\f"
        | _ => if Char.ord c < 0x20 then "\\u" ^ hex4 (Char.ord c)
               else String.str c

    fun quoted s = "\"" ^ String.concat (List.map escChar (explode s)) ^ "\""

    fun keyStr k = if isBare k then k else quoted k

    fun pathStr ks = String.concatWith "." (List.map keyStr ks)

    (* Integer text with a leading "-" rather than SML's "~". *)
    fun intStr n =
      let val s = IntInf.toString n
      in if String.isPrefix "~" s then "-" ^ String.extract (s, 1, NONE) else s end

    (* Forced-decimal real: always a '.', '-' not '~', trailing fractional
       zeros trimmed to a single digit; inf/nan handled explicitly. *)
    fun realStr r =
      if not (Real.isFinite r) then
        (if Real.isNan r then "nan" else if r < 0.0 then "-inf" else "inf")
      else
        let
          val s0 = Real.fmt (StringCvt.FIX (SOME 6)) r
          val s1 = if String.isPrefix "~" s0
                   then "-" ^ String.extract (s0, 1, NONE) else s0
          fun dropZeros (#"0" :: rest) = dropZeros rest
            | dropZeros (#"." :: rest) = #"0" :: #"." :: rest
            | dropZeros cs = cs
        in
          if List.exists (fn c => c = #".") (explode s1)
          then implode (List.rev (dropZeros (List.rev (explode s1))))
          else s1
        end

    (* Stable ascending sort by key (small inputs; insertion sort). *)
    fun sortKvs kvs =
      let
        fun ins (kv, []) = [kv]
          | ins ((k, v), (k2, v2) :: rest) =
              if String.compare (k, k2) = GREATER
              then (k2, v2) :: ins ((k, v), rest)
              else (k, v) :: (k2, v2) :: rest
      in List.foldr (fn (kv, acc) => ins (kv, acc)) [] kvs end

    fun isTable (_, Table _) = true
      | isTable _ = false

    (* Inline rendering: scalars, arrays, and tables-inside-arrays. *)
    fun inline (Str s)      = quoted s
      | inline (Int n)      = intStr n
      | inline (Float r)    = realStr r
      | inline (Bool b)     = if b then "true" else "false"
      | inline (Datetime d) = d
      | inline (Array xs)   = "[" ^ String.concatWith ", " (List.map inline xs) ^ "]"
      | inline (Table kvs)  =
          if null kvs then "{}"
          else "{ " ^ String.concatWith ", "
                        (List.map (fn (k, v) => keyStr k ^ " = " ^ inline v)
                                  (sortKvs kvs)) ^ " }"

    (* Lines for the body of the table at `path` (no header for this level):
       sorted scalar/array pairs, then a `[header]`-prefixed block per nested
       table, sections separated by a single blank line. *)
    fun bodyLines (path, kvs) =
      let
        val nonT = sortKvs (List.filter (fn p => not (isTable p)) kvs)
        val tbls = sortKvs (List.filter isTable kvs)
        val scalarSec = List.map (fn (k, v) => keyStr k ^ " = " ^ inline v) nonT
        val tableSecs =
          List.map (fn (k, Table sub) =>
                        ("[" ^ pathStr (path @ [k]) ^ "]") :: bodyLines (path @ [k], sub)
                     | _ => raise Toml "unreachable: non-table in table section")
                   tbls
        val secs = (if null scalarSec then [] else [scalarSec]) @ tableSecs
        fun join [] = []
          | join [s] = s
          | join (s :: rest) = s @ ("" :: join rest)
      in
        join secs
      end
  in
    fun toString (Table kvs) =
          (case bodyLines ([], kvs) of
               [] => ""
             | ls => String.concatWith "\n" ls ^ "\n")
      | toString v = inline v ^ "\n"

    val encode = toString
  end
end
