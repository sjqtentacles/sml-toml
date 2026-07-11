(* demo.sml - parse a small TOML document, look up nested fields and an
   array-of-tables, then serialize it back out (sorted keys, forced-decimal
   floats). Deterministic: identical output on every run and both
   compilers. *)

structure T = Toml

val doc = String.concat [
  "title = \"sml-toml demo\"\n",
  "version = 3\n",
  "ratio = 0.333333\n",
  "enabled = true\n",
  "tags = [\"parser\", \"toml\", \"sml\"]\n",
  "\n",
  "[server]\n",
  "host = \"localhost\"\n",
  "port = 8080\n",
  "\n",
  "[server.tls]\n",
  "cert = \"server.pem\"\n",
  "\n",
  "[[endpoint]]\n",
  "path = \"/health\"\n",
  "methods = [\"GET\"]\n",
  "\n",
  "[[endpoint]]\n",
  "path = \"/users\"\n",
  "methods = [\"GET\", \"POST\"]\n"
]

val () = print "=== sml-toml demo ===\n\n"

fun field (T.Table kvs) k = List.find (fn (k', _) => k' = k) kvs
  | field _ _ = NONE
fun get t k = #2 (valOf (field t k))

fun str (T.Str s) = s | str _ = "<not a string>"
fun int (T.Int n) = IntInf.toString n | int _ = "<not an int>"

val root =
  case T.parse doc of
      T.Ok v => v
    | T.Err msg => raise Fail ("parse failed: " ^ msg)

val () = print "-- top-level lookups --\n"
val () = print ("  title    = " ^ str (get root "title") ^ "\n")
val () = print ("  version  = " ^ int (get root "version") ^ "\n")
val () = print ("  ratio    = " ^ T.toString (get root "ratio") ^ "\n")

val () = print "\n-- nested table lookup: server.tls.cert --\n"
val server = get root "server"
val tls = get server "tls"
val () = print ("  " ^ str (get tls "cert") ^ "\n")

val () = print "\n-- array-of-tables: endpoint --\n"
val endpoints = case field root "endpoint" of
                     SOME (_, T.Array xs) => xs
                   | _ => []
val () = List.app (fn e => print ("  " ^ str (get e "path") ^ "\n")) endpoints

val () = print "\n-- re-serialized (sorted keys, forced-decimal floats) --\n"
val () = print (T.toString root)
