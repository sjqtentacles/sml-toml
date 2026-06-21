(* charparsec.sig

   The character-level parser interface: the full generic PARSEC core
   instantiated at `CharStream`, plus character-specific primitives and a
   `runParser` that accepts a plain `string`.

   This is the interface most consumers want. Build grammars from `char`,
   `string`, `digit`, `integer`, ... and the combinators inherited from PARSEC
   (`>>=`, `<|>`, `many`, `chainl1`, `between`, ...). *)

signature CHAR_PARSEC =
sig
  type pos = { line : int, col : int, off : int }
  type error = { pos : pos, expected : string list, msg : string option }
  type 'a parser

  datatype 'a result = Ok of 'a | Err of error

  (* Run a parser over a whole string. Does NOT require consuming all input;
     combine with `eof` for that guarantee. *)
  val runParser : 'a parser -> string -> 'a result
  val errorToString : error -> string

  (* ---- core (inherited from PARSEC) ---------------------------------- *)
  val return : 'a -> 'a parser
  val fail   : string -> 'a parser
  val >>=    : 'a parser * ('a -> 'b parser) -> 'b parser
  val >>     : 'a parser * 'b parser -> 'b parser
  val <*     : 'a parser * 'b parser -> 'a parser
  val <*>    : ('a -> 'b) parser * 'a parser -> 'b parser
  val <$>    : ('a -> 'b) * 'a parser -> 'b parser
  val <|>    : 'a parser * 'a parser -> 'a parser
  val <?>    : 'a parser * string -> 'a parser
  val try    : 'a parser -> 'a parser

  (* ---- named aliases (zero-fixity ergonomics) ---- *)
  val andThen  : 'a parser -> ('a -> 'b parser) -> 'b parser
  val seqRight : 'a parser -> 'b parser -> 'b parser
  val seqLeft  : 'a parser -> 'b parser -> 'a parser
  val ap       : ('a -> 'b) parser -> 'a parser -> 'b parser
  val map      : ('a -> 'b) -> 'a parser -> 'b parser
  val orElse   : 'a parser -> 'a parser -> 'a parser
  val label    : 'a parser -> string -> 'a parser

  val anyItem : char parser
  val sat     : (char -> bool) -> char parser
  val eof     : unit parser

  val many     : 'a parser -> 'a list parser
  val many1    : 'a parser -> 'a list parser
  val optional : 'a parser -> 'a option parser
  val option   : 'a -> 'a parser -> 'a parser
  val choice   : 'a parser list -> 'a parser
  val count    : int -> 'a parser -> 'a list parser
  val manyTill : 'a parser -> 'b parser -> 'a list parser
  val notFollowedBy : 'a parser -> unit parser
  val skipMany : 'a parser -> unit parser
  val skipMany1: 'a parser -> unit parser
  val sepBy    : 'a parser -> 'b parser -> 'a list parser
  val sepBy1   : 'a parser -> 'b parser -> 'a list parser
  val endBy    : 'a parser -> 'b parser -> 'a list parser
  val endBy1   : 'a parser -> 'b parser -> 'a list parser
  val sepEndBy : 'a parser -> 'b parser -> 'a list parser
  val sepEndBy1: 'a parser -> 'b parser -> 'a list parser
  val between  : 'a parser -> 'b parser -> 'c parser -> 'c parser
  val chainl1  : 'a parser -> ('a * 'a -> 'a) parser -> 'a parser
  val chainr1  : 'a parser -> ('a * 'a -> 'a) parser -> 'a parser
  val chainl   : 'a parser -> ('a * 'a -> 'a) parser -> 'a -> 'a parser
  val chainr   : 'a parser -> ('a * 'a -> 'a) parser -> 'a -> 'a parser
  val delay    : (unit -> 'a parser) -> 'a parser

  (* ---- character primitives ------------------------------------------ *)
  val anyChar : char parser
  val char    : char -> char parser
  (* Match an exact string. Atomic: on a partial match it fails WITHOUT
     consuming input, so a surrounding `<|>` can still try alternatives. *)
  val string  : string -> string parser
  val oneOf   : string -> char parser
  val noneOf  : string -> char parser
  val digit   : char parser
  val letter  : char parser
  val spaces  : unit parser

  (* Lexeme helper: parse `p` then skip trailing whitespace. *)
  val lexeme  : 'a parser -> 'a parser
  (* Deprecated alias for `lexeme`, kept for backward compatibility. *)
  val token   : 'a parser -> 'a parser

  (* ---- lexer / token kit --------------------------------------------- *)
  (* Match a literal string then skip trailing whitespace. *)
  val symbol  : string -> string parser
  (* Run `p` between matching brackets (each eats trailing whitespace). *)
  val parens   : 'a parser -> 'a parser
  val brackets : 'a parser -> 'a parser
  val braces   : 'a parser -> 'a parser
  (* `identifier isFirst isRest`: one `isFirst` char then zero or more `isRest`
     chars, returned as a string, skipping trailing whitespace. *)
  val identifier : (char -> bool) -> (char -> bool) -> string parser
  (* A keyword: the exact word, NOT followed by an alphanumeric char, then
     trailing whitespace. Rejects e.g. `lettuce` for keyword `let`. *)
  val keyword : string -> unit parser
  (* Separated lists with the usual punctuation, each skipping whitespace. *)
  val commaSep  : 'a parser -> 'a list parser
  val commaSep1 : 'a parser -> 'a list parser
  val semiSep   : 'a parser -> 'a list parser
  val semiSep1  : 'a parser -> 'a list parser

  (* Parse a (possibly signed) integer. *)
  val integer : int parser

  (* Full-input parse driver: skip leading whitespace, run `p`, require eof.
     Unlike `runParser` (which permits trailing input), this fails if any
     unconsumed input remains. *)
  val parse : 'a parser -> string -> 'a result
end
