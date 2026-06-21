(* expr.sig

   A precedence-table driven expression-parser builder, generic over any
   PARSEC instance. Give it a table of operators grouped by precedence level
   and a parser for the atomic terms, and it returns a parser for the whole
   expression language.

   The outer list of the table is ordered HIGHEST precedence first (tightest
   binding), matching the Parsec convention: the first group binds more tightly
   than later groups. Within a group, `Infix` operators share a precedence and
   an associativity. *)

signature EXPR_PARSER =
sig
  type 'a parser

  datatype assoc = LeftAssoc | RightAssoc | NonAssoc

  datatype 'a operator =
      Infix of ('a * 'a -> 'a) parser * assoc
    | Prefix of ('a -> 'a) parser
    | Postfix of ('a -> 'a) parser

  (* buildExpressionParser table term : a parser for the expression grammar
     whose atoms are parsed by `term` and whose operators come from `table`
     (highest precedence first). *)
  val buildExpressionParser : 'a operator list list -> 'a parser -> 'a parser
end
