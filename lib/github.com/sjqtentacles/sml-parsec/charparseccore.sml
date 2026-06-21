(* charparseccore.sml

   The generic parser core instantiated at CharStream, as a standalone named
   structure that matches PARSEC. This exists so the expression-parser builder
   (ExprParserFn) can be instantiated against a PARSEC-typed structure whose
   `'a parser` is the SAME type as CharParsec.parser.

   CharParsec is built by `open CharParsecCore` and sealed with
   `where type 'a parser = 'a CharParsecCore.parser`, so a parser produced by
   CharParsec can be fed directly to CharExpr.buildExpressionParser. *)

structure CharParsecCore = ParsecFn (CharStream)
