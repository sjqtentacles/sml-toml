(* charexpr.sml

   The expression-parser builder instantiated for character parsing. Because
   CharParsec is sealed with `where type 'a parser = 'a CharParsecCore.parser`,
   parsers built with CharParsec (e.g. `lexeme integer`, `parens p`) can be
   passed directly to `CharExpr.buildExpressionParser`. *)

structure CharExpr = ExprParserFn (CharParsecCore)
