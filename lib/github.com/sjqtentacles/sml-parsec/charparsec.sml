(* charparsec.sml

   The CharParsec structure: ParsecFn instantiated at CharStream, re-exported
   with character-specific primitives and a string-based `runParser`.

   The generic core (`>>=`, `<|>`, `many`, `chainl1`, ...) is inherited verbatim
   from the functor result `P`. The character primitives (`char`, `string`,
   `digit`, `integer`, ...) are defined here in terms of `P.sat`/`P.anyItem`. *)

structure CharParsec :> CHAR_PARSEC
  where type 'a parser = 'a CharParsecCore.parser =
struct
  structure P = CharParsecCore

  infix 1 >>= >>
  infix 1 <*
  infix 4 <*> <$>
  infixr 1 <|>
  infix 0 <?>

  open P

  (* Run over a string by constructing the initial CharStream cursor. The
     `stream` type is exposed concretely by CharStream, so we can build it
     directly. *)
  fun runParser p s =
      P.runParser p { src = s, pos = { line = 1, col = 1, off = 0 } }

  (* ---- character primitives ---- *)

  val anyChar = anyItem

  fun char c = (sat (fn x => x = c)) <?> ("'" ^ String.str c ^ "'")

  fun oneOf set = sat (fn c => CharVector.exists (fn x => x = c) set)
  fun noneOf set = sat (fn c => not (CharVector.exists (fn x => x = c) set))

  val digit = (sat Char.isDigit) <?> "digit"
  val letter = (sat Char.isAlpha) <?> "letter"

  (* Match an exact string, atomically. We consume character by character; if
     any character mismatches after some have matched, `try` restores the
     failure to non-consuming so `<|>` can still recover (the original library's
     "string is atomic" guarantee). The empty string succeeds without
     consuming. *)
  fun string str =
      let
        val n = String.size str
        fun go i =
            if i >= n then return str
            else char (String.sub (str, i)) >> go (i + 1)
      in
        if n = 0 then return str
        else (try (go 0)) <?> ("\"" ^ str ^ "\"")
      end

  val spaces = many (sat Char.isSpace) >>= (fn _ => return ())

  fun lexeme p = p <* spaces
  (* Deprecated alias kept for backward compatibility. *)
  fun token p = lexeme p

  val integer =
      let
        val sign = (char #"~" >> return ~1)
                   <|> (char #"-" >> return ~1)
                   <|> return 1
        val digits = many1 digit
      in
        sign >>= (fn sgn =>
          digits >>= (fn ds =>
            return (sgn * (valOf (Int.fromString (implode ds))))))
      end

  (* ---- lexer / token kit ---- *)

  fun symbol s = lexeme (string s)

  fun parens p = between (lexeme (char #"(")) (lexeme (char #")")) p
  fun brackets p = between (lexeme (char #"[")) (lexeme (char #"]")) p
  fun braces p = between (lexeme (char #"{")) (lexeme (char #"}")) p

  fun identifier isFirst isRest =
      lexeme (sat isFirst >>= (fn c =>
              many (sat isRest) >>= (fn cs =>
                return (implode (c :: cs)))))

  (* A keyword: the literal word, not followed by an alphanumeric, plus
     trailing whitespace. `try` makes the whole thing non-consuming on failure
     so alternatives can recover. *)
  fun keyword kw =
      lexeme (try (string kw >> notFollowedBy (sat Char.isAlphaNum)))

  fun commaSep1 p = sepBy1 p (lexeme (char #","))
  fun commaSep  p = sepBy  p (lexeme (char #","))
  fun semiSep1  p = sepBy1 p (lexeme (char #";"))
  fun semiSep   p = sepBy  p (lexeme (char #";"))

  (* Full-input parse: skip leading whitespace, run p, require end of input. *)
  fun parse p s = runParser (spaces >> (p <* eof)) s
end
