(* parsecfn.sml

   Implementation of the PARSEC signature as a functor over a STREAM.

   Representation. The parser state is just the input `stream` (the position
   rides inside it, retrieved via `S.pos`). A parser is a function from a stream
   to an outcome that records, in addition to success or failure, whether any
   input was *consumed*. The consumed flag is what gives `<|>` its ordered,
   non-backtracking semantics:

     - p <|> q : if p fails WITHOUT consuming, try q; if p fails having consumed,
       propagate p's failure (commit). `try p` resets p's consumed flag on
       failure so the surrounding `<|>` can still recover.

   Errors carry the furthest position reached (by offset) and the set of
   expected tokens, so messages point at the real problem rather than the start
   of the alternative. Only `anyItem`, `sat`, and `eof` read the stream; every
   other combinator is defined in terms of them and is stream-agnostic. *)

functor ParsecFn (S : STREAM) :> PARSEC
  where type stream = S.stream
  and   type item   = S.item =
struct
  (* Declaring infix INSIDE the structure is required, otherwise `fun p >>= f`
     parses as application rather than an infix definition. *)
  infix 1 >>= >>
  infix 1 <*
  infix 4 <*> <$>
  infixr 1 <|>
  infix 0 <?>

  type stream = S.stream
  type item = S.item

  type pos = { line : int, col : int, off : int }
  type error = { pos : pos, expected : string list, msg : string option }
  datatype 'a result = Ok of 'a | Err of error

  (* The parser state is the stream itself; position is read via S.pos. *)
  type state = S.stream

  (* consumed flag, then either value+state or error *)
  datatype 'a reply = OkR of 'a * state | ErrR of error
  datatype 'a outcome = Consumed of 'a reply | Empty of 'a reply

  type 'a parser = state -> 'a outcome

  fun posOf (s : state) : pos = S.pos s

  fun mkErr (p : pos) (exp : string list) (m : string option) : error =
      { pos = p, expected = exp, msg = m }

  (* Merge two errors, keeping the one that reached further; union expected
     sets when they are at the same position. *)
  fun mergeErr (e1 : error) (e2 : error) : error =
      let val o1 = #off (#pos e1) and o2 = #off (#pos e2)
      in if o1 > o2 then e1
         else if o2 > o1 then e2
         else { pos = #pos e1,
                expected = (#expected e1) @ (#expected e2),
                msg = case #msg e1 of SOME _ => #msg e1 | NONE => #msg e2 }
      end

  fun return x = fn s => Empty (OkR (x, s))

  fun fail m = fn (s : state) =>
      Empty (ErrR (mkErr (posOf s) [] (SOME m)))

  fun p >>= f = fn s =>
      (case p s of
           Empty (OkR (a, s')) => f a s'
         | Empty (ErrR e) => Empty (ErrR e)
         | Consumed (OkR (a, s')) =>
             (* once consumed, stay consumed regardless of f's empty/consumed *)
             (case f a s' of
                  Empty r => Consumed r
                | Consumed r => Consumed r)
         | Consumed (ErrR e) => Consumed (ErrR e))

  fun p >> q = p >>= (fn _ => q)
  fun p <* q = p >>= (fn a => q >>= (fn _ => return a))
  fun pf <*> px = pf >>= (fn f => px >>= (fn x => return (f x)))
  fun f <$> px = px >>= (fn x => return (f x))

  fun p <|> q = fn s =>
      (case p s of
           Empty (ErrR e1) =>
             (case q s of
                  Empty (ErrR e2) => Empty (ErrR (mergeErr e1 e2))
                | Empty (OkR (a, s')) => Empty (OkR (a, s'))
                | other => other)
         | Empty (OkR (a, s')) => Empty (OkR (a, s'))
         | consumed => consumed)

  fun try p = fn s =>
      (case p s of
           Consumed (ErrR e) => Empty (ErrR e)  (* pretend nothing consumed *)
         | other => other)

  fun p <?> name = fn s =>
      (case p s of
           Empty (ErrR e) =>
             Empty (ErrR { pos = #pos e, expected = [name], msg = #msg e })
         | other => other)

  (* ---- named aliases: curried, prefix forms of the operators above ---- *)
  fun andThen p f = p >>= f
  fun seqRight p q = p >> q
  fun seqLeft p q = p <* q
  fun ap pf px = pf <*> px
  fun map f px = f <$> px
  fun orElse p q = p <|> q
  fun label p name = p <?> name

  (* ---- primitives ---- *)

  fun anyItem (s : state) =
      (case S.uncons s of
           SOME (x, s') => Consumed (OkR (x, s'))
         | NONE => Empty (ErrR (mkErr (posOf s) ["any item"] NONE)))

  fun sat pred = fn (s : state) =>
      (case S.uncons s of
           SOME (x, s') =>
             if pred x
             then Consumed (OkR (x, s'))
             else Empty (ErrR (mkErr (posOf s) [] NONE))
         | NONE => Empty (ErrR (mkErr (posOf s) ["more input"] NONE)))

  (* ---- many / repetition (iterative so deep inputs don't overflow) ---- *)

  fun many p = fn s =>
      let
        fun loop (acc, st, consumedAny) =
            (case p st of
                 Empty (OkR _) =>
                   (* a parser that succeeds without consuming would loop
                      forever; treat as done to stay total *)
                   finish (acc, st, consumedAny)
               | Empty (ErrR _) => finish (acc, st, consumedAny)
               | Consumed (OkR (a, st')) => loop (a :: acc, st', true)
               | Consumed (ErrR e) => Consumed (ErrR e))
        and finish (acc, st, consumedAny) =
            let val r = OkR (List.rev acc, st)
            in if consumedAny then Consumed r else Empty r end
      in loop ([], s, false) end

  fun many1 p = p >>= (fn x => many p >>= (fn xs => return (x :: xs)))

  fun optional p =
      (p >>= (fn x => return (SOME x))) <|> return NONE

  fun option default p =
      p <|> return default

  fun choice ps =
      List.foldr (fn (p, acc) => p <|> acc) (fail "no alternative matched") ps

  fun sepBy1 p sep =
      p >>= (fn x =>
        many (sep >> p) >>= (fn xs => return (x :: xs)))

  fun sepBy p sep =
      sepBy1 p sep <|> return []

  (* count: exactly n; iterative to keep the stack flat for large n *)
  fun count n p =
      let
        fun loop (i, acc) =
            if i <= 0 then return (List.rev acc)
            else p >>= (fn x => loop (i - 1, x :: acc))
      in loop (n, []) end

  (* manyTill: accumulate `p` until `endp` matches; iterative loop. We try
     `endp` first at each step; if it fails without consuming we require a `p`. *)
  fun manyTill p endp = fn s =>
      let
        fun step (acc, st) =
            (case (endp st) of
                 Empty (OkR (_, st')) => Empty (OkR (List.rev acc, st'))
               | Consumed (OkR (_, st')) => Consumed (OkR (List.rev acc, st'))
               | Empty (ErrR _) =>
                   (case p st of
                        Empty (OkR (a, st')) => step (a :: acc, st')
                      | Consumed (OkR (a, st')) => stepC (a :: acc, st')
                      | Empty (ErrR e) => Empty (ErrR e)
                      | Consumed (ErrR e) => Consumed (ErrR e))
               | Consumed (ErrR e) => Consumed (ErrR e))
        (* once anything has been consumed, results must stay Consumed *)
        and stepC (acc, st) =
            (case (endp st) of
                 Empty (OkR (_, st')) => Consumed (OkR (List.rev acc, st'))
               | Consumed (OkR (_, st')) => Consumed (OkR (List.rev acc, st'))
               | Empty (ErrR _) =>
                   (case p st of
                        Empty (OkR (a, st')) => stepC (a :: acc, st')
                      | Consumed (OkR (a, st')) => stepC (a :: acc, st')
                      | Empty (ErrR e) => Consumed (ErrR e)
                      | Consumed (ErrR e) => Consumed (ErrR e))
               | Consumed (ErrR e) => Consumed (ErrR e))
      in step ([], s) end

  (* notFollowedBy: succeeds (without consuming) iff p fails; fails (without
     consuming) iff p succeeds. Inspect the outcome directly and always reset
     to Empty so it never consumes input. *)
  fun notFollowedBy p = fn s =>
      (case p s of
           Empty (OkR _) => Empty (ErrR (mkErr (posOf s) ["not followed by"] (SOME "unexpected input")))
         | Consumed (OkR _) => Empty (ErrR (mkErr (posOf s) ["not followed by"] (SOME "unexpected input")))
         | Empty (ErrR _) => Empty (OkR ((), s))
         | Consumed (ErrR _) => Empty (OkR ((), s)))

  fun skipMany p = many p >>= (fn _ => return ())
  fun skipMany1 p = p >>= (fn _ => skipMany p)

  fun endBy1 p sep = many1 (p <* sep)
  fun endBy p sep = many (p <* sep)

  (* sepEndBy: like sepBy but the trailing separator is optional. After each
     `p`, a `sep` may follow; if it does, more items may follow (or not, if the
     sep was trailing). *)
  fun sepEndBy1 p sep =
      p >>= (fn x =>
        (sep >> (sepEndBy p sep >>= (fn xs => return (x :: xs))))
        <|> return [x])
  and sepEndBy p sep =
      sepEndBy1 p sep <|> return []

  fun between openp closep p =
      openp >> (p <* closep)

  fun chainl1 p opp =
      let
        fun rest x =
            (opp >>= (fn f => p >>= (fn y => rest (f (x, y)))))
            <|> return x
      in p >>= (fn x => rest x) end

  (* chainr1: right-recursive by nature; recursion depth bounded by input. *)
  fun chainr1 p opp =
      p >>= (fn x =>
        (opp >>= (fn f => chainr1 p opp >>= (fn y => return (f (x, y)))))
        <|> return x)

  fun chainl p opp default = chainl1 p opp <|> return default
  fun chainr p opp default = chainr1 p opp <|> return default

  fun delay thunk = fn s => (thunk ()) s

  fun eof (s : state) =
      (case S.uncons s of
           NONE => Empty (OkR ((), s))
         | SOME _ => Empty (ErrR (mkErr (posOf s) ["end of input"] NONE)))

  (* ---- driver ---- *)

  fun runParser p s0 =
      (case p s0 of
           Empty (OkR (a, _)) => Ok a
         | Consumed (OkR (a, _)) => Ok a
         | Empty (ErrR e) => Err e
         | Consumed (ErrR e) => Err e)

  fun errorToString (e : error) =
      let
        val { line, col, ... } = #pos e
        val loc = "line " ^ Int.toString line ^ ", column " ^ Int.toString col
        val what =
            case #msg e of
                SOME m => m
              | NONE =>
                  (case #expected e of
                       [] => "unexpected input"
                     | xs => "expected " ^ String.concatWith " or " xs)
      in "parse error at " ^ loc ^ ": " ^ what end
end
