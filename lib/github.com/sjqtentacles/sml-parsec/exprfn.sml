(* exprfn.sml

   Implementation of EXPR_PARSER over an arbitrary PARSEC instance.

   For each precedence level we build a `level operand` parser from `operand`
   (the parser for everything binding more tightly, i.e. the previous level or
   the atomic term). A level first applies any prefix/postfix unary operators
   to a single operand, then combines such terms with the level's infix
   operators according to their associativity:

     - LeftAssoc  -> chainl1
     - RightAssoc -> chainr1
     - NonAssoc   -> at most one application (a op b, no chaining)

   Levels are processed highest-precedence-first, each wrapping the previous,
   so the tightest-binding operators sit closest to the atoms. *)

functor ExprParserFn (P : PARSEC) :> EXPR_PARSER
  where type 'a parser = 'a P.parser =
struct
  infix 1 >>= >>
  infix 1 <*
  infix 4 <*> <$>
  infixr 1 <|>

  open P

  type 'a parser = 'a P.parser

  datatype assoc = LeftAssoc | RightAssoc | NonAssoc

  datatype 'a operator =
      Infix of ('a * 'a -> 'a) parser * assoc
    | Prefix of ('a -> 'a) parser
    | Postfix of ('a -> 'a) parser

  (* Partition one precedence level's operators by kind/associativity. *)
  fun partition ops =
      let
        fun go ([], l, r, n, pre, post) = (l, r, n, pre, post)
          | go (Infix (p, LeftAssoc) :: rest, l, r, n, pre, post) =
              go (rest, p :: l, r, n, pre, post)
          | go (Infix (p, RightAssoc) :: rest, l, r, n, pre, post) =
              go (rest, l, p :: r, n, pre, post)
          | go (Infix (p, NonAssoc) :: rest, l, r, n, pre, post) =
              go (rest, l, r, p :: n, pre, post)
          | go (Prefix p :: rest, l, r, n, pre, post) =
              go (rest, l, r, n, p :: pre, post)
          | go (Postfix p :: rest, l, r, n, pre, post) =
              go (rest, l, r, n, pre, p :: post)
      in go (ops, [], [], [], [], []) end

  (* zero-or-one prefix op, then operand, then zero-or-one postfix op *)
  fun withUnary (preP, postP) operand =
      let
        val pre = (preP >>= (fn f => return f)) <|> return (fn x => x)
        val post = (postP >>= (fn f => return f)) <|> return (fn x => x)
      in
        pre >>= (fn f =>
          operand >>= (fn x =>
            post >>= (fn g =>
              return (g (f x)))))
      end

  fun buildLevel ops operand =
      let
        val (lefts, rights, nons, pres, posts) = partition ops
        val preP  = choice pres
        val postP = choice posts
        (* a term at this level: an operand wrapped by optional unary ops *)
        val term =
            if null pres andalso null posts
            then operand
            else withUnary (preP, postP) operand
        val leftOp  = choice lefts
        val rightOp = choice rights
        val nonOp   = choice nons
        (* combine terms with infix ops; left/right chaining, non-assoc once *)
        val withLeft  = if null lefts  then term else chainl1 term leftOp
        val combined1 = withLeft
        val withRight = if null rights then combined1 else chainr1 combined1 rightOp
        val combined2 = withRight
        val withNon =
            if null nons then combined2
            else combined2 >>= (fn x =>
                   (nonOp >>= (fn f => combined2 >>= (fn y => return (f (x, y)))))
                   <|> return x)
      in withNon end

  fun buildExpressionParser table term =
      List.foldl (fn (level, operand) => buildLevel level operand) term table
end
