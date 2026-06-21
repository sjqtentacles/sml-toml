(* tokenstream.sml

   A reusable STREAM instance over a list of arbitrary items (tokens). This is
   the "polymorphic payoff": the same parser-combinator core that runs over
   characters also runs over a token stream produced by a separate lexer.

   `ListStream` is a functor over the item type and a `show` function. The
   position is purely an index: `off`/`col` advance by one per item consumed and
   `line` stays 1 (a token vector has no inherent line structure; a real lexer
   can attach source spans to the tokens themselves if needed). *)

functor ListStream (type t val show : t -> string) :> STREAM
  where type item = t
  and   type stream = { toks : t list, idx : int } =
struct
  type item = t
  type pos = { line : int, col : int, off : int }
  type stream = { toks : t list, idx : int }

  fun fromList ts : stream = { toks = ts, idx = 0 }

  fun uncons ({ toks, idx } : stream) =
      (case toks of
           [] => NONE
         | x :: rest => SOME (x, { toks = rest, idx = idx + 1 }))

  fun pos ({ idx, ... } : stream) =
      { line = 1, col = idx + 1, off = idx }

  fun showItem x = show x

  fun showPos ({ off, ... } : pos) = "token " ^ Int.toString off
end
