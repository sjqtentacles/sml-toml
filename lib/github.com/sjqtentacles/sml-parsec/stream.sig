(* stream.sig

   The input abstraction for the parser-combinator core. A `stream` is an
   immutable cursor over a sequence of `item`s. The whole parser machinery is
   defined over this signature, so the same combinators parse character streams,
   token streams produced by a separate lexer, and anything else expressible as
   `uncons`.

   Position. Every stream can report its current `pos`. The position type is a
   single CONCRETE record `{ line, col, off }` shared by all streams. `off` is a
   0-based offset into the underlying sequence and gives a total order used to
   pick the "furthest" failure when merging errors; `line`/`col` are for
   human-readable messages. Streams that have no natural notion of line/column
   (e.g. a token vector) set `line = 1` and use `col`/`off` as the index. *)

signature STREAM =
sig
  type stream
  type item

  (* 1-based line and column, plus a 0-based offset into the sequence. *)
  type pos = { line : int, col : int, off : int }

  (* Take the next item and the remaining stream, or NONE at end of input. *)
  val uncons : stream -> (item * stream) option

  (* The current position of the stream's cursor. *)
  val pos : stream -> pos

  (* Render an item for an error's "expected/unexpected" message. *)
  val showItem : item -> string

  (* Render a position for a human-readable error location. *)
  val showPos : pos -> string
end
