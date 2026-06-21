(* charstream.sml

   A STREAM instance over an immutable string, with line/column/offset tracking.
   This is where the character-specific position bookkeeping lives (the `bump`
   logic that used to sit in the monolithic parser).

   `CharStream.stream` pairs the whole source string with the current cursor
   position. `uncons` reads the character at the current offset and returns a new
   stream whose position has advanced over it. `fromString` builds an initial
   stream at line 1, column 1, offset 0. *)

structure CharStream :> STREAM
  where type item = char
  and   type stream = { src : string, pos : { line : int, col : int, off : int } } =
struct
  (* NOTE: `stream` is exposed concretely above so that CharParsec can build an
     initial stream from a string (via the record literal / fromString). *)
  type item = char
  type pos = { line : int, col : int, off : int }
  type stream = { src : string, pos : pos }

  val startPos : pos = { line = 1, col = 1, off = 0 }

  fun fromString s : stream = { src = s, pos = startPos }

  (* advance a position over a single character *)
  fun bump ({ line, col, off } : pos) c =
      if c = #"\n" then { line = line + 1, col = 1, off = off + 1 }
      else { line = line, col = col + 1, off = off + 1 }

  fun uncons ({ src, pos } : stream) =
      let val off = #off pos
      in if off < String.size src
         then let val c = String.sub (src, off)
              in SOME (c, { src = src, pos = bump pos c }) end
         else NONE
      end

  fun pos ({ pos = p, ... } : stream) = p

  fun showItem c = "'" ^ String.str c ^ "'"

  fun showPos ({ line, col, ... } : pos) =
      "line " ^ Int.toString line ^ ", column " ^ Int.toString col
end
