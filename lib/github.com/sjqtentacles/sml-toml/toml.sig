(* toml.sig

   A parser for a useful subset of TOML v1.0.0, built on the vendored
   sml-parsec combinator library. The parser turns a document into a tree of
   `value`s rooted at a `Table`.

   Supported TOML features (see README for the precise subset):
     - scalars: integers (optional +/- sign, `_` digit separators), floats,
       booleans, basic strings "..." (with \n \t \r \" \\ \b \f \/ \uXXXX
       escapes), and literal strings '...' (no escapes);
     - bare keys and dotted keys (`a.b.c = 1`);
     - standard table headers `[a.b]` and array-of-tables headers `[[a.b]]`;
     - inline tables `{ x = 1, y = 2 }`;
     - arrays `[1, 2, 3]`, including nested and heterogeneous arrays;
     - `#` line comments, blank lines, and arbitrary inter-token whitespace.

   Deliberately out of scope (captured or rejected, never silently wrong):
     - date-times are NOT interpreted; an offset/local date-time/date/time
       lexeme is captured verbatim as `Datetime` with its raw text;
     - multi-line basic/literal strings (\"\"\"...\"\"\" / '''...''') are not
       supported. *)

signature TOML =
sig
  datatype value =
      Str      of string
    | Int      of int
    | Float    of real
    | Bool     of bool
    | Datetime of string                 (* raw RFC-3339-ish lexeme, uninterpreted *)
    | Array    of value list
    | Table    of (string * value) list

  (* Result of a parse. We define our own two-armed type rather than lean on a
     Basis `result` (not portably present) or the vendored parsec `result`
     (whose error arm is a structured parse error, not our string message). *)
  datatype ('ok, 'err) result = Ok of 'ok | Err of 'err

  (* Parse a whole TOML document. On success the result is the root `Table`
     (a list of top-level key/value pairs in source order). On failure the
     `Err` carries a one-line, human-readable message (parse error location or
     a semantic error such as a duplicate key). *)
  val parse : string -> (value, string) result

  (* Serialize a `value` back to TOML text — the inverse of `parse` over the
     supported value space. The root is expected to be a `Table`; its pairs
     are emitted with keys sorted ascending, so output is stable and
     byte-identical across MLton and Poly/ML.

       - non-table pairs come first as `key = value` lines, then nested tables
         as `[dotted.header]` sections;
       - strings are basic strings with `"`, `\`, and control chars escaped;
       - integers and floats use a leading "-" (never SML's "~") for
         negatives, and floats always carry a decimal point;
       - arrays (including arrays of tables) are rendered inline, e.g.
         `[1, 2]` and `[{ name = "a" }]`.

     `parse (toString v)` yields a value equal to `v` for everything the
     parser accepts. (`inf`/`nan` floats are emitted as `inf`/`-inf`/`nan` for
     completeness but are outside the parser's subset, so they do not
     round-trip.) *)
  val toString : value -> string
  val encode   : value -> string   (* synonym for toString *)
end
