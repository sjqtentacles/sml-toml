# sml-toml

[![CI](https://github.com/sjqtentacles/sml-toml/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-toml/actions/workflows/ci.yml)

A TOML parser for Standard ML, covering a useful subset of
[TOML v1.0.0](https://toml.io/en/v1.0.0). It turns a configuration document
into a tree of `value`s rooted at a `Table`.

Built on [`sml-parsec`](https://github.com/sjqtentacles/sml-parsec) (vendored
under `lib/`), a parser-combinator library. Pure Standard ML over the Basis
library — no native code, no FFI.

Verified on **MLton** and **Poly/ML** with identical, deterministic output.

## The value tree

```sml
datatype value =
    Str      of string
  | Int      of int
  | Float    of real
  | Bool     of bool
  | Datetime of string              (* raw RFC-3339-ish lexeme, uninterpreted *)
  | Array    of value list
  | Table    of (string * value) list

datatype ('ok, 'err) result = Ok of 'ok | Err of 'err

val parse : string -> (value, string) result
```

`parse` returns `Ok (Table ...)` on success — the root table, with top-level
keys in source order — or `Err msg` with a one-line message on a syntax error
or a semantic error such as a duplicate key.

### Example

```sml
val src =
  "[server]\n\
  \host = \"localhost\"\n\
  \ports = [8000, 8001]\n"

val Toml.Ok (Toml.Table root) = Toml.parse src
(* root = [("server",
            Table [("host",  Str "localhost"),
                   ("ports", Array [Int 8000, Int 8001])])] *)
```

## Supported subset

- **Scalars:** integers (optional leading `+`/`-`, `_` digit separators such as
  `1_000`), floats (fraction and/or `e`/`E` exponent), booleans `true`/`false`.
- **Strings:** basic strings `"..."` with the escapes `\n \t \r \" \\ \b \f \/`
  and `\uXXXX` / `\UXXXXXXXX` (decoded to UTF-8); literal strings `'...'` (no
  escapes, verbatim).
- **Keys:** bare keys (`[A-Za-z0-9_-]+`), quoted keys, and dotted keys
  (`a.b.c = 1` builds nested tables).
- **Tables:** standard headers `[table]` and dotted `[a.b]`; array-of-tables
  headers `[[a.b]]` (two `[[x]]` blocks produce an `Array` of two `Table`s).
- **Inline tables:** `{ x = 1, y = 2 }`, including nesting and dotted keys
  inside (`{ a.b = 1 }`).
- **Arrays:** `[1, 2, 3]`, empty `[]`, trailing commas, nested arrays, and
  heterogeneous arrays (`[1, "two", true]` — TOML 1.0 permits mixed types;
  this parser does not reject them). Arrays may span multiple lines with
  comments and blank lines between elements.
- **Comments & whitespace:** `#` line comments, blank lines, and arbitrary
  inter-token spaces/tabs are ignored.
- **Duplicate keys** within the same table (including inside inline tables and
  via dotted keys) are rejected with `Err "duplicate key: ..."`.

## Deliberately omitted

These are recognized-but-not-interpreted or unsupported, never silently wrong:

- **Date-times are not interpreted.** An offset/local date-time, local date,
  or local time is captured *verbatim* as `Datetime` holding the raw lexeme
  (e.g. `Datetime "1979-05-27T07:32:00Z"`). Parsing it into structured fields
  is out of scope.
- **Multi-line strings** (`"""..."""` and `'''...'''`) are not supported.
- **Special float values** `inf`/`nan` and hex/octal/binary integer literals
  (`0xDEAD`, `0o755`, `0b1010`) are not supported.

## Build & test

```sh
make test        # MLton
make test-poly   # Poly/ML (via tools/polybuild)
make all-tests   # both
make clean
```

## Installing with smlpkg

```sh
smlpkg add github.com/sjqtentacles/sml-toml
smlpkg sync
```

Reference `lib/github.com/sjqtentacles/sml-toml/sml-toml.mlb` from your own
`.mlb`, or feed `test/sources.mlb` to `tools/polybuild` (Poly/ML). The
`sml-parsec` dependency is vendored under `lib/` and committed, so builds need
no network.

## Tests

46 deterministic checks across scalars, comments/blank lines, dotted keys and
`[table]`/`[a.b]` nesting, arrays and `[[array of tables]]`, inline tables,
string escapes and literal strings, duplicate-key rejection, verbatim
date-time capture, and a realistic multi-section fixture. Run `make all-tests`.

## License

MIT. See [LICENSE](LICENSE).
