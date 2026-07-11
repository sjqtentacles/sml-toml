# sml-toml

[![CI](https://github.com/sjqtentacles/sml-toml/actions/workflows/ci.yml/badge.svg)](https://github.com/sjqtentacles/sml-toml/actions/workflows/ci.yml)

A TOML parser **and serializer** for Standard ML, covering a useful subset of
[TOML v1.0.0](https://toml.io/en/v1.0.0). It turns a configuration document
into a tree of `value`s rooted at a `Table`, and turns that tree back into
deterministic TOML text with `toString` / `encode`.

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

val parse    : string -> (value, string) result
val toString : value -> string
val encode   : value -> string   (* synonym for toString *)
```

`parse` returns `Ok (Table ...)` on success — the root table, with top-level
keys in source order — or `Err msg` with a one-line message on a syntax error
or a semantic error such as a duplicate key.

`toString` (and its synonym `encode`) is the inverse: it renders a `value` back
to TOML text. Output is **deterministic** — keys are sorted ascending and reals
use a forced-decimal formatter — so the same tree always serializes to the same
bytes on both MLton and Poly/ML. For everything the parser accepts,
`parse (toString v)` round-trips back to a value equal to `v`.

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

### Serializing

```sml
open Toml

val doc = Table [("title", Str "TOML"),
                 ("owner", Table [("name", Str "Tom")])]

val out = toString doc
(* out =
   "title = \"TOML\"\n\
   \\n\
   \[owner]\n\
   \name = \"Tom\"\n" *)
```

That is, `toString doc` produces exactly:

```toml
title = "TOML"

[owner]
name = "Tom"
```

Non-table pairs are emitted first as `key = value` lines (keys sorted
ascending), then each nested table as a `[dotted.header]` section. Strings are
basic strings with `"`, `\`, and control characters escaped; integers and
floats print negatives with a leading `-` (never SML's `~`), and floats always
carry a decimal point. Arrays — including arrays of tables — are rendered inline
(`[1, 2]`, `[{ name = "a" }]`), which the parser reads back to the identical
tree. Bare keys are emitted unquoted; keys that are empty or contain anything
outside `[A-Za-z0-9_-]` are quoted.

> `inf`/`nan` floats are serialized as `inf`/`-inf`/`nan` for completeness, but
> they fall outside the parser's subset and so do not round-trip.

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

## Example

`make example` builds and runs [`examples/demo.sml`](examples/demo.sml), which
parses a small TOML document, looks up nested fields and an
array-of-tables, then serializes it back out (output is byte-identical
under MLton and Poly/ML):

```
=== sml-toml demo ===

-- top-level lookups --
  title    = sml-toml demo
  version  = 3
  ratio    = 0.333333


-- nested table lookup: server.tls.cert --
  server.pem

-- array-of-tables: endpoint --
  /health
  /users

-- re-serialized (sorted keys, forced-decimal floats) --
enabled = true
endpoint = [{ methods = ["GET"], path = "/health" }, { methods = ["GET", "POST"], path = "/users" }]
ratio = 0.333333
tags = ["parser", "toml", "sml"]
title = "sml-toml demo"
version = 3

[server]
host = "localhost"
port = 8080

[server.tls]
cert = "server.pem"
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

66 deterministic checks across scalars, comments/blank lines, dotted keys and
`[table]`/`[a.b]` nesting, arrays and `[[array of tables]]`, inline tables,
string escapes and literal strings, duplicate-key rejection, verbatim
date-time capture, a realistic multi-section fixture, and the `toString`
serializer (exact-output vector, round-trips for nested tables / arrays /
floats / escaped strings, serialization idempotence, and forced-decimal float
formatting). Run `make all-tests`.

## License

MIT. See [LICENSE](LICENSE).
