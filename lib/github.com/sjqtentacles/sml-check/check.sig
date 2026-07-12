(* check.sig

   sml-check: QuickCheck-/Hedgehog-style property-based testing for the
   sjqtentacles Standard ML libraries.

   This library is a deliberate *delta* over `sml-test`'s built-in `Prop`
   substructure rather than a duplicate of it. `Test.Prop` carries shrinking
   as a separate `shrink : 'a -> 'a list` function bolted onto each
   generator, so its `map`/`bind`/`oneof`/`choose` necessarily DISCARD
   shrinking (`shrink = fn _ => []`) and lose the value printer. sml-check
   instead uses INTEGRATED, LAZY ROSE-TREE shrinking (the Hedgehog model):
   every generated value carries its own lazily-unfolded tree of strictly
   smaller candidates, so `map`/`bind`/`filter`/`suchThat` preserve shrinking
   for free and the real counterexample is always rendered.

   On top of integrated shrinking it adds the combinators `Prop` lacks
   (`frequency`/`sized`/`resize`/`suchThat`/`elements`/`vectorOf`/`option`/
   `tuple3`), `forAllShrink` with a user-supplied shrinker, and
   classify/label/collect coverage statistics -- all driven by the shared
   `sml-prng` SplitMix64 so seeds line up with the rest of the ecosystem, and
   with a `toTest` bridge so properties live in ordinary `sml-test` suites.

   Everything is pure and reproducible: a property is a function of an
   explicit `Word64.word` seed, so a given seed always yields the same cases,
   the same minimal counterexample, and byte-identical output on MLton and
   Poly/ML. *)

signature CHECK =
sig
  (* A 64-bit seed for the underlying sml-prng SplitMix64 stream. *)
  type seed = Word64.word

  (* ---- Lazy rose tree of shrink candidates --------------------------- *)

  (* A value paired with a LAZILY computed forest of strictly-smaller
     candidate values. This is the heart of integrated shrinking: every
     generated value carries its own shrink tree, so structure-preserving
     combinators keep shrinking automatically. Children are produced behind
     a thunk so only the branches actually explored during minimisation are
     ever forced. *)
  datatype 'a rose = Rose of 'a * (unit -> 'a rose list)

  val root     : 'a rose -> 'a
  val children : 'a rose -> 'a rose list
  val roseMap  : ('a -> 'b) -> 'a rose -> 'b rose

  (* ---- Generators ---------------------------------------------------- *)

  (* A size-parameterised, seeded generator of 'a values-with-shrink-trees.
     Pure: the same (size, seed) always produces the same tree. *)
  type 'a gen

  (* Run a generator at an explicit size and seed, exposing the integrated
     shrink tree and the next seed. *)
  val generate : 'a gen -> int -> seed -> 'a rose * seed

  (* Draw `count` sample values (roots only) for demos/sanity checks. *)
  val sampleN : int -> seed -> 'a gen -> 'a list

  (* Functor / monad combinators -- all PRESERVE integrated shrinking. *)
  val map  : ('a -> 'b) -> 'a gen -> 'b gen
  val map2 : ('a -> 'b -> 'c) -> 'a gen -> 'b gen -> 'c gen
  val bind : 'a gen -> ('a -> 'b gen) -> 'b gen
  val pure : 'a -> 'a gen

  (* Strip a value's shrink tree (stop shrinking this component). *)
  val noShrink : 'a gen -> 'a gen

  (* Keep only values satisfying the predicate, by bounded resampling; the
     surviving value's shrink tree is pruned to satisfying candidates too. *)
  val filter   : ('a -> bool) -> 'a gen -> 'a gen
  val suchThat : 'a gen -> ('a -> bool) -> 'a gen   (* flip of filter *)

  (* ---- Size control -------------------------------------------------- *)
  val sized   : (int -> 'a gen) -> 'a gen
  val resize  : int -> 'a gen -> 'a gen
  val getSize : int gen

  (* ---- Primitive generators (with integrated shrinking) -------------- *)
  val bool      : bool gen                 (* shrinks true -> false *)
  val int       : int gen                  (* size-bounded, shrinks toward 0 *)
  val choose    : int * int -> int gen     (* uniform [lo,hi], shrinks toward 0/lo *)
  val intRange  : int * int -> int gen     (* alias of choose *)
  val real      : real gen                 (* in [0,1), shrinks toward 0.0 *)
  val realRange : real * real -> real gen
  val char      : char gen                 (* printable ASCII, shrinks toward 'a' *)
  val charRange : char * char -> char gen
  val string    : string gen
  val stringOf  : char gen -> string gen

  (* ---- Combinators --------------------------------------------------- *)
  val elements       : 'a list -> 'a gen        (* uniform pick, shrinks toward first *)
  val oneof          : 'a gen list -> 'a gen
  val frequency      : (int * 'a gen) list -> 'a gen
  val option         : 'a gen -> 'a option gen  (* shrinks SOME x -> NONE *)
  val listOf         : 'a gen -> 'a list gen    (* length 0..size *)
  val listOfLen      : int -> 'a gen -> 'a list gen
  val vectorOf       : int -> 'a gen -> 'a list gen  (* alias of listOfLen *)
  val nonEmptyListOf : 'a gen -> 'a list gen
  val tuple2         : 'a gen * 'b gen -> ('a * 'b) gen
  val tuple3         : 'a gen * 'b gen * 'c gen -> ('a * 'b * 'c) gen

  (* A user-supplied shrinker: smaller candidates for a value. *)
  type 'a shrink = 'a -> 'a list

  (* ---- Properties ---------------------------------------------------- *)

  (* The testable outcome of one case, carrying classification labels. *)
  type prop
  val tt       : prop              (* passes, no labels *)
  val ff       : prop              (* fails, no labels *)
  val bool'    : bool -> prop      (* prop from a boolean *)
  val classify : bool -> string -> prop -> prop   (* tag the case if cond holds *)
  val label    : string -> prop -> prop           (* always tag the case *)
  val collect  : string -> prop -> prop           (* tally a rendered value *)

  type property

  (* forAll gen show pred: holds iff `pred` holds for every generated value.
     On failure the value is shrunk via its INTEGRATED tree to a minimal
     counterexample, rendered with `show`. *)
  val forAll : 'a gen -> ('a -> string) -> ('a -> bool) -> property

  (* As forAll, but the body returns a classified `prop` for coverage stats. *)
  val forAllP : 'a gen -> ('a -> string) -> ('a -> prop) -> property

  (* As forAll, but minimisation uses an EXPLICIT user shrinker instead of
     the generator's integrated tree (QuickCheck-style). *)
  val forAllShrink : 'a gen -> 'a shrink -> ('a -> string) -> ('a -> bool) -> property

  datatype result =
      Passed of { tests : int, labels : (string * int) list }
    | Failed of { counterexample : string
                , seed    : seed
                , shrinks : int
                , tests   : int }

  val defaultSeed     : seed
  val defaultNumTests : int

  (* Run a property over `numTests` cases from `seed`. Pure & reproducible. *)
  val check      : { seed : seed, numTests : int } -> property -> result
  val quickCheck : property -> result   (* check with the defaults *)

  (* A deterministic, multi-line human-readable rendering of a result. *)
  val report : result -> string

  (* ---- Bridge into sml-test suites ----------------------------------- *)

  (* Wrap a property as an ordinary `Test.test`, checked with the defaults;
     on failure raises `Test.TestFailure` with the shrunk counterexample,
     shrink count and seed so the failure reproduces exactly. *)
  val toTest : string -> property -> Test.test
end
