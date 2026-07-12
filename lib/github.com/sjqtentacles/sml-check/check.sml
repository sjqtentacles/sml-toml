(* check.sml

   Implementation of sml-check. See check.sig for the contract.

   The design is the Hedgehog model of INTEGRATED, LAZY shrinking: a generator
   is a pure function from a size and a seed to a `rose` -- a value paired with
   a lazily-unfolded forest of strictly-smaller candidates. Because the shrink
   tree travels with the value, the structure-preserving combinators
   (`map`/`bind`/`filter`/`suchThat`/tuples) automatically carry shrinking,
   unlike a separate `shrink : 'a -> 'a list` function that `map`/`bind` would
   have to discard.

   Randomness comes from the shared sml-prng SplitMix64 (vendored), so seeds
   line up with the rest of the ecosystem; all arithmetic is on Word64, so the
   stream -- and hence every generated case and minimal counterexample -- is
   byte-identical on MLton and Poly/ML. *)

structure Check :> CHECK =
struct
  (* The shared, byte-identical RNG. *)
  structure R = Prng.SplitMix64

  type seed = Word64.word

  (* ---- Lazy rose trees ----------------------------------------------- *)

  datatype 'a rose = Rose of 'a * (unit -> 'a rose list)

  fun root (Rose (x, _)) = x
  fun children (Rose (_, cs)) = cs ()

  fun roseMap f (Rose (x, cs)) =
    Rose (f x, fn () => List.map (roseMap f) (cs ()))

  (* Monadic bind on rose trees: shrink the outer value (re-running k on each
     outer shrink) and then offer the inner shrinks. This is what makes
     `bind` preserve integrated shrinking. *)
  fun roseBind (Rose (x, xs)) k =
    let val Rose (y, ys) = k x
    in Rose (y, fn () =>
         List.map (fn t => roseBind t k) (xs ()) @ ys ())
    end

  (* Pair two trees, shrinking the left then the right. *)
  fun roseTuple2 (ra, rb) =
    Rose ((root ra, root rb), fn () =>
      List.map (fn ra' => roseTuple2 (ra', rb)) (children ra)
      @ List.map (fn rb' => roseTuple2 (ra, rb')) (children rb))

  fun roseTuple3 (ra, rb, rc) =
    Rose ((root ra, root rb, root rc), fn () =>
      List.map (fn ra' => roseTuple3 (ra', rb, rc)) (children ra)
      @ List.map (fn rb' => roseTuple3 (ra, rb', rc)) (children rb)
      @ List.map (fn rc' => roseTuple3 (ra, rb, rc')) (children rc))

  (* Keep only the candidates whose root satisfies p (recursively). *)
  fun filterRose p (Rose (x, cs)) =
    Rose (x, fn () =>
      List.mapPartial
        (fn r => if p (root r) then SOME (filterRose p r) else NONE)
        (cs ()))

  (* ---- Generators ---------------------------------------------------- *)

  (* A generator: size -> RNG state -> value-with-shrink-tree. Pure. *)
  type 'a gen = int -> R.state -> 'a rose

  (* Split a state into two independent states (for combining sub-gens). *)
  fun split (st : R.state) : R.state * R.state =
    let val (w, st') = R.next st in (R.seed w, st') end

  fun pure x = (fn _ => fn _ => Rose (x, fn () => []))

  fun map f g = (fn size => fn st => roseMap f (g size st))

  fun bind g k =
    (fn size => fn st =>
       let val (l, r) = split st
           val t = g size l
       in roseBind t (fn a => k a size r) end)

  fun tuple2 (ga, gb) =
    (fn size => fn st =>
       let val (l, r) = split st
       in roseTuple2 (ga size l, gb size r) end)

  fun tuple3 (ga, gb, gc) =
    (fn size => fn st =>
       let val (l, m) = split st
           val (m1, r) = split m
       in roseTuple3 (ga size l, gb size m1, gc size r) end)

  fun map2 f ga gb = map (fn (a, b) => f a b) (tuple2 (ga, gb))

  fun noShrink g = (fn size => fn st => Rose (root (g size st), fn () => []))

  fun filter p g =
    (fn size => fn st =>
       let
         fun loop (k, st) =
           let val t = g size st
           in if p (root t) then filterRose p t
              else if k <= 0 then raise Domain
              else loop (k - 1, #2 (R.next st))
           end
       in loop (1000, st) end)

  fun suchThat g p = filter p g

  fun sized f = (fn size => fn st => f size size st)
  fun resize m g = (fn _ => fn st => g m st)
  val getSize : int gen = (fn size => fn _ => Rose (size, fn () => []))

  (* ---- Integer shrinking toward a target ----------------------------- *)

  (* Candidates between 0 and n that approach 0, biggest jump first; uses
     `quot` (truncates toward zero) so negative n terminate (unlike `div`,
     where ~1 div 2 = ~1 loops forever). *)
  fun shrinkInt n =
    if n = 0 then []
    else
      let
        fun halves cur acc =
          if cur = 0 then List.rev acc
          else halves (Int.quot (cur, 2)) ((n - cur) :: acc)
        val cands = 0 :: halves n []
        fun dedup [] _ = []
          | dedup (x :: xs) seen =
              if x = n orelse List.exists (fn y => y = x) seen
              then dedup xs seen
              else x :: dedup xs (x :: seen)
      in dedup cands [] end

  (* Shrink x toward an arbitrary target by translation. *)
  fun shrinkTowards target x =
    List.map (fn d => target + d) (shrinkInt (x - target))

  (* Build an integer rose tree, shrinking toward `target`, constrained to
     [lo, hi]. *)
  fun intTreeIn (lo, hi, target) x =
    Rose (x, fn () =>
      List.map (intTreeIn (lo, hi, target))
        (List.filter (fn c => c >= lo andalso c <= hi andalso c <> x)
           (shrinkTowards target x)))

  (* ---- Primitive generators ------------------------------------------ *)

  fun choose (lo, hi) =
    if lo > hi then raise Domain
    else
      let val target = if lo <= 0 andalso 0 <= hi then 0 else lo
      in (fn _ => fn st =>
            let val (v, _) = R.intRange (lo, hi) st
            in intTreeIn (lo, hi, target) v end)
      end

  fun intRange (lo, hi) = choose (lo, hi)

  (* size-bounded ints, both signs, shrinking toward 0 *)
  val int : int gen =
    sized (fn n => let val b = Int.max (1, n) in choose (~b, b) end)

  val bool : bool gen =
    (fn _ => fn st =>
       let val (bv, _) = R.bool st
       in Rose (bv, fn () => if bv then [Rose (false, fn () => [])] else []) end)

  val real : real gen =
    (fn _ => fn st =>
       let val (rv, _) = R.real01 st
       in Rose (rv, fn () =>
            if Real.== (rv, 0.0) then [] else [Rose (0.0, fn () => [])])
       end)

  fun realRange (lo, hi) =
    let val target = if lo <= 0.0 andalso 0.0 <= hi then 0.0 else lo
    in (fn _ => fn st =>
          let val (u, _) = R.real01 st
              val v = lo + u * (hi - lo)
          in Rose (v, fn () =>
               if Real.== (v, target) then [] else [Rose (target, fn () => [])])
          end)
    end

  fun charRange (lo, hi) =
    let
      val a = Char.ord lo and b = Char.ord hi
      val target = a
    in
      if a > b then raise Domain
      else (fn _ => fn st =>
              let val (v, _) = R.intRange (a, b) st
              in roseMap Char.chr (intTreeIn (a, b, target) v) end)
    end

  (* printable ASCII, shrinking toward 'a' *)
  val char : char gen =
    let val target = Char.ord #"a"
    in (fn _ => fn st =>
          let val (v, _) = R.intRange (32, 126) st
          in roseMap Char.chr (intTreeIn (32, 126, target) v) end)
    end

  (* ---- List interleaving (integrated list shrinking) ----------------- *)

  fun dropAt (i, xs) = List.take (xs, i) @ List.drop (xs, i + 1)
  fun replaceAt (i, xs, y) = List.take (xs, i) @ (y :: List.drop (xs, i + 1))

  (* Interleave element trees into a tree of lists. Children offer (a) the
     list with each single element removed (shrinks length toward []), then
     (b) the list with each element replaced by one of its own shrinks. *)
  fun interleave (trees : 'a rose list) : 'a list rose =
    let val n = List.length trees in
      Rose (List.map root trees, fn () =>
        let
          val drops = List.tabulate (n, fn i => interleave (dropAt (i, trees)))
          val elemShrinks =
            List.concat (List.tabulate (n, fn i =>
              List.map (fn ci => interleave (replaceAt (i, trees, ci)))
                (children (List.nth (trees, i)))))
        in drops @ elemShrinks end)
    end

  (* Fixed-length interleave: element shrinks only, never changes length. *)
  fun interleaveFixed (trees : 'a rose list) : 'a list rose =
    let val n = List.length trees in
      Rose (List.map root trees, fn () =>
        List.concat (List.tabulate (n, fn i =>
          List.map (fn ci => interleaveFixed (replaceAt (i, trees, ci)))
            (children (List.nth (trees, i))))))
    end

  fun buildTrees (g, len, size, st) =
    let
      fun loop (0, _, acc) = List.rev acc
        | loop (k, s, acc) =
            let val (l, r) = split s
            in loop (k - 1, r, g size l :: acc) end
    in loop (len, st, []) end

  fun listOf g =
    (fn size => fn st =>
       let
         val (len, st1) = R.intRange (0, Int.max (0, size)) st
       in interleave (buildTrees (g, len, size, st1)) end)

  fun listOfLen len g =
    (fn size => fn st =>
       interleaveFixed (buildTrees (g, Int.max (0, len), size, st)))

  fun vectorOf n g = listOfLen n g

  fun nonEmptyListOf g =
    map (fn (x, xs) => x :: xs) (tuple2 (g, listOf g))

  fun stringOf cg = map String.implode (listOf cg)
  val string : string gen = stringOf char

  (* ---- Choice combinators -------------------------------------------- *)

  fun oneof [] = raise Domain
    | oneof gs =
        let val k = List.length gs in
          (fn size => fn st =>
             let val (i, st1) = R.intRange (0, k - 1) st
             in (List.nth (gs, i)) size st1 end)
        end

  fun frequency [] = raise Domain
    | frequency pairs =
        let val total = List.foldl (fn ((w, _), acc) => acc + w) 0 pairs in
          if total <= 0 then raise Domain
          else
            (fn size => fn st =>
               let
                 val (p, st1) = R.intRange (0, total - 1) st
                 fun pick (_, []) = raise Domain
                   | pick (acc, (w, g) :: rest) =
                       if p < acc + w then g else pick (acc + w, rest)
               in (pick (0, pairs)) size st1 end)
        end

  fun elements [] = raise Domain
    | elements xs =
        let val k = List.length xs in
          (fn _ => fn st =>
             let
               val (i, _) = R.intRange (0, k - 1) st
               fun tree idx =
                 Rose (List.nth (xs, idx), fn () =>
                   List.map tree
                     (List.filter (fn j => j >= 0 andalso j < idx)
                        (shrinkTowards 0 idx)))
             in tree i end)
        end

  fun option g =
    (fn size => fn st =>
       let val (isSome, st1) = R.bool st in
         if not isSome then Rose (NONE, fn () => [])
         else
           let
             fun wrap (Rose (x, cs)) =
               Rose (SOME x, fn () =>
                 Rose (NONE, fn () => []) :: List.map wrap (cs ()))
           in wrap (g size st1) end
       end)

  (* ---- Samples / running a generator --------------------------------- *)

  val defaultMaxSize = 100
  val defaultNumTests = 100
  val defaultSeed : seed = 0wx853C49E6748FEA9B

  fun generate g size sd =
    let val st = R.seed sd
        val t = g size st
        val (w, _) = R.next st
    in (t, w) end

  fun sampleN count sd g =
    let
      fun loop (0, _, acc) = List.rev acc
        | loop (k, st, acc) =
            let val t = g defaultMaxSize st
                val (_, st') = R.next st
            in loop (k - 1, st', root t :: acc) end
    in if count <= 0 then [] else loop (count, R.seed sd, []) end

  (* ---- Properties ---------------------------------------------------- *)

  type 'a shrink = 'a -> 'a list

  type prop = { ok : bool, labels : string list }

  val tt : prop = { ok = true, labels = [] }
  val ff : prop = { ok = false, labels = [] }
  fun bool' b = { ok = b, labels = [] }
  fun classify cond lbl (p : prop) =
    if cond then { ok = #ok p, labels = lbl :: #labels p } else p
  fun label lbl (p : prop) = { ok = #ok p, labels = lbl :: #labels p }
  fun collect lbl (p : prop) = { ok = #ok p, labels = lbl :: #labels p }

  type caseResult =
    { failed : bool, counterexample : string, shrinks : int, labels : string list }

  type property = int -> R.state -> caseResult

  (* Greedy DFS minimisation: descend into the first child that still fails. *)
  fun minimise (badRoot : 'a -> bool) (tree : 'a rose) =
    let
      fun go (r, steps) =
        case List.find (fn c => badRoot (root c)) (children r) of
            NONE => (root r, steps)
          | SOME c => go (c, steps + 1)
    in go (tree, 0) end

  fun forAllP (g : 'a gen) (showf : 'a -> string) (body : 'a -> prop) : property =
    (fn size => fn st =>
       let
         val tree = g size st
         val x = root tree
         val p0 = (body x) handle _ => ff
       in
         if #ok p0 then
           { failed = false, counterexample = "", shrinks = 0, labels = #labels p0 }
         else
           let
             fun bad v = not (#ok ((body v) handle _ => ff))
             val (v, steps) = minimise bad tree
           in
             { failed = true, counterexample = showf v, shrinks = steps, labels = [] }
           end
       end)

  fun forAll g showf pred = forAllP g showf (fn x => bool' (pred x))

  fun forAllShrink (g : 'a gen) (shr : 'a shrink)
                   (showf : 'a -> string) (pred : 'a -> bool) : property =
    (fn size => fn st =>
       let
         val x = root (g size st)
         fun unfold v = Rose (v, fn () => List.map unfold (shr v))
         fun ok v = (pred v) handle _ => false
       in
         if ok x then
           { failed = false, counterexample = "", shrinks = 0, labels = [] }
         else
           let val (v, steps) = minimise (fn w => not (ok w)) (unfold x)
           in { failed = true, counterexample = showf v, shrinks = steps, labels = [] } end
       end)

  datatype result =
      Passed of { tests : int, labels : (string * int) list }
    | Failed of { counterexample : string
                , seed    : seed
                , shrinks : int
                , tests   : int }

  (* Tally label occurrences, sorted by descending count then name (so the
     output is deterministic). *)
  fun tally labels =
    let
      fun bump (l, []) = [(l, 1)]
        | bump (l, (k, c) :: rest) =
            if k = l then (k, c + 1) :: rest else (k, c) :: bump (l, rest)
      val counts = List.foldr bump [] labels
      fun gt ((a, ca), (b, cb)) =
        if ca <> cb then ca < cb else String.compare (a, b) = GREATER
      fun insert (x, []) = [x]
        | insert (x, y :: ys) = if gt (x, y) then y :: insert (x, ys) else x :: y :: ys
      fun sort [] = []
        | sort (x :: xs) = insert (x, sort xs)
    in sort counts end

  fun check { seed, numTests } (property : property) : result =
    let
      fun sizeFor i =
        if numTests <= 1 then defaultMaxSize
        else 1 + ((i - 1) * (defaultMaxSize - 1)) div (numTests - 1)
      fun loop (i, st, labelAcc) =
        if i > numTests then
          Passed { tests = numTests, labels = tally labelAcc }
        else
          let
            val (w, st') = R.next st
            val r = property (sizeFor i) (R.seed w)
          in
            if #failed r then
              Failed { counterexample = #counterexample r
                     , seed = seed, shrinks = #shrinks r, tests = i }
            else loop (i + 1, st', #labels r @ labelAcc)
          end
    in loop (1, R.seed seed, []) end

  fun quickCheck p = check { seed = defaultSeed, numTests = defaultNumTests } p

  fun report (Passed { tests, labels }) =
        let
          fun pct c = Int.toString ((c * 100) div (Int.max (1, tests))) ^ "%"
          val lines =
            List.map (fn (l, c) => "  " ^ pct c ^ " " ^ l ^ "\n") labels
        in
          "+++ OK, passed " ^ Int.toString tests ^ " tests.\n" ^ String.concat lines
        end
    | report (Failed { counterexample, seed, shrinks, tests }) =
        "*** Failed! Falsifiable (after " ^ Int.toString tests ^ " tests"
        ^ (if shrinks > 0 then " and " ^ Int.toString shrinks ^ " shrinks" else "")
        ^ "):\n  counterexample: " ^ counterexample
        ^ "\n  seed: " ^ Word64.toString seed ^ "\n"

  fun toTest name (property : property) =
    Test.test name (fn () =>
      case check { seed = defaultSeed, numTests = defaultNumTests } property of
          Passed _ => ()
        | Failed { counterexample, seed, shrinks, tests } =>
            raise Test.TestFailure
              ("property falsified after " ^ Int.toString tests ^ " tests: "
               ^ counterexample ^ " (" ^ Int.toString shrinks ^ " shrinks, seed "
               ^ Word64.toString seed ^ ")"))
end
