# Changelog for interval-algebra

## 2.1

* Refactors the `Intervallic` typeclass.
* Adds a package component `tutorial` used to provide data for a tutorial document.
* Adds a tutorial document: _interval-algebra foundations_.

## 2.0.3

* Adds `labeledIntervalDiagram`, which creates interval diagrams with labels and a `rangeInterval` reference interval.

## 2.0.2

* Adds `rangeInterval`, which creates the smallest inverval containing all intervals in a `Foldable`.

## 2.0.2

* Adds `rangeInterval`, which creates the smallest inverval containing all intervals in a `Foldable`.

## 2.0.1

* Relaxes cabal package bounds; notably:
  * the `time` bounds gets a wider range
  * the `text` bounds include `2.0`

## 2.0.0

* Adds `Abitrary (Interval a)` instance generic over `Ord a, Arbitrary a`.
* Removes the `moment'` function from the `IntervalSizeable` class.
Use type application with `moment` instead, as in `moment @Int`, `moment @Day`, etc.
* Adds the following utility functions:
`lookback`, `lookahead`, `makeGapsWithinPredicate`,
`pairGaps`, `anyGapsWithinAtLeastDuration`, `allGapsWithinLessThanDuration`

## 2.0

* Removes the `moment'` function from the `IntervalSizeable` class.
Use type application with `moment` instead, as in `moment @Int`, `moment @Day`, etc.
* Adds the following utility functions:
`lookback`, `lookahead`, `makeGapsWithinPredicate`,
`pairGaps`, `anyGapsWithinAtLeastDuration`, `allGapsWithinLessThanDuration`

## 1.4.0

* Adds the `safeInterval` function to `Core`,
as a utility for creating intervals from a pair of endpoints.
* Uses
[PVP](https://pvp.haskell.org/)-style
major upper bounds in cabal dependencies as described
[here](https://cabal.readthedocs.io/en/3.4/cabal-package.html#build-information).
* Adds the `IntervalDiagram` module which includes functions and types
for printing diagrams of intervals as text.
These functions are useful for documentation and examples.

## 1.3.0

* Adds `NFData` and `Binary` instances for `Interval` and `PairedInterval`

## 1.2.0

* Derives `Generic` instances for `Interval` and `PairedInterval`.
* Adds an `Arbitrary` instance for `PairedInterval`.

## 1.1.3

* `Arbitrary` instances for `DiffTime`, `NominalDiffTime` and `Day` are now sized, the absence of which had prevented the 'fix' from version 1.1.1 from being effective. `DiffTime` and `NominalDiffTime` generators are also now limited to a maximum `86399` seconds directly.

## 1.1.2

* Adds an internal utility to `IntervalAlgebra.Arbitrary` to generate a `Maybe (i a)` for `Intervallic i a` from a reference interval and set of relations. `Nothing` is returned for cases in which no interval can be generated.

## 1.1.1

* Modifies internals of `IntervalAlgebra.Arbitrary` module to give uniformity over support for `Integer` and `UTCTime` intervals, yielding better interval generators. Also bounds the `UTCTime` `utctDayTime` argument to `86399` rather than `86400` to avoid trivial and rare cases of property testing failures related to leap seconds.

## 1.1.0

* Fixes bug in `parseInterval`. For example, `parseInterval 0 0` parsed to a `Right (Interval (0, 0))`. Oops, the inequality of the should have been `y <= x` not `y < x`. This was fixed and a test added to catch this error.

## 1.0.1

* Adds `beginervalMoment` and `endervalMoment` functions to create intervals of moment duration from a begin or end.

## 1.0.0

* Moves the main `IntervalAlgebra` module to `IntervalAlgebra.Core` and `IntervalAlgebra` now reexports `IntervalAlgebra.Core`, `IntervalAlgebra.IntervalUtilites`, and `IntervalAlgebra.PairedInterval`.
* Creates a new `IntervalAlgebra.Axioms` module containing the `IntervalAxioms` typeclass of property tests of the interval algebra axioms. These were in the testing suite. Including this as a module in case users need add new `Interval` types and want to test the axioms.
* Creates a new `IntervalAlgebra.RelationProperties` module containing a typeclass of property tests of the interval algebra. These were in the testing suite. Including this as a module in case users need add new `Interval` types and want to test the axioms.
* Adds `UTCTime`/`NominalDiffTime` instance for `IntervalSizeable`.
* Adds additional tests to the testing suite.

## 0.10.2

* Adds the `momentize` function for changing the duration of some interval value to a moment.

## 0.10.1

* Replaces unnecessary `IntervalCombinable i0 a` constraint in `gapsWithin` with `Intervallic i0 a`.

## 0.10.0

* Adds `shiftFromBegin` (`shiftFromEnd`) functions (not totally satisfied with these names) which change the reference point of the interval in the second argument by the difference from the `begin` (`end`) of the interval in the first argument.
* Adds a `Functor` instance for `PairedInterval b`s, which maps an `PairedInterval c a` to `PairedInterval c b`. That is, `fmap` acts on the interval type.

## 0.9.0

* Fixes bug in `gapsWithin` introduced in last version.

## 0.8.6

* Adds the `beginervalFromEnd` and `endervalFromBegin` utilities to create an interval of the provided duration from the end (respectively, begin) of another interval.
* Cleans up some of the internals in the `IntervalUtilies` using functions from the `foldl` package rather than homemade versions.

## 0.8.5

* Fixes synonyms so that `before == precedes` and `after == precededBy`, rather than the incorrect `starts == precedes` and `startedBy == precededBy`.

## 0.8.4

* Fixes bug in `formMeetingSequence` wherein sequences of events with >2 nested events were not returning a meeting sequence.

## 0.8.3

* Moves `begin` and `end` out of the `Intervallic` class.
* Avoids incomplete patterns warnings by:
  * deriving `Enum` instance of `IntervalRelation`
  * catching equals case with `otherwise` in `disjoinPairs`
  * catching disjoint case with `otherwise` in `clip`

## 0.8.2

* Removes `Show` constraint from `intervals` function in `PairIntervals`.

## 0.8.1

* Generalizes `gaps`, `gapsWithin`, and `combineIntervals` to take general `Intervallic` inputs but still return `Interval`s.
* Relaxes the `Show a` constraint on `Intervallic` class.
* Removes unnecessary pragmas.

## 0.8.0

* Removes the `IntervalAlgebraic` typeclass. The functions that were in this class are now regular functions exported in the main module.
* Generalizes all interval predicate functions to work on (potentially) two different `Intervallic` containers.
* Cleans up and reorganizes documentation.

## 0.7.1

* Adds `Safe` language extension to all library modules.

## 0.7.0

* Adds a `Bifunctor` instance for `PairedInterval`s.
* Adds the two new functions to `IntervalUtilites`:
  * `foldMeetingSafe`: Folds over a list of Paired Intervals and in the case that the 'getPairData' is equal between two sequential meeting intervals, these two intervals are combined into one. This function is "safe" in the sense that if the input is invalid and contains any sequential pairs of intervals with an `IntervalRelation`, other than `Meets`, then the function returns an empty list.
  * `formMeetingSequence`: Converts an ordered sequence of `PairedInterval b a` that may have any interval relation
('before', 'starts', etc) into a sequence of sequentially meeting `PairedInterval b a`.  That is, a sequence where one the end of one interval meets the beginning of the subsequent interval. The `getPairData` of the input `PairedInterval`s are
combined using the Monoid `<>` function, hence the pair data must be a `Monoid` instance.
* Renames `pairData` accessor function to `getPairData` in PairedInterval module.
* Removes the `unsafeInterval` function.

## 0.6.3

* Extends the `IntervalCombinable` class to operate on general `Interval` containers.
* Removes all usage of `unsafeInterval` from the testing suite in preparation of removing this function.
* Modifies internals of the `combineIntervals` function to use safe (exception-free) functions rather than footguns like `head` and `tail`.

## 0.6.2

* Fixes bug in `equals` which was checking for equality of the interval container, not just the interval.

## 0.6.1

* Removes the deriving `Show` instance for `PairedInterval`s so people can customize their own instances.

## 0.6.0

* Generalizes the `IntervalAlgebraic` class to work on any data structure that *contains* an interval (not just intervals themselves). This is possible by modification to the `Intervallic` class, which now works in part as lens with `getInterval` and `setInterval` functions. This change allows users to define their own type which contains an interval get all the interval algebraic operation on that new type. The utility of this generalization can be seen in the `PairedInterval` module, which defines a parameterized type for interval *paired* with some other data.
* Eliminates the `Moment` class and combined it with the `IntervalSizeable` class. Like the `IntervalAlgebraic` class, the `IntervalSizeable` class no longer depends on the `Interval` type, but its functions like `duration` now work on any `Intervallic i a` type.
* Removes the `expand`, `expandl`, and `expandr` functions from the `IntervalSizeable` class are now just general functions. These function now work to modify the interval within any `Intervallic i a` type.  Similarly `beginerval`, `enderval`, and `extenterval` were removed from the class; however, these functions only *return* the `Interval` type.
* Generalizes the `filter*` functions in the utilities module to operate on potentially different interval algebraic types. For example, in `filterOverlaps x [ys]`, `x` could be an `Interval a` and the `ys` could be a list of `PairedInterval b a`, so you can filter a container of one interval algebraic type with another interval algebraic type.

## 0.5.0

* Adds the `compose` function to `IntervalAlgebraic` typeclass, thus now all the algebraic operations are available: complement, composition, converse, intersection, and union.
* In the `IntervalAlgebraic` typeclass, adds `concur` as a synonym for `notDisjoint`; `enclosedBy` as a synonym for `within`; and `enclose` as the converse of `enclosedBy`.
* Generalizes the utilities `combineIntervals`, `gaps`, `gapsWithin`, and `relations` to work with any `Applicative`, `Foldable` `Monoid` (of which `List` is a case).
* Changes the signature of `gapsWithin` to return `Maybe (f (Interval a))`, so that in the case that there are no gaps `Nothing` is returned.
* Renames the `emptyIf*` function to `nothingIf*`. Like `gapsWithin`, these now return `Maybe (f (Interval a))` so that `Nothing` is returned if the quantified predicated is true.
* Removes the `IntervalFilterable` typeclass and these functions are now available in the utilities module without needing to specify instances for each container type you want to filter.

## 0.4.0

* Adds utilities `emptyIfNone`, `emptyIfAny`, and `emptyIfAll` that apply predicates to a list of inputs. If none, any, or all of the inputs meet the predicate, then the empty list is returned. Otherwise, the input is returned unmodified. These functions are generalized to `Monoid`s, so they work on structures other than lists.
* Adds `gapsWithin` function to `IntervalUtilities` module that applies `gaps` to all intervals in the input list that are non-disjoint from the interval in the first argument.
* Fixed bug in `combineIntervals` where intervals could fail to be combined properly because `foldr` was being used instead of `foldl'`.
* Adds `intersect` function to `IntervalCombinable` class that returns the (maybe) intersection of two intervals.
* Adds `relations` utility function which returns a list of the `IntervalRelations` between each consecutive pair of intervals in the input list.
* Renames `in'` predicate to `within`. Also, renames `filterIn'` to `filterWithin`.
* Adds `predicate` function to `IntervalAlgebraic` class to map an `IntervalRelation` to its corresponding predicate function. Also adds `predicates` to map a set of `IntervalRelation`s to a list of predicate functions.  
* Adds `intersection`, `union`, `converse`, and `complement` methods to `IntervalAlgebraic` for taking the respective operation on `Set IntervalRelation`.
* Instantiates `Bounded`, `Enum`, and `Ord` for `IntervalRelation`, so that, for one, interval relations can be ordered and used in `Data.Set`. Uses the total ordering defined [here](https://thomasalspaugh.org/pub/fnd/allen.html), though in general, interval relations only have a partial order.
* Renames `composeRelations` to the more accurate `unionPredicates`.
* Adds `<|>` as operator for "union"ing `ComparativePredicateOf (Interval a)`, as in `starts <|> overlaps === unionPredicates [starts, overlaps]`.
* Adds a `clip x y` function which clips the interval `y` to the extent of `x`, provided `x` and `y` are not disjoint.

## 0.3.3

* Fixes bug in `expand` function

## 0.3.2

* Fixes bug in `filterNotDisjoint`

## 0.3.1

* Adds the `diff` function to the `IntervalSizeable` to make comparisons of endpoints easier.
* Adds the `notDisjoint` relation to determine if two intervals share any support.
* Adds `filterDisjoint`, `filterNotDisjoint`, and `filterIn'` to the `IntervalFilterable` class.

## 0.3.0

* Adds `beginerval` and `enderval` function to `IntervalSizeable` class for safely creating `Interval`s given a begin (or end) and a duration.
* Moves `moment` to its own typeclass `Moment`, which is now a constraint on `IntervalSizeable`.
* Removes function exports from the `IntervalAlgebra.Arbitrary` module which where only meant to exported for the testing modules anyway.

## 0.2.0

* Adds `IntervalSizeable` class.
* Moves `IntervalFilterable` class to main module and generalizes the container to any `Filterable` type.
* Adds `IntervalAlgebra.IntervalAlgebraUtilities` module to collect various useful functions.
* Adds `IntervalAlgebra.Arbitrary` module to provide functions for generating arbitrary intervals.
