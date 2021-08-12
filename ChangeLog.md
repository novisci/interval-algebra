# Changelog for interval-algebra

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
