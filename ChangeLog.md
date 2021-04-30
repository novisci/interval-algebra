# Changelog for interval-algebra

## 0.5.0

* In the `IntervalAlgebraic` typeclass, adds `concur` as a synonym for `notDisjoint`; `enclosedBy` as a synonym for `within`; and `enclose` as the converse of `enclosedBy`.
* Generalizes the utilities `combineIntervals`, `gaps`, and `relations` to work with any `Applicative`, `Foldable` `Monoid` (of which `List` is a case).

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
