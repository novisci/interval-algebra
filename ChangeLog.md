# Changelog for interval-algebra

## 0.3.0

* Adds `beginerval` and `enderval` function to `IntervalSizeable` class for safely creating `Interval`s given a begin (or end) and a duration.
* Moves `moment` to its own typeclass `Moment`, which is now a constraint on `IntervalSizeable`.
* Removes function exports from the `IntervalAlgebra.Arbitrary` module which where only meant to exported for the testing modules anyway.

## 0.2.0

* Adds `IntervalSizeable` class.
* Moves `IntervalFilterable` class to main module and generalizes the container to any `Filterable` type.
* Adds `IntervalAlgebra.IntervalAlgebraUtilities` module to collect various useful functions.
* Adds `IntervalAlgebra.Arbitrary` module to provide functions for generating arbitrary intervals.
