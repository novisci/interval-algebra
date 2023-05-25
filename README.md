# interval-algebra

The `interval-algebra` package implements [Allen's interval algebra](https://www.ics.uci.edu/~alspaugh/cls/shr/allen.html) in [Haskell](https://www.haskell.org). The main module provides data types and related classes for the interval-based temporal logic described in [Allen (1983)](https://doi.org/10.1145/182.358434) and axiomatized in [Allen and Hayes (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x). A good primer on Allen's algebra can be [found here](https://thomasalspaugh.org/pub/fnd/allen.html).

## Design
The module provides an `Interval` type wrapping the most basic type of interval
needed for the relation algebra defined in the papers cited above. `Interval a`
wraps `(a, a)`, giving the interval's `begin` and `end` points.

However, the module provides typeclasses to generalize an `Interval` in two ways:

1. `Intervallic` provides an interface for data structures which contain an
   `Interval`, allowing the relation algebra to be performed relative to the
   `Interval` within. The `PairedInterval` defined here is the prototypical
   case.
2. `IntervalSizeable` provides a generic interface for creating and
   manipulating intervals on which the relations can be defined. The class
   relies on type families `Point` and `Moment` for the types of beginning and
   ending points, and for the unit of increment between those points.

The module defines only `IntervalSizeable (Interval a)` instances for a few
common `a`. However, for certain manipulations it is more natural or efficient
to work with, say, an interval cast as a `Vector` whose `head` and `last`
elements are the beginning and ending points. Once an appropriate
`IntervalSizeable (Vector a)` instance and `Point (Vector a)` type is defined,
the exported relations immediately apply. This library might define and export
such instances in the future.

The naming convention for relation function names is: "Bare" names such as
`starts` or `contains` are generalized over `Intervallic` and their
`IntervalSizeable` counterparts start with `iv`, for example `ivStarts` and
`ivContains`.

## Axiom tests

The package [includes tests](test/IntervalAlgebraSpec.hs) that the functions of the `IntervalAlgebraic` typeclass meets the axioms for _intervals_ (not points) as laid out in [Allen and Hayes (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x).
