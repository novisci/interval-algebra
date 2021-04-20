# interval-algebra

The `interval-algebra` package implements [Allen's interval algebra](https://www.ics.uci.edu/~alspaugh/cls/shr/allen.html) in [Haskell](https://www.haskell.org). The main module provides data types and related classes for the interval-based temporal logic described in [Allen (1983)](https://doi.org/10.1145/182.358434) and axiomatized in [Allen and Hayes (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x).

A good primer on Allen's algebra can be [found here](https://thomasalspaugh.org/pub/fnd/allen.html).

## Design

The module is built around five typeclasses designed to separate concerns of constructing, relating, and combining `Interval`s:

1. `Intervallic` provides an interface to the data structure of an `Interval`, defining how an `Interval a` (simply a pair `(a, a)`) is constructed.
2. `IntervalAlgebraic` provides an interface to the `IntervalRelation`s, the workhorse of Allen's temporal logic.
3. `IntervalCombinable` provides an interface to methods of combining multiple `Interval`s.
4. `IntervalSizeable` provides methods for measuring and modifying the size of an interval.
5. `IntervalFilterable` provides methods for filtering 'Filterable' collections of intervals.

An advantage of nested typeclass design is that developers can define an `Interval` of type `a` with just the amount of structure that they need.

## Total Ordering of `Interval`s

The modules makes the (opinionated) choice of a total ordering for `Intervallic` `Interval`s. Namely, the ordering is based on first ordering the `begin`s then the `end`s.

## Axiom tests

The package [includes tests](test/IntervalAlgebraSpec.hs) that the functions of the `IntervalAlgebraic` typeclass meets the axioms for _intervals_ (not points) as laid out in [Allen and Hayes (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x).

## Development

This module is under development and the API may change in the future.
