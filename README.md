# interval-algebra

The `interval-algebra` package implements [Allen's interval algebra](https://www.ics.uci.edu/~alspaugh/cls/shr/allen.html) in [Haskell](https://www.haskell.org). The main module provides data types and related classes for the interval-based temporal logic described in [Allen (1983)](https://doi.org/10.1145/182.358434) and axiomatized in [Allen and Hayes (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x). A good primer on Allen's algebra can be [found here](https://thomasalspaugh.org/pub/fnd/allen.html).

## Design

The module is built around three typeclasses designed to separate concerns of constructing, relating, and combining types that contain `Interval`s:

1. `Intervallic` provides an interface to the data structures which contain an `Interval`.
2. `IntervalCombinable` provides an interface to methods of combining two `Interval`s.
3. `IntervalSizeable` provides methods for measuring and modifying the size of an interval.

An advantage of nested typeclass design is that developers can define an `Interval` of type `a` with just the amount of structure that they need.

## Axiom tests

The package [includes tests](test/IntervalAlgebraSpec.hs) that the functions of the `IntervalAlgebraic` typeclass meets the axioms for _intervals_ (not points) as laid out in [Allen and Hayes (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x).
