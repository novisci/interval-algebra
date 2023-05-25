# interval-algebra

The `interval-algebra` package implements [Allen's interval
algebra](https://en.wikipedia.org/wiki/Allen%27s_interval_algebra) in
[Haskell](https://www.haskell.org), for a canonical representation of intervals
as a pair of points representing a begin and an end. The main module provides
data types and related classes for the interval-based temporal logic described
in [Allen (1983)](https://doi.org/10.1145/182.358434) and axiomatized in [Allen
and Hayes (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x). A good
primer on Allen's algebra can be [found
here](https://thomasalspaugh.org/pub/fnd/allen.html).

## Design
The module provides an `Interval` type wrapping the most basic type of interval
needed for the relation algebra defined in the papers cited above. `Interval a`
wraps `(a, a)`, giving the interval's `begin` and `end` points.

However, the module provides typeclasses to generalize an `Interval` and the
interval algebra for temporal logic:

1. `Iv` provides an abstract interface for defining the 13 relations of the
   interval algebra. Instances are provided for the canonical `Interval a`,
   when `a` is an instance of `Ord`, as described in Allen 1983. However, 
   the interval algebra can be used for temporal logic on "intervals" that
   are qualitative and not represented as pairs of points in an ordered set, 
   as provided in examples of that paper.
2. `PointedIv` is an interface for types that, in effect, be cast to the 
   canonical `Interval`.
3. `SizedIv` provides a generic interface for creating and
   manipulating `PointedIv` intervals. In particular, when the interval type also 
   is an instance of `Iv`, it specifies class properties to ensure 
   intervals created or altered via its methods are valid for the purpose using the interval 
   algebra. 
1. `Intervallic` provides an interface for data structures which contain an
   `Interval`, allowing the relation algebra to be performed relative to the
   `Interval` within. The `PairedInterval` defined here is the prototypical
   case.

The module defines instances of the classes above for `Interval a`, and only
provides `SizedIv (Interval a)` instances for a few common `a`. See class
documentation for examples of other possible use-cases. It also defines a
variety of ways to construct valid `Interval a` values for supported point
types `a`.

The loose naming convention is: "Bare" names such as `starts` or `contains` are
generalized over `Intervallic` and their `Iv*` class counterparts start with
`iv`, for example `ivStarts` and `ivContains`.

## Axiom tests

The package [includes tests](test/IntervalAlgebraSpec.hs) that the functions of
the `IntervalAlgebraic` typeclass meets the axioms for _intervals_ (not points)
as laid out in [Allen and Hayes
(1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x).

## Comparisons

`interval-algebra` differs from `data-interval` mainly in that it is more
general and has as its starting point the relation algebra from Allen 1983. The
latter package provides an interval type that is tied to the notion of an
interval as a connected convex subset of the integer or real lines,
differentiating for example between closed and open endpoints. It provides
a `Relation` type codifying the 13 temporal relations from Allen 1983.

For use-cases where that structure is meaningful, `data-interval` might be a
more natural choice. `interval-algebra` might be used instead when more
abstract concepts are needed or there is no need for the notion of
connectedness between the starting and ending points.

An important difference is that `data-interval` supports empty
intervals. `interval-algebra` does not, since Allen's interval relations cannot
be defined for such intervals.
