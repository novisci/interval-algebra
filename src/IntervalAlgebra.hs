{-|
Module      : Interval Algebra
Description : Implementation of Allen's interval algebra
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental

The @IntervalAlgebra@ module provides data types and related classes for the 
interval-based temporal logic described in [Allen (1983)](https://doi.org/10.1145/182.358434)
and axiomatized in [Allen and Hayes (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x). 

A good primer on Allen's algebra can be [found here](https://thomasalspaugh.org/pub/fnd/allen.html).

= Design

The module is built around three typeclasses designed to separate concerns of 
constructing, relating, and combining @'Interval'@s: 

1. @'Intervallic'@ provides an interface to the data structure of an @'Interval'@, 
   defining how an @'Interval' a@ is constructed.
2. @'IntervalAlgebraic'@ provides an interface to the @'IntervalRelation's@, 
   the workhorse of Allen's temporal logic.
3. @'IntervalCombinable'@ provides an interface to methods of combining multiple
   @'Interval's@.

An advantage of nested typeclass design is that developers can define an 
@'Interval'@ of type @a@ with just the amount of structure that they need.

== Total Ordering of @Interval@s 

The modules makes the (opinionated) choice of a total ordering for @'Intervallic'@ 
@'Interval'@s. Namely, the ordering is based on first ordering the 'begin's 
then the 'end's.

= Development

This module is under development and the API may change in the future.
-}

module IntervalAlgebra(

    -- * Classes
      Intervallic(..)
    , IntervalAlgebraic(..)
    , IntervalCombinable(..)
    
    -- * Data Types
    , Interval(..)
    , IntervalRelation
    , ComparativePredicateOf

) where

import Data.Time as DT

{- | An @'Interval' a@ is a pair of @a@s \( (x, y) \text{ where } x < y\). The
@'Intervallic'@ class provides a safe @'parseInterval'@ function that returns a 
@'Left'@ error if \(y < x\) and 'unsafeInterval' as constructor for creating an
interval that may not be valid. 
-}
newtype Interval a = Interval (a, a) deriving (Eq)

{- | 

The 'IntervalRelation' type enumerates the thirteen possible ways that two 
@'Interval' a@ objects can relate according to the interval algebra.

=== Meets, Metby

> x `meets` y
> y `metBy` x

@ 
x: |-----|
y:       |-----| 
@

=== Before, After

> x `before` y
> y `after` x

@ 
x: |-----|  
y:          |-----|
@


=== Overlaps, OverlappedBy

> x `overlaps` y
> y `overlappedBy` x

@ 
x: |-----|
y:     |-----|
@

=== Starts, StartedBy

> x `starts` y
> y `startedBy` x

@ 
x: |---| 
y: |-----|
@

=== Finishes, FinishedBy

> x `finishes` y
> y `finishedBy` x

@ 
x:   |---| 
y: |-----|
@

=== During, Contains

> x `during` y
> y `contains` x

@ 
x:   |-| 
y: |-----|
@

=== Equal

> x `equal` y
> y `equal` x

@ 
x: |-----| 
y: |-----|
@

-}
data IntervalRelation = 
      Meets 
    | MetBy
    | Before
    | After
    | Overlaps
    | OverlappedBy
    | Starts
    | StartedBy
    | Finishes
    | FinishedBy
    | During
    | Contains
    | Equals
    deriving (Show, Read)

{- | 
The @'Intervallic'@ typeclass specifies how an @'Interval' a@s is constructed.
It also includes functions for getting the @'begin'@ and @'end'@ of an @'Interval' a@.
-}
class (Ord a, Show a) => Intervallic a where 

    -- | Safely parse a pair of @a@s to create an @'Interval' a@.
    parseInterval :: a -> a -> Either String (Interval a)
    parseInterval x y
        -- TODO: create more general framework for error handling
        |  y < x    = Left  $ show y ++ "<" ++ show x
        | otherwise = Right $ Interval (x, y)

    {- | Create a new @'Interval' a@. This function is __not__ safe as it does 
       not enforce that \(x < y\). Use with caution. It is meant to be helper 
       function in early prototyping of this package. This function may be 
       deprecated in future releases.
    -}
    unsafeInterval :: a -> a -> Interval a
    unsafeInterval x y = Interval (x, y)

    -- | Access the ends of an @'Interval' a@ .
    begin, end :: Interval a -> a
    begin (Interval x) = fst x --  \( \text{begin}(x, y) = x \)
    end   (Interval x) = snd x --  \( \text{end}(x, y) = y \)

{- |
The @'IntervalAlgebraic'@ typeclass specifies the functions and relational 
operators for interval-based temporal logic. The typeclass defines the 
relational operators for intervals, plus other useful utilities such as 
@'disjoint'@, @'in''@, and @'composeRelations'@.
-}
class (Eq a, Intervallic a) => IntervalAlgebraic a where

    -- | Compare two intervals to determine their 'IntervalRelation'.
    relate :: Interval a -> Interval a -> IntervalRelation
    relate x y
        | x `before` y       = Before
        | x `after`  y       = After
        | x `meets`  y       = Meets
        | x `metBy`  y       = MetBy
        | x `overlaps` y     = Overlaps
        | x `overlappedBy` y = OverlappedBy
        | x `starts` y       = Starts
        | x `startedBy` y    = StartedBy
        | x `finishes` y     = Finishes
        | x `finishedBy` y   = FinishedBy
        | x `during` y       = During
        | x `contains` y     = Contains
        | otherwise          = Equals

    -- | Does x equal y?
    equals                 :: ComparativePredicateOf (Interval a)
    equals   x y  = x == y

    -- | Does x meet y? Is y metBy x?
    meets, metBy           :: ComparativePredicateOf (Interval a)
    meets    x y  = end x == begin y 
    metBy         = flip meets

    -- | Is x before y? Is x after y?
    before, after          :: ComparativePredicateOf (Interval a)
    before   x y  = end x < begin y
    after         = flip before
    
    -- | Does x overlap y? Is x overlapped by y?
    overlaps, overlappedBy :: ComparativePredicateOf (Interval a)
    overlaps x y  = begin x < begin y && end x < end y && end x > begin y 
    overlappedBy  = flip overlaps

    -- | Does x start y? Is x started by y?
    starts, startedBy      :: ComparativePredicateOf (Interval a)
    starts   x y  = begin x == begin y && (end x < end y)
    startedBy     = flip starts

    -- | Does x finish y? Is x finished by y?
    finishes, finishedBy   :: ComparativePredicateOf (Interval a)
    finishes x y  = begin x > begin y && end x == end y
    finishedBy    = flip finishes

    -- | Is x during y? Does x contain y?
    during, contains       :: ComparativePredicateOf (Interval a)
    during   x y  = begin x > begin y && end x < end y
    contains      = flip during

    -- ** Interval Algebra utilities

    -- | Compose a list of interval relations with _or_ to create a new
    -- @'ComparativePredicateOf' 'Interval' a@.For example, 
    -- @composeRelations [before, meets]@ creates a predicate function determining
    -- if one interval is either before or meets another interval.
    composeRelations       :: [ComparativePredicateOf (Interval a)] ->
                               ComparativePredicateOf (Interval a)
    composeRelations fs x y = any (\ f -> f x y) fs

    -- | Are x and y disjoint ('before', 'after', 'meets', or 'metBy')?
    disjoint               :: ComparativePredicateOf (Interval a)
    disjoint = composeRelations [before, after, meets, metBy]

    -- | Is x contained in y in any sense ('during', 'starts', 'finishes' 
    -- or 'equals'?
    in'                    :: ComparativePredicateOf (Interval a)
    in' = composeRelations [during, starts, finishes, equals]


{- |
The @'IntervalCombinable'@ typeclass provides methods combining multiple @'Interval's@.
-}
class (IntervalAlgebraic a) => IntervalCombinable a where

    -- | Maybe form a new @'Interval'@ by the union of two @'Interval'@s that 'meet'.
    (.+.) :: Interval a -> Interval a -> Maybe (Interval a)
    (.+.) x y
      | x `meets` y = Just $ Interval (begin x, end y)
      | otherwise   = Nothing

{-
Instances
-}

-- | Imposes a total ordering on @'Interval' a@ based on first ordering the 
--   'begin's then the 'end's.
instance (Intervallic a) => Ord (Interval a) where
    (<=) x y
      | begin x <  begin y = True
      | begin x == begin y = end x <= end y
      | otherwise = False
    (<)  x y 
      | begin x <  begin y = True
      | begin x == begin y = end x < end y
      | otherwise = False

instance (Intervallic a, Show a) => Show (Interval a) where
   show x = "(" ++ show (begin x) ++ ", " ++ show (end x) ++ ")"

instance Intervallic Int
instance IntervalAlgebraic Int
instance IntervalCombinable Int

instance Intervallic Integer
instance IntervalAlgebraic Integer
instance IntervalCombinable Integer

instance Intervallic DT.Day
instance IntervalAlgebraic DT.Day
instance IntervalCombinable DT.Day

{-
Misc Utilities
-}

-- | Defines a predicate of two objects of type @a@.
type ComparativePredicateOf a = (a -> a -> Bool) 


