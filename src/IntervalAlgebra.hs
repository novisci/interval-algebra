{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}

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

The module is built around five typeclasses designed to separate concerns of 
constructing, relating, and combining @'Interval'@s: 

1. @'Intervallic'@ provides an interface to the data structure of an @'Interval'@, 
   defining how an @'Interval' a@ is constructed.
2. @'IntervalAlgebraic'@ provides an interface to the @'IntervalRelation's@, 
   the workhorse of Allen's temporal logic.
3. @'IntervalCombinable'@ provides an interface to methods of combining two
   @'Interval's@.
4. @'IntervalSizeable'@ and the related @'Moment'@ provides methods for 
   measuring and modifying the size of an interval.
5. @'IntervalFilterable'@ provides methods for filtering 'Witherable.Filterable' 
   collections of intervals.

An advantage of nested typeclass design is that developers can define an 
@'Interval'@ of type @a@ with just the amount of structure that they need.

== Total Ordering of @Interval@s 

The modules makes the (opinionated) choice of a total ordering for @'Intervallic'@ 
@'Interval'@s. Namely, the ordering is based on first ordering the 'begin's 
then the 'end's.

= Development

This module is under development and the API may change in the future.
-}


{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module IntervalAlgebra(

    -- * Classes
      Intervallic(..)
    , IntervalAlgebraic(..)
    , IntervalCombinable(..)
    , Moment(..)
    , IntervalSizeable(..)
    , IntervalFilterable(..)

    -- * Types
    , Interval
    , IntervalRelation(..)
    , ComparativePredicateOf
) where

import Prelude (Eq, Ord, Show, Read, Enum(..), Bounded(..), Ordering (LT)
               , Maybe(..), Either(..), String, Integer, Int, Bool(..), Num
               , Foldable (maximum, minimum, foldMap, foldr)
               , map, otherwise, flip, show, fst, snd, min, max, any, negate, not
               , (++), (==), (&&), (<), (>), (<=), ($), (+), (-), (.))
import Data.Time as DT ( Day, addDays, diffDays, addGregorianYearsClip, calendarYear )
import Data.Semigroup ( Semigroup((<>)) )
import Data.Set(Set, fromList, difference, intersection, union, map, toList)
import Data.Ord( Ord(..), Ordering(..))
import GHC.Base (Applicative(pure))
import Witherable ( Filterable(filter) )

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
data IntervalRelation a =
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
    deriving (Eq, Show, Read)

instance Bounded (IntervalRelation a) where
    minBound = Before
    maxBound = After

instance Enum (IntervalRelation a) where
    fromEnum r = case r of
                    Before       -> 0
                    Meets        -> 1
                    Overlaps     -> 2
                    FinishedBy   -> 3
                    Contains     -> 4
                    Starts       -> 5
                    Equals       -> 6
                    StartedBy    -> 7
                    During       -> 8
                    Finishes     -> 9
                    OverlappedBy -> 10
                    MetBy        -> 11
                    After        -> 12

    toEnum i = case i of
               0  -> Before
               1  -> Meets
               2  -> Overlaps
               3  -> FinishedBy
               4  -> Contains
               5  -> Starts
               6  -> Equals
               7  -> StartedBy
               8  -> During
               9 -> Finishes
               10 -> OverlappedBy
               11 -> MetBy
               12 -> After

instance Ord (IntervalRelation a) where
    compare x y = compare (fromEnum x) (fromEnum y)

-- | The 'Set' of all 'IntervalRelation's.
intervalRelations :: Set (IntervalRelation a)
intervalRelations = fromList (Prelude.map toEnum [0..12] ::[IntervalRelation a])

-- | Find the converse of a single 'IntervalRelation'
converseRelation :: IntervalRelation a -> IntervalRelation a
converseRelation x = toEnum (12 - fromEnum x)

{-
Misc
-}

-- | Defines a predicate of two objects of type @a@.
type ComparativePredicateOf a = (a -> a -> Bool)

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
@'disjoint'@, @'within'@, and @'unionPredicates'@.
-}
class (Eq a, Intervallic a) => IntervalAlgebraic a where

    -- | Compare two intervals to determine their 'IntervalRelation'.
    relate :: Interval a -> Interval a -> IntervalRelation a
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

    -- | Maps an 'IntervalRelation' to its corresponding predicate function.
    predicate' :: IntervalRelation a -> ComparativePredicateOf (Interval a)
    predicate' r = 
        case r of
            Before       -> before
            Meets        -> meets
            Overlaps     -> overlaps
            FinishedBy   -> finishedBy
            Contains     -> contains
            Starts       -> starts
            Equals       -> equals
            StartedBy    -> startedBy
            During       -> during
            Finishes     -> finishes
            OverlappedBy -> overlappedBy
            MetBy        -> metBy
            After        -> after

    -- | Given a set of 'IntervalRelation's return a list of 'predicate' functions 
    --   corresponding to each relation.
    predicates :: Set (IntervalRelation a) -> [ComparativePredicateOf (Interval a)]
    predicates x = Prelude.map predicate' (toList x)

    -- | Forms a predicate function from the union of a set of 'IntervalRelation's.
    predicate :: Set (IntervalRelation a) -> ComparativePredicateOf (Interval a)
    predicate = unionPredicates.predicates

    -- ** Algebraic operations on IntervalRelations

    -- | Shortcut to creating a 'Set IntervalRelation' from a list.
    toSet :: [IntervalRelation a] -> Set (IntervalRelation a)
    toSet = fromList

    -- | Finds the complement of a 'Set IntervalRelation'.
    complement :: Set (IntervalRelation a) -> Set (IntervalRelation a)
    complement = difference intervalRelations

    -- | Find the intersection of two 'Set's of 'IntervalRelation'
    intersection ::  Set (IntervalRelation a) 
                  -> Set (IntervalRelation a)
                  -> Set (IntervalRelation a)
    intersection = Data.Set.intersection

    -- | Find the union of two 'Set's of 'IntervalRelation'
    union ::  Set (IntervalRelation a) 
           -> Set (IntervalRelation a)
           -> Set (IntervalRelation a)
    union = Data.Set.union 

    -- | Find the converse of a 'Set IntervalRelation'. 
    converse ::   Set (IntervalRelation a) 
                  -> Set (IntervalRelation a)
    converse = Data.Set.map converseRelation 

    -- ** Interval algebra predicates

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
    
    -- | Synonyms for 'starts' and 'startedBy'
    precedes, precededBy      :: ComparativePredicateOf (Interval a)
    precedes      = starts
    precededBy    = startedBy

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
    -- @'ComparativePredicateOf' 'Interval' a@. For example, 
    -- @unionPredicates [before, meets]@ creates a predicate function determining
    -- if one interval is either before or meets another interval.
    unionPredicates       :: [ComparativePredicateOf (Interval a)] ->
                              ComparativePredicateOf (Interval a)
    unionPredicates fs x y = any (\ f -> f x y) fs

    -- | Operator for composing the union of two predicates
    (<|>) ::  ComparativePredicateOf (Interval a)
        -> ComparativePredicateOf (Interval a)
        -> ComparativePredicateOf (Interval a)
    (<|>) f g = unionPredicates [f, g]    

    disjointRelations :: Set (IntervalRelation a)
    disjointRelations = toSet [Before, After, Meets, MetBy]

    withinRelations :: Set (IntervalRelation a)
    withinRelations = toSet [During, Starts, Finishes, Equals]

    -- | Are x and y disjoint ('before', 'after', 'meets', or 'metBy')?
    disjoint               :: ComparativePredicateOf (Interval a)
    disjoint = predicate disjointRelations

    -- | Are x and y not disjoint; i.e. do they share any support?
    notDisjoint            :: ComparativePredicateOf (Interval a)
    notDisjoint = predicate (complement disjointRelations)

    -- | Is x wholly within y (including endpoints) in any sense 'during', 
    --   'starts', 'finishes', or 'equals'?
    within                    :: ComparativePredicateOf (Interval a)
    within = predicate withinRelations


{- |
The 'Moment' class fixes the smallest duration of an 'Intervallic a'.
-}
class (Intervallic a, Num b, Ord b) => Moment a b| a -> b where
    moment :: b
    moment = 1

{- |
The 'IntervalSizeable' typeclass provides functions to determine the size of
and to resize an 'Interval a'.
-}
class (Intervallic a, Moment a b, Num b, Ord b) => IntervalSizeable a b| a -> b where

    -- | Determine the duration of an 'Interval a'.
    duration :: Interval a -> b
    duration x = diff (end x) (begin x)

    -- | Shifts an @a@. Most often, the @b@ will be the same type as @a@. 
    --   But for example, if @a@ is 'Day' then @b@ could be 'Int'.
    add :: b -> a -> a

    -- | Takes the difference between two @a@ to return a @b@.
    diff :: a -> a -> b

    -- | Resize an 'Interval a' to by expanding to "left" by @l@ and to the 
    --   "right" by @r@. In the case that @l@ or @r@ are less than a 'moment'
    --   the respective endpoints are unchanged. 
    expand :: b -> b -> Interval a -> Interval a
    expand l r p = Interval (add s $ begin p, add e $ end p)
      where s = if l < (moment @a) then 0 else negate l
            e = if r < (moment @a) then 0 else r

    -- | Expands an 'Interval a' to left by i.
    expandl :: b -> Interval a -> Interval a
    expandl i = expand i 0

    -- | Expands an 'Interval a' to right by i.
    expandr :: b -> Interval a -> Interval a
    expandr = expand 0

    -- | Safely creates an 'Interval a' using @x@ as the 'begin' and adding 
    --   @max moment dur@ to @x@ as the 'end'.
    beginerval :: b -> a -> Interval a
    beginerval dur x = Interval (x, add (max (moment @a) dur) x)

    -- | Safely creates an 'Interval a' using @x@ as the 'end' and adding
    --   @negate max moment dur@ to @x@ as the 'begin'.
    enderval :: b -> a -> Interval a
    enderval dur x = Interval (add (negate $ max (moment @a) dur) x, x)

{- |
The @'IntervalCombinable'@ typeclass provides methods for (possibly) combining
two @'Interval's@.
-}
class (IntervalAlgebraic a) => IntervalCombinable a where

    -- | Maybe form a new @'Interval'@ by the union of two @'Interval'@s that 'meets'.
    (.+.) :: Interval a -> Interval a -> Maybe (Interval a)
    (.+.) x y
      | x `meets` y = Just $ Interval (begin x, end y)
      | otherwise   = Nothing

    -- | Creates a new @Interval@ spanning the extent x and y
    extenterval :: Interval a -> Interval a -> Interval a
    extenterval x y = Interval (s, e)
       where s = min (begin x) (begin y)
             e = max (end x) (end y)

    -- | If @x@ is 'before' @y@, then form a new @Just Interval a@ from the 
    --   interval in the "gap" between @x@ and @y@ from the 'end' of @x@ to the
    --   'begin' of @y@. Otherwise, 'Nothing'.
    (><) ::  Interval a -> Interval a -> Maybe (Interval a)
    (><) x y
        | x `before` y = Just $ Interval ( end x, begin y )
        | otherwise    = Nothing

    -- | If @x@ is 'before' @y@, return @f x@ appended to @f y@. Otherwise, 
    --   return 'extenterval' of @x@ and @y@ (wrapped in @f@). This is useful for 
    --   (left) folding over an *ordered* container of @Interval@s and combining 
    --   intervals when @x@ is *not* 'before' @y@.
    (<+>):: (Semigroup (f (Interval a)), Applicative f) =>
            Interval a ->
            Interval a ->
            f (Interval a)
    (<+>) x y
      | x `before` y = pure x <> pure y
      | otherwise    = pure ( extenterval x y )

    -- | Forms a 'Just' new interval from the intersection of two intervals, 
    --   provided the intervals are not disjoint.
    intersect :: Interval a -> Interval a -> Maybe (Interval a)
    intersect x y
       | disjoint x y = Nothing 
       | otherwise    = Just $ Interval (b, e)
           where b = max (begin x) (begin y)
                 e = min (end x) (end y)

{- | 
The @'IntervalFilterable'@ class provides functions for filtering 'Filterable's of 
@'Interval'@s based on @'IntervalAlgebraic'@ relations.
-}
class (Filterable f, IntervalAlgebraic a) => IntervalFilterable f a where

    -- |Creates a function for filtering a 'Witherable.Filterable' of @Interval a@s based on a predicate
    filterMaker :: ComparativePredicateOf (Interval a)
                   -> Interval a
                   -> (f (Interval a) -> f (Interval a))
    filterMaker f p = Witherable.filter (`f` p)

    -- | Filter a 'Witherable.Filterable' of @Interval a@s to those that 'overlaps' the @Interval a@
    --   in the first argument.
    filterOverlaps :: Interval a -> f (Interval a) -> f (Interval a)
    filterOverlaps = filterMaker overlaps

    -- | Filter a 'Witherable.Filterable' of @Interval a@s to those 'overlappedBy' the @Interval a@
    --   in the first argument.
    filterOverlappedBy :: Interval a -> f (Interval a) -> f (Interval a)
    filterOverlappedBy = filterMaker overlappedBy

    -- | Filter a 'Witherable.Filterable' of Interval as to those 'before' the @Interval a@
    --   in the first argument.
    filterBefore :: Interval a -> f (Interval a) -> f (Interval a)
    filterBefore = filterMaker before

    -- | Filter a 'Witherable.Filterable' of Interval as to those 'after' the @Interval a@
    --   in the first argument.
    filterAfter :: Interval a -> f (Interval a) -> f (Interval a)
    filterAfter = filterMaker after

    -- | Filter a 'Witherable.Filterable' of Interval as to those that 'meets' the @Interval a@
    --   in the first argument.
    filterMeets :: Interval a -> f (Interval a) -> f (Interval a)
    filterMeets = filterMaker meets

    -- | Filter a 'Witherable.Filterable' of Interval as to those 'metBy' the @Interval a@
    --   in the first argument.
    filterMetBy :: Interval a -> f (Interval a) -> f (Interval a)
    filterMetBy = filterMaker metBy

    -- | Filter a 'Witherable.Filterable' of Interval as to those 'during' the @Interval a@
    --   in the first argument.
    filterDuring :: Interval a -> f (Interval a) -> f (Interval a)
    filterDuring = filterMaker during

    -- | Filter a 'Witherable.Filterable' of Interval as to those that 'contains'
    --   the @Interval a@ in the first argument.
    filterContains :: Interval a -> f (Interval a) -> f (Interval a)
    filterContains = filterMaker contains

    -- | Filter a 'Witherable.Filterable' of Interval as to those that are 'disjoint'
    --   from the @Interval a@ in the first argument.
    filterDisjoint :: Interval a -> f (Interval a) -> f (Interval a)
    filterDisjoint = filterMaker disjoint

    -- | Filter a 'Witherable.Filterable' of Interval as to those that are 'notDisjoint'
    --   from the @Interval a@ in the first argument.
    filterNotDisjoint :: Interval a -> f (Interval a) -> f (Interval a)
    filterNotDisjoint = filterMaker notDisjoint

    -- | Filter a 'Witherable.Filterable' of Interval as to those that are 'within'
    --   the @Interval a@ in the first argument.
    filterWithin :: Interval a -> f (Interval a) -> f (Interval a)
    filterWithin = filterMaker disjoint
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
instance Moment Int Int
instance IntervalSizeable Int Int where
    add = (+)
    diff = (-)
instance IntervalFilterable [] Int

instance Intervallic Integer
instance IntervalAlgebraic Integer
instance IntervalCombinable Integer
instance Moment Integer Integer
instance IntervalSizeable Integer Integer where
    add = (+)
    diff = (-)
instance IntervalFilterable [] Integer

instance Intervallic DT.Day
instance IntervalAlgebraic DT.Day
instance IntervalCombinable DT.Day
instance Moment DT.Day Integer
instance IntervalSizeable DT.Day Integer where
    add = addDays
    diff = diffDays
instance IntervalFilterable [] DT.Day