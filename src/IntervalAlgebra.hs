{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

{-|
Module      : Interval Algebra
Description : Implementation of Allen's interval algebra
Copyright   : (c) NoviSci, Inc 2019
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental

This module specifies the functions and relational operators according to the 
interval-based temporal logic axiomatized in [Allen and Hayes (1987)](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.620.5144&rep=rep1&type=pdf). 
Specifically, the module currently implements section I, "An Axiomatization of 
Interval Time." That is, the module does not include a concept of points or 
"zero duration time" as expressed in the paper, though this module could be 
extended to do so. 
-}

module IntervalAlgebra(
    -- * Classes
       Periodable(..)
    ,  IntervalAlgebraic(..)
    ,  Expandable(..)
     
    -- * Types
    , IntervalRelation
    , Period
    , PredicateOf
) where

{- | 
A 'Period a' is a simply a pair of the same type. To be useful as a @Period@ 
of time, it will also be an instance of 'Periodable'.
-}

newtype Period a = Period (a, a) deriving (Eq, Read)

class (Eq a, Ord a) => Periodable a where

    {-
    The 'Periodable' typeclass specifies the functions defining periods of time. 
    That is, instances of this class can represent /periods of time/. Such
    entities have notions like a beginning, an end, and a duration.
    -}

    -- | Create a new @Period a@ from an @a@.
    period :: a -> a -> Period a
    
    -- | Evaluate the 'beginning', 'end', or 'duration' of a Periodable object.
    begin, end, duration :: Period a -> a
    begin (Period x) = fst x
    end   (Period x) = snd x
    
    -- | Converts a pairs of @a@ to a @Period a@.
    toPeriod :: (a, a) -> Period a
    toPeriod = uncurry period


class (Periodable a, Num b) => Expandable a b where

    {-
    The 'Expandable' typeclass specifies how a 'Periodable a' can be expanded. 
    -}

    -- | Shifts an @a@ "to the right". Most often, the @b@ will be the same
    -- type as the @a@. But for example, if @a@ is 'Day' then @b@ might be 'Int'.
    add :: b -> a -> a

    -- | Expands a 'Period a' to the "left" by @l@ and to the "right" by @r@. 
    expand :: b -> b -> Period a -> Period a
    expand l r p = period s e
      where s = min (add (negate l) $ begin p) (begin p)
            e = max (add r $ end p)   (end p)

    -- | Expands a period to left by i.
    expandl :: b -> Period a -> Period a
    expandl i = expand i 0

    -- | Expands a period to right by i.
    expandr :: b -> Period a -> Period a
    expandr = expand 0

{-
The 'IntervalRelation' type enumerates the thirteen possible ways that two 
intervals can relate according to the interval algebra.
-}

data IntervalRelation = 
      Equals
    | Meets
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
    deriving (Show, Read)

class (Periodable a) => IntervalAlgebraic a where

    {-
    ** Interval Algebra relations

    The 'IntervalAlgebraic' typeclass specifies the functions and relational 
    operators for interval-based temporal logic. The typeclass defines the 
    relational operators for intervals, plus other useful utilities such as 
    'disjoint'.
    -}

    -- | Compare two intervals to determine their relation.
    intervalCompare :: Period a -> Period a -> IntervalRelation
    intervalCompare x y
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
    equals                 :: PredicateOf (Period a)
    equals   x y  = x == y

    -- | Does x meet y? Does y meet x?
    meets, metBy           :: PredicateOf (Period a)
    meets    x y  = (x /= y) && begin y == end x
    metBy         = flip meets

    -- | Is x before y? Is x after y?
    before, after          :: PredicateOf (Period a)
    before   x y  = end x < begin y
    after         = flip before
    
    -- | Does x overlap y? Is x overlapped by y?
    overlaps, overlappedBy :: PredicateOf (Period a)
    overlaps x y  = x <= y  && end x < end y && end x > begin y 
    overlappedBy  = flip overlaps

    -- | Does x start y? Is x started by y?
    starts, startedBy      :: PredicateOf (Period a)
    starts   x y  = (x <= y) && begin x == begin y
    startedBy     = flip starts

    -- | Does x finish y? Is x finished by y?
    finishes, finishedBy   :: PredicateOf (Period a)
    finishes x y  = y <= x && end x == end y
    finishedBy    = flip finishes

    -- | Is x during y? Does x contain y?
    during, contains       :: PredicateOf (Period a)
    during   x y  = begin x >= begin y && end x <= end y
    contains      = flip during

    -- ** Interval Algebra utilities

    -- | Are x and y disjoint?
    disjoint               :: PredicateOf (Period a)
    disjoint x y  = before x y || after x y

{- TODO
class (Periodable a) => PeriodComparable a where
    -- | From a pair of periods form a new period from the min of the begins
    --   to the max of the ends.
    extentPeriod :: Period a -> Period a -> Period a

    -- | Returns a `t` of durations from a `t` of periods.
    durations :: (Functor t) => t (Period a) -> t a

    durations = fmap duration
    extentPeriod p1 p2 = period a b 
      where a = min (begin p1) (begin p2)
            b = max (end p1)   (end p2) 
-}

-- | Defines a comparator predicate of two objects of type a
type PredicateOf a = (a -> a -> Bool) 

instance (Periodable a) => Ord (Period a) where
    (<=) x y
      | begin x <  begin y = True
      | begin x == begin y = end x <= end y
      | otherwise = False
    (<)  x y 
      | begin x <  begin y = True
      | begin x == begin y = end x < end y
      | otherwise = False

instance Periodable Int where
    duration x = end x - begin x
    period a b
    -- TODO: handle this in a more Haskelly way
      | b < (a + 1) = error "b < (a + 1)" 
      | otherwise   = Period (a, b)

instance Expandable Int Int where
    add = (+)

instance IntervalAlgebraic Int

instance Show (Period Int) where
   show x = "(" ++ show (begin x) ++ ", " ++ show (end x) ++ ")"




