{-# LANGUAGE FlexibleInstances, DataKinds, FunctionalDependencies #-}

{-|
Module      : Interval Algebra
Description : Implementation of Allen's interval algebra
Copyright   : (c) NoviSci, Inc 2019
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental

This module specifies the functions and relational operators according to the 
interval-based temporal logic axiomatized in [Allen and Hayes (1987)](http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.620.5144&rep=rep1&type=pdf). 
-}

module IntervalAlgebra(
    -- * Classes
       Interval(..)
    ,  Expandable(..)
    ,  IntervalAlgebraic(..)
     
    -- * Types
    , Intrvl
    , Period
    , IntervalRelation
    , ComparativePredicateOf
) where



newtype Pnt a    = Pnt a deriving (Eq, Show)
newtype Intrvl a = Intrvl (a, a) deriving (Eq)

{- | 
A 'Period a' is a simply a pair of the same type. To be useful as a @Period@ 
of time, it will also be an instance of 'Periodable'.
-}

data Period a =
     Point a
   | Moment (Intrvl a)
   | TrueInterval (Intrvl a)
   deriving (Eq)

{-
The 'IntervalRelation' type enumerates the thirteen possible ways that two 
'Interval a' objects can relate according to the interval algebra.
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

{-
The 'Interval' typeclass specifies what makes a valid interval.
how a 'Intrvl a' is constructed. It also includes functions for getting 
the 'begin' and 'end' of an 'Intrvl a'.
-}

class (Ord a, Enum a, Show a) => Interval a where 

    -- | Defines what happens when the inputs to forming an interval are invalid.
    invalidInterval :: a -> a -> String
    invalidInterval a b = show a ++ " <= " ++ show b -- TODO: there's a better way

    -- | This functions _always_ creates an @Interval a@.
    validInterval :: a -> a -> Intrvl a
    validInterval a b = Intrvl (min a b, max a b) 

    -- | Create a new @Interval a@ from an @a@.
    interval :: a -> a -> Either String (Intrvl a)
    interval a b 
      | b <= a    = Left  $ invalidInterval a b
      | otherwise = Right $ validInterval a b

    -- | Converts a pairs of @a@ to an @Interval a@.
    toInterval :: (a, a) -> Either String (Intrvl a)
    toInterval = uncurry interval

    -- | Evaluate the 'begin' or 'end' of an 'Interval a' object.
    begin, end :: Intrvl a -> a
    begin (Intrvl x) = fst x
    end   (Intrvl x) = snd x

-- | Imposes a total ordering on 'Intrvl a' based on first ordering the 'begin's
--   then the 'end's.
instance (Interval a) => Ord (Intrvl a) where
    (<=) x y
      | begin x <  begin y = True
      | begin x == begin y = end x <= end y
      | otherwise = False
    (<)  x y 
      | begin x <  begin y = True
      | begin x == begin y = end x < end y
      | otherwise = False

instance (Interval a, Show a) => Show (Intrvl a) where
   show x = "(" ++ show (begin x) ++ ", " ++ show (end x) ++ ")"

{-
The 'Expandable' typeclass specifies how a type that is an instance of 
'Interval a' can be expanded, shifted, or flipped. 
-}

class (Interval a, Num b) => Expandable a b | a -> b where

    -- | Forms an 'Intrvl a' by 'add'ing 'dur' to 'bgn'. WARNING:
    --   This uses 'validInterval', thus do not provide a _negative_ number to 
    --   'dur', else you could end up with an invalid interval.
    interval' :: a -> b -> Intrvl a
    interval' bgn dur = validInterval bgn (add dur bgn) 

    -- | Determine the duration of an 'Interval a'.
    duration :: Intrvl a -> b

    -- | Shifts an @a@. Most often, the @b@ will be the same
    -- type as the @a@. But for example, if @a@ is 'Day' then @b@ would be 'Int'.
    add :: b -> a -> a

    -- | Expands an 'Intrvl a' to the "left" by @l@ and to the "right" by @r@. 
    expand :: b -> b -> Intrvl a -> Intrvl a
    expand l r p = validInterval s e
      where s = min (add (negate l) $ begin p) (begin p)
            e = max (add r $ end p)   (end p)

    -- | Expands an 'Intrvl a' to left by i.
    expandl :: b -> Intrvl a -> Intrvl a
    expandl i = expand i 0

    -- | Expands an 'Intrvl a' to right by i.
    expandr :: b -> Intrvl a -> Intrvl a
    expandr = expand 0

    -- | Shifts an 'Interval a' to the right by i.
    -- TODO: because of the way expand is defined these won't work.
    --shiftr :: b -> Intrvl a -> Intrvl a
    --shiftr i = expand (negate i) i

    -- | Shifts an 'Intrvl a' to the left by i.
    --shiftl :: b -> Intrvl a -> Intrvl a
    --shiftl i = expand i (negate i)


{-
** Interval Algebra relations

The 'IntervalAlgebraic' typeclass specifies the functions and relational 
operators for interval-based temporal logic. The typeclass defines the 
relational operators for intervals, plus other useful utilities such as 
'disjoint'.
-}

class (Interval a) => IntervalAlgebraic a where

    -- | Compare two intervals to determine their relation.
    intervalCompare :: Intrvl a -> Intrvl a -> IntervalRelation
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
    equals                 :: ComparativePredicateOf (Intrvl a)
    equals   x y  = x == y

    -- | Does x meet y? Does y meet x?
    meets, metBy           :: ComparativePredicateOf (Intrvl a)
    meets    x y  = end x == begin y
    metBy         = flip meets

    -- | Is x before y? Is x after y?
    before, after          :: ComparativePredicateOf (Intrvl a)
    before   x y  = end x < begin y
    after         = flip before
    
    -- | Does x overlap y? Is x overlapped by y?
    overlaps, overlappedBy :: ComparativePredicateOf (Intrvl a)
    overlaps x y  = begin x < begin y && end x < end y && end x > begin y 
    overlappedBy  = flip overlaps

    -- | Does x start y? Is x started by y?
    starts, startedBy      :: ComparativePredicateOf (Intrvl a)
    starts   x y  = begin x == begin y && (end x < end y)
    startedBy     = flip starts

    -- | Does x finish y? Is x finished by y?
    finishes, finishedBy   :: ComparativePredicateOf (Intrvl a)
    finishes x y  = begin x > begin y && end x == end y
    finishedBy    = flip finishes

    -- | Is x during y? Does x contain y?
    during, contains       :: ComparativePredicateOf (Intrvl a)
    during   x y  = begin x > begin y && end x < end y
    contains      = flip during

    -- ** Interval Algebra utilities

    -- | Compare interval relations with _or_.
    composeRelations       :: [ComparativePredicateOf (Intrvl a)] ->
                               ComparativePredicateOf (Intrvl a)
    composeRelations fs x y = any (\ f -> f x y) fs

    -- | Are x and y disjoint?
    disjoint               :: ComparativePredicateOf (Intrvl a)
    disjoint = composeRelations [before, after]

    -- | Is x contained in y in any sense?
    in'                    :: ComparativePredicateOf (Intrvl a)
    in' = composeRelations [during, starts, finishes, equals]

    -- | Ordered Union of meeting intervals.
    (.+.) :: Intrvl a -> Intrvl a -> Maybe (Intrvl a)
    (.+.) x y
        | x `meets` y = Just $ validInterval (begin x) (end y)
        | otherwise   = Nothing

-- | Defines a comparator predicate of two objects of type a
type ComparativePredicateOf a = (a -> a -> Bool) 


{-
TODO: Expand to points, moments, and intervals
-}

instance Ord a => Ord (Pnt a) where
    (<=) (Pnt x) (Pnt y) = x <= y

{-
Instances
-}

instance Interval Int

instance Expandable Int Int where
    add = (+)
    duration x = end x - begin x

instance IntervalAlgebraic Int





