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
    ,  IntervalSizeable(..)
    ,  IntervalAlgebraic(..)
    ,  Orderable(..)
     
    -- * Types
    , Intrvl
    , Period
    , IntervalRelation
    , ComparativePredicateOf
) where

newtype Pnt a     = Pnt a deriving (Eq, Show)
newtype Intrvl a  = Intrvl a deriving (Eq)
newtype Pair a    = Pair (a, a) deriving (Eq, Show)
newtype Ordered a = Ordered a deriving (Eq, Ord, Show)

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
The 'Orderable' typeclass defines how to create an 'Ordered b a' and how to 
handle cases where the inputs are not in the specified order.
-}

class (Ord a, Show a, Eq (b a)) => Orderable b a  where

    -- | The prototype function for parsing a pair of inputs into a 'Left' failure
    --   or a 'Right' and 'Ordered' object. 
    parseOrdered :: (a -> a -> Bool)    -- ^ An ordering operator (<, <=, >=, >)
                    -> (a -> a -> b a)  -- ^ The constructor of type b of type a
                    -> a -- 
                    -> a
                    -> Either String (Ordered (b a))
    parseOrdered op cons x y
        -- TODO: create more general framework for error handling
        | op y x    = Left $ show x ++ " and " ++ show y ++ " are not in order"
        | otherwise = Right $ Ordered $ cons x y

    -- | This function should require that the inputs conform to a 
    --   [strict order](https://en.wikipedia.org/wiki/Partially_ordered_set#Strict_and_non-strict_partial_orders)
    parseOrderedStrict :: a -> a -> Either String (Ordered (b a))

    -- | This function should require that the inputs conform to a 
    --   [total order](https://en.wikipedia.org/wiki/Total_order
    parseOrderedTotal :: a -> a -> Either String (Ordered (b a))

{-
The 'Interval' typeclass specifies how to create valid interval.
how a 'Intrvl b a' is constructed. It also includes functions for getting 
the 'begin' and 'end' of an 'Intrvl b a'.
-}
class (Orderable b a) => Interval b a where 
    -- | Create a new @Intrvl b a@ from an @Ordered b a@.
    interval :: Ordered (b a) -> Intrvl (b a)
    interval (Ordered x) = Intrvl x

    -- | Determine the 'begin' or 'end' of an 'Interval b a' object.
    begin, end :: Intrvl (b a) -> a


-- | Imposes a total ordering on 'Intrvl a' based on first ordering the 'begin's
--   then the 'end's.
instance (Interval b a) => Ord (Intrvl (b a)) where
    (<=) x y
      | begin x <  begin y = True
      | begin x == begin y = end x <= end y
      | otherwise = False
    (<)  x y 
      | begin x <  begin y = True
      | begin x == begin y = end x < end y
      | otherwise = False

instance (Interval b a, Show a) => Show (Intrvl (b a)) where
   show x = "(" ++ show (begin x) ++ ", " ++ show (end x) ++ ")"

{-
The 'IntervalSizeable' typeclass specifies how the "size" of type that is an 
instance of 'Interval a' can be determined and how the object can be expanded. 
-}

class (Interval b a, Num c) => IntervalSizeable b a c | a -> c, a -> b where

    -- | Determine the duration of an 'Interval b a'.
    duration :: Intrvl (b a) -> c

    -- | Shifts an @a@. Most often, the @c@ will be the same
    -- type as the @a@. But for example, if @a@ is 'Day' then @c@ would be 'Int'.
    add :: c -> a -> a

    -- | Expands an 'Intrvl a' to the "left" by @l@ and to the "right" by @r@. 
    -- TODO: One could implement this function in an unsafe way, in the sense 
    -- that it does not enforce (by types) that the inputs to interval creation 
    -- do not get parsed by 'parseOrdered'.
    expand :: c -> c -> Intrvl (b a) -> Intrvl (b a)

    -- | Expands an 'Intrvl a' to left by i.
    expandl :: c -> Intrvl (b a) -> Intrvl (b a)
    expandl i = expand i 0

    -- | Expands an 'Intrvl a' to right by i.
    expandr :: c -> Intrvl (b a) -> Intrvl (b a)
    expandr = expand 0

{-
** Interval Algebra relations

The 'IntervalAlgebraic' typeclass specifies the functions and relational 
operators for interval-based temporal logic. The typeclass defines the 
relational operators for intervals, plus other useful utilities such as 
'disjoint'.
-}

class (Eq a, Interval b a) => IntervalAlgebraic b a where

    -- | Compare two intervals to determine their relation.
    intervalCompare :: Intrvl (b a) -> Intrvl (b a) -> IntervalRelation
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
    equals                 :: ComparativePredicateOf (Intrvl (b a))
    equals   x y  = x == y

    -- | Does x meet y? Does y meet x?
    meets, metBy           :: ComparativePredicateOf (Intrvl (b a))
    meets    x y  = end x == begin y
    metBy         = flip meets

    -- | Is x before y? Is x after y?
    before, after          :: ComparativePredicateOf (Intrvl (b a))
    before   x y  = end x < begin y
    after         = flip before
    
    -- | Does x overlap y? Is x overlapped by y?
    overlaps, overlappedBy :: ComparativePredicateOf (Intrvl (b a))
    overlaps x y  = begin x < begin y && end x < end y && end x > begin y 
    overlappedBy  = flip overlaps

    -- | Does x start y? Is x started by y?
    starts, startedBy      :: ComparativePredicateOf (Intrvl (b a))
    starts   x y  = begin x == begin y && (end x < end y)
    startedBy     = flip starts

    -- | Does x finish y? Is x finished by y?
    finishes, finishedBy   :: ComparativePredicateOf (Intrvl (b a))
    finishes x y  = begin x > begin y && end x == end y
    finishedBy    = flip finishes

    -- | Is x during y? Does x contain y?
    during, contains       :: ComparativePredicateOf (Intrvl (b a))
    during   x y  = begin x > begin y && end x < end y
    contains      = flip during

    -- ** Interval Algebra utilities

    -- | Compare interval relations with _or_.
    composeRelations       :: [ComparativePredicateOf (Intrvl (b a))] ->
                               ComparativePredicateOf (Intrvl (b a))
    composeRelations fs x y = any (\ f -> f x y) fs

    -- | Are x and y disjoint?
    disjoint               :: ComparativePredicateOf (Intrvl (b a))
    disjoint = composeRelations [before, after]

    -- | Is x contained in y in any sense?
    in'                    :: ComparativePredicateOf (Intrvl (b a))
    in' = composeRelations [during, starts, finishes, equals]

    -- | Ordered Union of meeting intervals.
    (.+.) :: Intrvl (b a) -> Intrvl (b a) -> Maybe (Intrvl (b a))

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

instance Orderable Pair Int where 
    parseOrderedStrict = parseOrdered (<)  pair
    parseOrderedTotal  = parseOrdered (<=) pair

instance Interval Pair Int where
    begin (Intrvl (Pair x)) = fst x
    end   (Intrvl (Pair x)) = snd x

instance IntervalSizeable Pair Int Int where
    add = (+)
    duration x = end x - begin x
    expand l r p = Intrvl $ pair s e
      where s = min (add (negate l) $ begin p) (begin p)
            e = max (add r $ end p)   (end p)

instance IntervalAlgebraic Pair Int where
   (.+.) x y
      | x `meets` y = Just $ Intrvl $ pair (begin x) (end y)
      | otherwise   = Nothing

{-
Utilities
-}

-- | A smart constructor for 'Pair a' objects
pair :: a -> a -> Pair a
pair x y = Pair (x, y)


