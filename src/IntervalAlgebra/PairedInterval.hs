{-|
Module      : Paired interval 
Description : Extends the Interval Algebra to an interval paired with some data.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module IntervalAlgebra.PairedInterval (
      PairedInterval
    , mkPairedInterval
    , getPairData
    , intervals
    , equalPairData
    , toTrivialPair
    , trivialize
    -- , makePairPredicate
) where

import IntervalAlgebra
    ( Interval
    , Intervallic(..)
    , IntervalAlgebraic(..)
    , IntervalCombinable(..)
    , ComparativePredicateOf
    , extenterval )
-- import IntervalAlgebra.IntervalUtilities(compareIntervals, filterOverlaps)
import Witherable ( Filterable(filter) )

-- | An @Interval a@ paired with some other data of type @b@.
newtype PairedInterval b a = PairedInterval (Interval a, b)
    deriving (Eq)

instance (Ord a, Show a) => Intervallic (PairedInterval b) a where
    getInterval (PairedInterval x)        = fst x
    setInterval (PairedInterval (x, y)) i = PairedInterval (i, y)

-- | Defines A total ordering on 'PairedInterval b a' based on the 'Interval a'
--   part.
instance (Eq a, Eq b, Ord a, Show a) => Ord (PairedInterval b a) where
  (<=) x y = getInterval x <= getInterval y
  (<) x y  = getInterval x <  getInterval y

instance (Eq b, Show a, Ord a) => IntervalAlgebraic (PairedInterval b) a 

instance (Show b, Show a, Ord a) => Show (PairedInterval b a) where
    show x = "{" ++ show (getInterval x) ++ ", " ++ show (getPairData x) ++ "}"

instance (Ord a, Show a, Eq b, Monoid b) => 
          IntervalCombinable (PairedInterval b) a where
    (><) x y = fmap (mkPairedInterval mempty) (getInterval x >< getInterval y)

    (<+>) x y
        | x `before` y = pure x <> pure y
        | otherwise    = pure $ mkPairedInterval (getPairData x <> getPairData y)
                                                 (extenterval x y) 


-- | Make a paired interval. 
mkPairedInterval :: b -> Interval a -> PairedInterval b a
mkPairedInterval d i = PairedInterval (i, d)

-- | Gets the data (i.e. non-interval) part of a @PairedInterval@.
getPairData :: PairedInterval b a -> b
getPairData (PairedInterval (_, y)) = y

equalPairData :: (Eq b) => ComparativePredicateOf (PairedInterval b a)
equalPairData x y = getPairData x == getPairData y

-- | Gets the intervals from a list of paired intervals.
intervals :: (Ord a, Show a) => [PairedInterval b a] -> [Interval a]
intervals = map getInterval

{-| 
-}

data Empty = Empty deriving (Eq, Ord, Show)
instance Semigroup Empty where 
    x <> y = Empty
instance Monoid Empty where
    mempty = Empty
    mappend x y = x <> y

toTrivialPair :: Interval a -> PairedInterval Empty a
toTrivialPair = mkPairedInterval Empty

trivialize :: Functor f => f (Interval a) -> f (PairedInterval Empty a)
trivialize = fmap toTrivialPair

-- | Takes a predicate of intervals and a predicate on the data part of a 
--   paired interval to create a single predicate such that both input
--   predicates should hold.
-- makePairPredicate :: (IntervalAlgebraic (PairedInterval b) a) =>
--        ComparativePredicateOf (Interval a)
--     -> ComparativePredicateOf b
--     -> ComparativePredicateOf (PairedInterval b a)
-- makePairPredicate intervalPredicate dataPredicate x y =
--          compareIntervals intervalPredicate x y &&
--          dataPredicate (getPairData x) (getPairData y)
