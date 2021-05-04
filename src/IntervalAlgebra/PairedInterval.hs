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
    , pairData
    , intervals
    , makePairPredicate
) where

import IntervalAlgebra
    ( Interval
    , Intervallic(..)
    , IntervalAlgebraic(..)
    , ComparativePredicateOf )
import IntervalAlgebra.IntervalUtilities(compareIntervals, filterOverlaps)
import Witherable ( Filterable(filter) )

-- | An @Interval a@ paired with some other data of type @b@.
newtype PairedInterval b a = PairedInterval (Interval a, b)
    deriving (Eq, Show)

instance (Ord a) => Intervallic (PairedInterval b) a where
    getInterval (PairedInterval x)        = fst x
    setInterval (PairedInterval (x, y)) i = PairedInterval (i, y)

-- | Defines A total ordering on 'PairedInterval b a' based on the 'Interval a'
--   part.
instance (Eq a, Eq b, Ord a, Show a) => Ord (PairedInterval b a) where
  (<=) x y = getInterval x <= getInterval y
  (<) x y  = getInterval x <  getInterval y

instance (Eq b) => IntervalAlgebraic (PairedInterval b) Int 

-- | Make a paired interval. 
mkPairedInterval :: b -> Interval a -> PairedInterval b a
mkPairedInterval d i = PairedInterval (i, d)

-- | Gets the data (i.e. non-interval) part of a @PairedInterval@.
pairData :: PairedInterval b a -> b
pairData (PairedInterval (_, y)) = y

-- | Gets the intervals from a list of paired intervals.
intervals :: Ord a => [PairedInterval b a] -> [Interval a]
intervals = map getInterval

-- | Takes a predicate of intervals and a predicate on the data part of a 
--   paired interval to create a single predicate such that both input
--   predicates should hold.
makePairPredicate :: (IntervalAlgebraic (PairedInterval b) a) =>
       ComparativePredicateOf (Interval a)
    -> ComparativePredicateOf b
    -> ComparativePredicateOf (PairedInterval b a)
makePairPredicate intervalPredicate dataPredicate x y =
         compareIntervals intervalPredicate x y &&
         dataPredicate (pairData x) (pairData y)
