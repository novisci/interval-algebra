{-|
Module      : Paired interval
Description : Extends the Interval Algebra to an interval paired with some data.
Copyright   : (c) NoviSci, Inc 2020-2022
                  TargetRWE, 2023
License     : BSD3
Maintainer  : bsaul@novisci.com 2020-2022, bbrown@targetrwe.com 2023
Stability   : experimental
-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IntervalAlgebra.PairedInterval
  ( PairedInterval
  , Empty(..)
  , makePairedInterval
  , getPairData
  , intervals
  , equalPairData
  , toTrivialPair
  , trivialize
  ) where

import           Control.Applicative  (liftA2)
import           Control.DeepSeq      (NFData)
import           Data.Binary          (Binary)
import           GHC.Generics         (Generic)
import           IntervalAlgebra.Core (ComparativePredicateOf1, Interval,
                                       Intervallic (..), SizedIv (..), before,
                                       extenterval)
import           Test.QuickCheck      (Arbitrary (..))
import           Witherable           (Filterable (filter))

-- | An @Interval a@ paired with some other data of type @b@.
newtype PairedInterval b a
  = PairedInterval (Interval a, b)
  deriving (Eq, Generic)

instance Intervallic (PairedInterval b) where
  getInterval (PairedInterval x) = fst x
  setInterval (PairedInterval (x, y)) i = PairedInterval (i, y)

instance (NFData a, NFData b) => NFData (PairedInterval b a)
instance (Binary a, Binary b) => Binary (PairedInterval b a)

-- | Defines A total ordering on 'PairedInterval b a' based on the 'Interval a'
--   part.
instance (Eq a, Eq b, Ord a) => Ord (PairedInterval b a) where
  (<=) x y = getInterval x <= getInterval y
  (<) x y = getInterval x < getInterval y

instance (Show b, Show a, Ord a) => Show (PairedInterval b a) where
  show x = "{" ++ show (getInterval x) ++ ", " ++ show (getPairData x) ++ "}"

-- | Make a paired interval.
makePairedInterval :: b -> Interval a -> PairedInterval b a
makePairedInterval d i = PairedInterval (i, d)

-- | Gets the data (i.e. non-interval) part of a @PairedInterval@.
getPairData :: PairedInterval b a -> b
getPairData (PairedInterval (_, y)) = y

-- | Tests for equality of the data in a @PairedInterval@.
equalPairData :: (Eq b) => ComparativePredicateOf1 (PairedInterval b a)
equalPairData x y = getPairData x == getPairData y

-- | Gets the intervals from a list of paired intervals.
intervals :: (Ord a, Functor f) => f (PairedInterval b a) -> f (Interval a)
intervals = fmap getInterval

-- | Empty is used to trivially lift an @Interval a@ into a @PairedInterval@.
data Empty = Empty deriving (Eq, Ord, Show)
instance Semigroup Empty where
  x <> y = Empty
instance Monoid Empty where
  mempty = Empty

-- | Lifts an @Interval a@ into a @PairedInterval Empty a@, where @Empty@ is a
--   trivial type that contains no data.
toTrivialPair :: Interval a -> PairedInterval Empty a
toTrivialPair = makePairedInterval Empty

-- | Lifts a @Functor@ containing @Interval a@(s) into a @Functor@ containing
--   @PairedInterval Empty a@(s).
trivialize :: Functor f => f (Interval a) -> f (PairedInterval Empty a)
trivialize = fmap toTrivialPair


-- TODO REFACTOR need to revisit this
-- Arbitrary instance
--instance (Arbitrary b, Arbitrary (Interval a)) => Arbitrary (PairedInterval b a) where
--  arbitrary = liftA2 makePairedInterval arbitrary arbitrary
