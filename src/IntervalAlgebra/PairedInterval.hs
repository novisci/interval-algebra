{-|
Module      : Paired interval 
Description : Extends the Interval Algebra to an interval paired with some data.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}
{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IntervalAlgebra.PairedInterval (
      PairedInterval
    , Empty
    , makePairedInterval
    , getPairData
    , intervals
    , equalPairData
    , toTrivialPair
    , trivialize
) where

import IntervalAlgebra  ( Interval
                        , Intervallic(..)
                        , before
                        , IntervalCombinable(..)
                        , ComparativePredicateOf1
                        , extenterval )
import Witherable       ( Filterable(filter) )
import Data.Bifunctor   ( Bifunctor(bimap) )

-- | An @Interval a@ paired with some other data of type @b@.
newtype PairedInterval b a = PairedInterval (Interval a, b)
    deriving (Eq)

instance (Ord a) => Intervallic (PairedInterval b) a where
    getInterval (PairedInterval x)        = fst x
    setInterval (PairedInterval (x, y)) i = PairedInterval (i, y)

instance Bifunctor PairedInterval where
    bimap f g (PairedInterval (x, y)) = PairedInterval (fmap g x, f y)

-- | Defines A total ordering on 'PairedInterval b a' based on the 'Interval a'
--   part.
instance (Eq a, Eq b, Ord a) => Ord (PairedInterval b a) where
  (<=) x y = getInterval x <= getInterval y
  (<) x y  = getInterval x <  getInterval y

instance (Show b, Show a, Ord a) => Show (PairedInterval b a) where
    show x = "{" ++ show (getInterval x) ++ ", " ++ show (getPairData x) ++ "}"

instance (Ord a, Eq b, Monoid b) => 
          IntervalCombinable (PairedInterval b) a where
    (><) x y = fmap (makePairedInterval mempty) (getInterval x >< getInterval y)

    (<+>) x y
        | x `before` y = pure x <> pure y
        | otherwise    = pure $ makePairedInterval (getPairData x <> getPairData y)
                                                 (extenterval x y) 


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
intervals :: (Ord a) => [PairedInterval b a] -> [Interval a]
intervals = map getInterval

-- | Empty is used to trivially lift an @Interval a@ into a @PairedInterval@.
data Empty = Empty deriving (Eq, Ord, Show)
instance Semigroup Empty where 
    x <> y = Empty
instance Monoid Empty where
    mempty = Empty
    mappend x y = x <> y

-- | Lifts an @Interval a@ into a @PairedInterval Empty a@, where @Empty@ is a
--   trivial type that contains no data.
toTrivialPair :: Interval a -> PairedInterval Empty a
toTrivialPair = makePairedInterval Empty

-- | Lifts a @Functor@ containing @Interval a@(s) into a @Functor@ containing
--   @PairedInterval Empty a@(s).
trivialize :: Functor f => f (Interval a) -> f (PairedInterval Empty a)
trivialize = fmap toTrivialPair
