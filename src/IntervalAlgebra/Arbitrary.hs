{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Generate arbitrary Intervals
Description : Functions for generating arbitrary intervals
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}
module IntervalAlgebra.Arbitrary(
    makePos
    , safeInterval
    , safeInterval'
    , safeInterval''
) where

import Test.QuickCheck ( Arbitrary(arbitrary, shrink) )
import GHC.Base
    ( otherwise,
      ($),
      Eq((==)),
      Ord((<=), (<), min, max),
      Int,
      Maybe(..),
      (.), 
      liftM2 )
import Control.Applicative((<$>))
import GHC.Num ( Num((+), negate) )
import IntervalAlgebra (
      Interval
    , Intervallic(unsafeInterval)
    , IntervalSizeable(add))
import Data.Time as DT ( Day(ModifiedJulianDay), toModifiedJulianDay)

instance Arbitrary (Interval Int) where
  arbitrary = liftM2 safeInterval' arbitrary arbitrary

instance Arbitrary DT.Day where
    arbitrary = DT.ModifiedJulianDay . (2000 +) <$> arbitrary
    shrink    = (DT.ModifiedJulianDay <$>) . shrink . DT.toModifiedJulianDay

instance Arbitrary (Interval DT.Day) where
  arbitrary = liftM2 safeInterval' arbitrary arbitrary

type IntervalInt = Interval Int

-- | Internal function for converting a number to a strictly positive value.
makePos :: (Ord b, Num b) => b -> b
makePos x
  | x == 0    = x + 1
  | x <  0    = negate x
  | otherwise = x

-- | A function for creating intervals when you think you know what you're doing.
safeInterval :: (Intervallic a) => a -> a -> Interval a
safeInterval x y = unsafeInterval (min x y) (max x y)

-- | Safely create a valid 'Interval a' from two @a@ by adding a positive @dur@
--   to @start@ to set the duration of the interval.
safeInterval' :: (IntervalSizeable a b) => a -> b -> Interval a
safeInterval' start dur = safeInterval start (add (makePos dur) start)

-- | Create a 'Maybe Interval a' from two @a@s.
safeInterval'' :: (Intervallic a) => a -> a -> Maybe (Interval a)
safeInterval'' x y
    | y <= x    = Nothing
    | otherwise = Just $ safeInterval x y