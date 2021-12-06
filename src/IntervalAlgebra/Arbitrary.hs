{-|
Module      : Generate arbitrary Intervals
Description : Functions for generating arbitrary intervals
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Safe              #-}


module IntervalAlgebra.Arbitrary() where

import           Control.Applicative (liftA2, (<$>))
import           Control.Monad       (liftM2)
import           Data.Bool
import           Data.Fixed
import           Data.Function       (($), (.))
import           Data.Ord
import           Data.Time           as DT (Day (ModifiedJulianDay),
                                            NominalDiffTime, UTCTime (..),
                                            DiffTime,
                                            picosecondsToDiffTime,
                                            secondsToDiffTime,
                                            secondsToNominalDiffTime,
                                            toModifiedJulianDay)
import           GHC.Float
import           GHC.Int             (Int)
import           GHC.Num
import           GHC.Real
import           IntervalAlgebra     (Interval, beginerval)
import           Test.QuickCheck     (Arbitrary (arbitrary, shrink), Gen,
                                      arbitrarySizedNatural, resize)

-- NOTE: the default size for arbitrary :: Gen Int appears to be 30
arbitrarySizedPositive :: Integral a => Gen a
arbitrarySizedPositive = (+ 1) <$> arbitrarySizedNatural

maxDiffTime :: Int
maxDiffTime = 86400

instance Arbitrary (Interval Int) where
  arbitrary = liftM2 beginerval arbitrarySizedPositive arbitrary

instance Arbitrary DT.Day where
    arbitrary = DT.ModifiedJulianDay <$> arbitrary
    shrink    = (DT.ModifiedJulianDay <$>) . shrink . DT.toModifiedJulianDay

instance Arbitrary DT.NominalDiffTime where
   arbitrary = fromInteger <$> (maxDiffTime `resize` arbitrarySizedNatural)

instance Arbitrary DT.DiffTime where
   arbitrary = fromInteger <$> (maxDiffTime `resize` arbitrarySizedNatural)

instance Arbitrary DT.UTCTime  where
    arbitrary = liftA2 UTCTime arbitrary arbitrary
                  
instance Arbitrary (Interval DT.Day) where
  arbitrary = liftM2 beginerval arbitrary arbitrary

instance Arbitrary (Interval DT.UTCTime) where
  arbitrary = liftM2 beginerval arbitrary arbitrary
