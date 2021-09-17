{-|
Module      : Generate arbitrary Intervals
Description : Functions for generating arbitrary intervals
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}

module IntervalAlgebra.Arbitrary() where

import Test.QuickCheck      ( Arbitrary(arbitrary, shrink),
                             Gen, NonNegative )
import GHC.Int              ( Int )
import GHC.Num
import GHC.Real
import GHC.Float
import Control.Applicative  ( (<$>), liftA2 )
import Control.Monad        ( liftM2 )
import IntervalAlgebra      (Interval, beginerval)
import Data.Function        ( (.), ($) )
import Data.Fixed 
import Data.Bool
import Data.Ord
import Data.Time as DT      ( Day(ModifiedJulianDay)
                            , toModifiedJulianDay
                            , picosecondsToDiffTime
                            , secondsToDiffTime
                            , secondsToNominalDiffTime
                            , UTCTime(..), NominalDiffTime)

instance Arbitrary (Interval Int) where
  arbitrary = liftM2 beginerval arbitrary arbitrary

instance Arbitrary DT.Day where
    arbitrary = DT.ModifiedJulianDay <$> arbitrary
    shrink    = (DT.ModifiedJulianDay <$>) . shrink . DT.toModifiedJulianDay


withinDiffTimeRange ::  Integer -> Integer
withinDiffTimeRange x
    | x < 0     = 0
    | x > 86400 = 86400
    | otherwise = x
   
instance Arbitrary DT.NominalDiffTime where
   arbitrary = fromInteger . withinDiffTimeRange <$> (arbitrary :: Gen Integer)

instance Arbitrary DT.UTCTime  where
    arbitrary = liftA2 UTCTime  
                  arbitrary 
                  (secondsToDiffTime . withinDiffTimeRange <$> (arbitrary :: Gen Integer) )

instance Arbitrary (Interval DT.Day) where
  arbitrary = liftM2 beginerval arbitrary arbitrary

instance Arbitrary (Interval DT.UTCTime) where
  arbitrary = liftM2 beginerval arbitrary arbitrary