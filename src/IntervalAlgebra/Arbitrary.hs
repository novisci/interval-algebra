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

module IntervalAlgebra.Arbitrary() where

import Test.QuickCheck      ( Arbitrary(arbitrary, shrink) )
import GHC.Int              ( Int )
import GHC.Num              ( Num((+), negate) )
import Control.Applicative  ( (<$>) )
import Control.Monad        ( liftM2 )
import IntervalAlgebra      (Interval, beginerval)
import Data.Function        ( (.) )
import Data.Time as DT      ( Day(ModifiedJulianDay), toModifiedJulianDay)

instance Arbitrary (Interval Int) where
  arbitrary = liftM2 beginerval arbitrary arbitrary

instance Arbitrary DT.Day where
    arbitrary = DT.ModifiedJulianDay . (2000 +) <$> arbitrary
    shrink    = (DT.ModifiedJulianDay <$>) . shrink . DT.toModifiedJulianDay

instance Arbitrary (Interval DT.Day) where
  arbitrary = liftM2 beginerval arbitrary arbitrary
