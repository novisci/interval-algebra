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


module IntervalAlgebra.Arbitrary( arbitraryWithRelation ) where

import           Control.Applicative (liftA2, (<$>))
import           Control.Monad       (liftM2)
import           Data.Bool
import           Data.Fixed
import           Data.Function       (flip, ($), (.))
import           Data.Maybe          (Maybe (Just, Nothing))
import           Data.Ord
import qualified Data.Set            (Set, difference, null, singleton)
import           Data.Time           as DT (Day (ModifiedJulianDay), DiffTime,
                                            NominalDiffTime, UTCTime (..),
                                            picosecondsToDiffTime,
                                            secondsToDiffTime,
                                            secondsToNominalDiffTime,
                                            toModifiedJulianDay)
import           GHC.Float
import           GHC.Int             (Int)
import           GHC.Num
import           GHC.Real
import           IntervalAlgebra     (Interval, IntervalRelation (..),
                                      IntervalSizeable, Intervallic, beginerval,
                                      converse, duration, moment', predicate,
                                      strictWithinRelations)
import           Prelude             (Eq, (==))
import           Test.QuickCheck     (Arbitrary (arbitrary, shrink), Gen,
                                      NonNegative, arbitrarySizedNatural,
                                      elements, resize, suchThat, sized)

-- NOTE: the default size for arbitrary :: Gen Int appears to be 30
arbitrarySizedPositive :: Integral a => Gen a
arbitrarySizedPositive = (+ 1) <$> arbitrarySizedNatural

-- resize in utctDayTime is to avoid rare leap-seconds-related failure, in
-- which e.g.  1858-12-31 00:00:00 UTC /= 1858-12-30 23:59:60 UTC
maxDiffTime :: Int
maxDiffTime = 86399

instance Arbitrary (Interval Int) where
  arbitrary = liftM2 beginerval arbitrarySizedPositive arbitrary

instance Arbitrary DT.Day where
    arbitrary = sized (\s -> DT.ModifiedJulianDay <$> s `resize` arbitrary)
    shrink    = (DT.ModifiedJulianDay <$>) . shrink . DT.toModifiedJulianDay

instance Arbitrary DT.NominalDiffTime where
   arbitrary = sized (\s -> fromInteger <$> (min s maxDiffTime `resize` arbitrarySizedNatural))

instance Arbitrary DT.DiffTime where
   arbitrary = sized (\s -> fromInteger <$> (min s maxDiffTime `resize` arbitrarySizedNatural))

instance Arbitrary DT.UTCTime  where
    arbitrary = liftA2 UTCTime arbitrary arbitrary
                  
instance Arbitrary (Interval DT.Day) where
  arbitrary = liftM2 beginerval arbitrary arbitrary

instance Arbitrary (Interval DT.UTCTime) where
  arbitrary = liftM2 beginerval arbitrary arbitrary

-- | Conditional generation of intervals relative to a reference.  If the
-- reference `iv` is of 'moment' duration, it is not possible to generate
-- intervals from the strict enclose relations StartedBy, Contains, FinishedBy.
-- If `iv` and `rs` are such that no possible relations can be generated, this
-- function returns `Nothing`. Otherwise, it returns `Just` an interval that
-- satisfies at least one of the possible relations in `rs` relative to
-- `iv`.
--
-- >>> generate $ arbitraryWithRelation (beginerval 10 (0::Int)) (fromList [Before])
-- Just (20, 22)
-- >>> generate $ arbitraryWithRelation (beginerval 1 (0::Int)) (fromList [StartedBy])
-- Nothing
-- >>> generate $ arbitraryWithRelation (beginerval 1 (0::Int)) (fromList [StartedBy, Before])
-- Just (4, 13)
arbitraryWithRelation :: (IntervalSizeable a b, Intervallic i a, Arbitrary (i a)) => 
  i a -- ^ reference interval
  -> Data.Set.Set IntervalRelation -- ^ set of `IntervalRelation`s, of which at least one will hold for the generated interval relative to the reference
  -> Gen (Maybe (i a))
arbitraryWithRelation iv rs
  | rs == Data.Set.singleton Equals = elements [Just iv]
  | isEnclose && isMom = elements [Nothing]
  | isMom = Just <$> arbitrary `suchThat` predicate notStrictEnclose iv
  | otherwise = Just <$> arbitrary `suchThat` predicate rs iv
  where
    notStrictEnclose = Data.Set.difference rs (converse strictWithinRelations)
    isEnclose = Data.Set.null notStrictEnclose
    isMom = duration iv == moment' iv
