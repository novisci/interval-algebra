{-|
Module      : Generate arbitrary Intervals
Description : Functions for generating arbitrary intervals
Copyright   : (c) NoviSci, Inc 2020-2022
                  TargetRWE, 2023
License     : BSD3
Maintainer  : bsaul@novisci.com 2020-2022, bbrown@targetrwe.com 2023
Stability   : experimental
-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}


module IntervalAlgebra.Arbitrary where

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
                                      Intervallic, PairedInterval, Point,
                                      SizedIv (..), beginerval, converse,
                                      duration, makePairedInterval, moment,
                                      predicate, strictWithinRelations)
import           Test.QuickCheck     (Arbitrary (arbitrary, shrink), Gen,
                                      NonNegative, arbitrarySizedNatural,
                                      elements, resize, sized, suchThat)

-- NOTE: the default size for arbitrary :: Gen Int appears to be 30
arbitrarySizedPositive :: Integral a => Gen a
arbitrarySizedPositive = (+ 1) <$> arbitrarySizedNatural

-- resize in utctDayTime is to avoid rare leap-seconds-related failure, in
-- which e.g.  1858-12-31 00:00:00 UTC /= 1858-12-30 23:59:60 UTC
maxDiffTime :: Int
maxDiffTime = 86399

--instance Arbitrary DT.DiffTime where
--  arbitrary = sized
--
--
--instance Arbitrary DT.UTCTime  where
--  arbitrary = liftA2 UTCTime arbitrary arbitrary

-- Helper
-- NOTE: You likely want to restrict the size of `dur` in a more appropriate
-- way, to be uniform over the range >= moment.
sizedIntervalGen :: (SizedIv (Interval a), Ord (Moment (Interval a))) => Int -> Gen a -> Gen (Moment (Interval a)) -> Gen (Interval a)
sizedIntervalGen s gpt gmom = do
  b <- s `resize` gpt
  dur <- s `resize` gmom
  pure $ beginerval dur b

-- Generators for types that do not implement Arbitrary. This avoids creating
-- orphan instances for these types.

genDay :: Gen DT.Day
genDay = sized (\s -> DT.ModifiedJulianDay <$> s `resize` arbitrary)

genNominalDiffTime :: Gen DT.NominalDiffTime
genNominalDiffTime = sized (\s -> fromInteger <$> (min s maxDiffTime `resize` arbitrarySizedNatural))

genDiffTime :: Gen DT.DiffTime
genDiffTime = sized (\s -> fromInteger <$> (min s maxDiffTime `resize` arbitrarySizedNatural))

genUTCTime :: Gen DT.UTCTime
genUTCTime = sized (\s -> liftA2 UTCTime genDay genDiffTime)

-- Arbitrary instances
-- for SizedIv instances defined in Core

instance Arbitrary (Interval Int) where
  arbitrary = sized (\s -> sizedIntervalGen s arbitrary arbitrary)

instance Arbitrary (Interval Integer) where
  arbitrary = sized (\s -> sizedIntervalGen s arbitrary arbitrary)

instance Arbitrary (Interval Double) where
  arbitrary = sized (\s -> sizedIntervalGen s arbitrary arbitrary)

instance Arbitrary (Interval DT.Day) where
  arbitrary = sized (\s -> sizedIntervalGen s genDay arbitrary)

instance Arbitrary (Interval DT.UTCTime) where
  arbitrary = sized (\s -> sizedIntervalGen s genUTCTime genNominalDiffTime)


-- | Conditional generation of intervals relative to a reference.  If the
-- reference @iv@ is of 'moment' duration, it is not possible to generate
-- intervals from the strict enclose relations StartedBy, Contains, FinishedBy.
-- If @iv@ and @rs@ are such that no possible relations can be generated, this
-- function returns `Nothing`. Otherwise, it returns `Just` an interval that
-- satisfies at least one of the possible relations in @rs@ relative to
-- @iv@.
--
-- @
-- > import Test.QuickCheck (generate)
-- > import Data.Set (fromList)
-- > isJust $ generate $ arbitraryWithRelation (beginerval 10 (0::Int)) (fromList [Before])
-- Just (20, 22)
-- > generate $ arbitraryWithRelation (beginerval 1 (0::Int)) (fromList [StartedBy])
-- Nothing
-- > generate $ arbitraryWithRelation (beginerval 1 (0::Int)) (fromList [StartedBy, Before])
-- Just (4, 13)
-- @
--
arbitraryWithRelation
  :: forall i a b
   . (SizedIv (Interval a), Ord a, Eq (Moment (Interval a)), Arbitrary (Interval a))
  => Interval a -- ^ reference interval
  -> Data.Set.Set IntervalRelation -- ^ set of `IntervalRelation`s, of which at least one will hold for the generated interval relative to the reference
  -> Gen (Maybe (Interval a))
arbitraryWithRelation iv rs
  | rs == Data.Set.singleton Equals = elements [Just iv]
  | isEnclose && isMom = elements [Nothing]
  | isMom = Just <$> arbitrary `suchThat` predicate notStrictEnclose iv
  | otherwise = Just <$> arbitrary `suchThat` predicate rs iv
 where
  notStrictEnclose = Data.Set.difference rs (converse strictWithinRelations)
  isEnclose        = Data.Set.null notStrictEnclose
  isMom            = duration iv == moment @(Interval a)
