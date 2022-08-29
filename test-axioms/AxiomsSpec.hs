{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
module AxiomsSpec
  ( spec
  ) where

import           Data.Time              (Day, UTCTime)
import           IntervalAlgebra.Axioms (IntervalAxioms (..))
import           Test.Hspec             (Spec, describe, hspec, it)
import           Test.Hspec.QuickCheck  (modifyMaxSuccess)
import           Test.QuickCheck        (Arbitrary (arbitrary), Gen (..),
                                         Property, Testable (property),
                                         generate, quickCheck)


testScale :: Int
testScale = 1000

spec :: Spec
spec = do

  describe "An Axiomatization of Interval Time"
    $ modifyMaxSuccess (* testScale)
    $ do
        it "M1" $ property (prop_IAaxiomM1 @Int)
        it "M1" $ property (prop_IAaxiomM1 @Day)
        it "M1" $ property (prop_IAaxiomM1 @UTCTime)

        it "M2" $ property (prop_IAaxiomM2 @Int)
        it "M2" $ property (prop_IAaxiomM2 @Day)
        it "M2" $ property (prop_IAaxiomM2 @UTCTime)

        it "ML1" $ property (prop_IAaxiomML1 @Int)
        it "ML1" $ property (prop_IAaxiomML1 @Day)
        it "ML1" $ property (prop_IAaxiomML1 @UTCTime)

        it "ML2" $ property (prop_IAaxiomML2 @Int)
        it "ML2" $ property (prop_IAaxiomML2 @Day)
        it "ML2" $ property (prop_IAaxiomML2 @UTCTime)

        it "M3" $ property (prop_IAaxiomM3 @Int)
        it "M3" $ property (prop_IAaxiomM3 @Day)
        it "M3" $ property (prop_IAaxiomM3 @UTCTime)

        it "M4" $ property (prop_IAaxiomM4 @Int)
        it "M4" $ property (prop_IAaxiomM4 @Day)
        it "M4" $ property (prop_IAaxiomM4 @UTCTime)

        it "M5" $ property (prop_IAaxiomM5 @Int)
        it "M5" $ property (prop_IAaxiomM5 @Day)
        it "M5" $ property (prop_IAaxiomM5 @UTCTime)

        it "M4.1" $ property (prop_IAaxiomM4_1 @Int)
        it "M4.1" $ property (prop_IAaxiomM4_1 @Day)
        it "M4.1" $ property (prop_IAaxiomM4_1 @UTCTime)

