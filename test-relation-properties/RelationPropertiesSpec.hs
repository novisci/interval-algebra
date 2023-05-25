{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications      #-}
module RelationPropertiesSpec
  ( spec
  ) where

import           Data.Time
import           IntervalAlgebra.RelationProperties
import           Test.Hspec                         (Spec, describe, hspec, it)
import           Test.Hspec.QuickCheck              (modifyMaxSuccess)
import           Test.QuickCheck

testScale = 100

spec :: Spec
spec = do
  describe "Interval Algebra relation properties for Interval Int"
    $ modifyMaxSuccess (* testScale)
    $ do
        it "before" $ property (prop_IAbefore @Int)
        it "starts" $ property (prop_IAstarts @Int)
        it "finishes" $ property (prop_IAfinishes @Int)
        it "overlaps" $ property (prop_IAoverlaps @Int)
        it "during" $ property (prop_IAduring @Int)
        it "disjoint" $ property (prop_disjoint_predicate @Int)
        it "within" $ property (prop_within_predicate @Int)
        it "encloses" $ property (prop_encloses_predicate @Int)
        it "enclosedBy" $ property (prop_enclosedBy_predicate @Int)
        it "notDisjoint" $ property (prop_notdisjoint_predicate @Int)
        it "concur" $ property (prop_concur_predicate @Int)

  describe "Interval Algebra relation properties for Interval Day"
    $ modifyMaxSuccess (* testScale)
    $ do
        it "before" $ property (prop_IAbefore @Day)
        it "starts" $ property (prop_IAstarts @Day)
        it "finishes" $ property (prop_IAfinishes @Day)
        it "overlaps" $ property (prop_IAoverlaps @Day)
        it "during" $ property (prop_IAduring @Day)
        it "disjoint" $ property (prop_disjoint_predicate @Day)
        it "within" $ property (prop_within_predicate @Day)
        it "encloses" $ property (prop_encloses_predicate @Day)
        it "enclosedBy" $ property (prop_enclosedBy_predicate @Day)
        it "notDisjoint" $ property (prop_notdisjoint_predicate @Day)
        it "concur" $ property (prop_concur_predicate @Day)

  describe "Interval Algebra relation properties for Interval UTCTime"
    $ modifyMaxSuccess (* testScale)
    $ do
        it "before" $ property (prop_IAbefore @UTCTime)
        it "starts" $ property (prop_IAstarts @UTCTime)
        it "finishes" $ property (prop_IAfinishes @UTCTime)
        it "overlaps" $ property (prop_IAoverlaps @UTCTime)
        it "during" $ property (prop_IAduring @UTCTime)
        it "disjoint" $ property (prop_disjoint_predicate @UTCTime)
        it "within" $ property (prop_within_predicate @UTCTime)
        it "encloses" $ property (prop_encloses_predicate @UTCTime)
        it "enclosedBy" $ property (prop_enclosedBy_predicate @UTCTime)
        it "notDisjoint" $ property (prop_notdisjoint_predicate @UTCTime)
        it "concur" $ property (prop_concur_predicate @UTCTime)

  describe "Interval Algebra relation uniqueness"
    $ modifyMaxSuccess (* testScale)
    $ do
        it "exactly one relation must be true"
          $ property (prop_exclusiveRelations @Int)
        it "exactly one relation must be true"
          $ property (prop_exclusiveRelations @Day)
        it "exactly one relation must be true"
          $ property (prop_exclusiveRelations @UTCTime)
