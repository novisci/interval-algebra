{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
module IntervalAlgebra.IntervalUtilitiesSpec (spec) where

import Test.Hspec.QuickCheck              ( modifyMaxSuccess )
import Test.Hspec                         ( Spec
                                          , it, shouldBe, describe, pending )
import Test.QuickCheck                    ( Property, Testable(property)
                                          , (===))
import Data.List                          (sort)
import IntervalAlgebra.Arbitrary          ()
import IntervalAlgebra                    ( Interval
                                          , Intervallic(..)
                                          , IntervalCombinable(..)
                                          , IntervalAlgebraic(..)
                                          , IntervalRelation (..)
                                          , beginerval)
import IntervalAlgebra.IntervalUtilities  ( combineIntervals
                                          , gaps
                                          , durations
                                          , clip
                                          , relations
                                          , gapsWithin
                                          , nothingIfNone
                                          , filterDisjoint
                                          , filterNotDisjoint )

intInt :: Int -> Int -> Interval Int
intInt = beginerval

containmentInt :: Interval Int
containmentInt = intInt (10 :: Int) (0 :: Int)

noncontainmentInt :: Interval Int
noncontainmentInt = intInt  6 4

anotherInt :: Interval Int
anotherInt = intInt 5 (15 :: Int)

gapInt :: Interval Int
gapInt = intInt 5 (10 :: Int)

prop_combineIntervals1:: (IntervalAlgebraic Interval a, IntervalCombinable Interval a)=>
     [Interval a]
   -> Property
prop_combineIntervals1 x = relations ci === replicate (length ci - 1) Before
      where ci = combineIntervals (sort x)

prop_gaps1:: (IntervalAlgebraic Interval a, IntervalCombinable Interval a)=>
     [Interval a]
   -> Property
prop_gaps1 x = relations gs === replicate (length gs - 1) Before
      where gs = gaps (sort x)


spec :: Spec
spec = do
   describe "combineIntervals unit tests" $
    modifyMaxSuccess (*10) $
    do
      it "noncontainmentInt combined into containmentInt" $
         combineIntervals [containmentInt, noncontainmentInt]
            `shouldBe` [containmentInt]
      it "noncontainmentInt combined into containmentInt; anotherInt unchanged" $
         combineIntervals [containmentInt, noncontainmentInt, anotherInt]
            `shouldBe` [containmentInt, anotherInt]
      it "idempotency of containmentInt" $
         combineIntervals [containmentInt] `shouldBe` [containmentInt]
      it "idempotency of noncontainmentInt" $
         combineIntervals [noncontainmentInt] `shouldBe` [noncontainmentInt]
      it "combineIntervals [] should be []" $
         combineIntervals ([] :: [Interval Int]) `shouldBe` []
      it "combineIntervals [(0, 10), (2, 7), (10, 12), (13, 15)]" $
         combineIntervals [intInt 10 0, intInt 5 2, intInt 2 10, intInt 2 13]
            `shouldBe` [intInt 12 0, intInt 2 13]
      it "after combining, only relation should be Before" $
         property (prop_combineIntervals1 @Int)

   describe "gaps tests" $
    modifyMaxSuccess (*10) $
    do
      it "no gaps in containmentInt and noncontainmentInt" $
         gaps [containmentInt, noncontainmentInt] `shouldBe` []
      it "no gaps in containmentInt" $
         gaps [containmentInt] `shouldBe` []
      it "single gap between containmentInt and anotherInt" $
         gaps [containmentInt, anotherInt] `shouldBe` [gapInt]
      it "after gaps, only relation should be Before" $
         property (prop_gaps1 @Int)

   describe "durations unit tests" $
      do
         it "durations of containmentInt is 10" $
            durations [containmentInt] `shouldBe` [10]
         it "durations of empty list is empty list" $
            durations  ([] :: [Interval Int])  `shouldBe` []
         it "durations of [containmentInt, anotherInt] is [10, 5]" $
            durations [containmentInt, anotherInt] `shouldBe` [10, 5]

   describe "clip tests" $
      do
         it "clip disjoint should be Nothing" $
           clip containmentInt gapInt `shouldBe` Nothing
         it "clip Interval (4, 10) Interval (0, 10) should be Interval (4, 10)" $
           clip noncontainmentInt containmentInt `shouldBe`
             Just (intInt 6 4)
         it "clip x y === intersect sort x y " pending

   describe "relations tests" $
      do
         it "relations [(0, 10), (4, 10), (10, 15), (15, 20)] == [FinishedBy, Meets, Meets]" $
            relations [containmentInt, noncontainmentInt, gapInt, anotherInt] `shouldBe`
               [FinishedBy, Meets, Meets]
         it "relations of [] shouldBe []" $
            relations ([] :: [Interval Int]) `shouldBe` []
         it "relations of singleton shouldBe []" $
            relations [containmentInt] `shouldBe` []
         it "more relations tests" pending

   describe "gapsWithin tests" $
      do
         it "gapsWithin (1, 10) [(0,5), (7,9), (12,15)] should be [(5,7), (9,10)]" $
            gapsWithin (intInt 9 1) [intInt 5 0, intInt 2 7, intInt 3 12]
               `shouldBe` Just [intInt 2 5, intInt 1 9]
         it "gapsWithin (1, 10) [] should be []" $
             gapsWithin (intInt 9 1) [] `shouldBe` Nothing
         it "more gapsWithin tests" pending

   describe "emptyIf tests" $
      do
         it "emptyIfNone (starts (3, 5)) [(3,4), (5,6)] should be empty" $
            nothingIfNone (starts (intInt 2 3)) [intInt 1 3, intInt 1 5]
               `shouldBe` Nothing
         it "emptyIfNone (starts (3, 5)) [(3,6), (5,6)] shoiuld be input" $
            nothingIfNone (starts (intInt 2 3)) [intInt 3 3, intInt 1 5]
               `shouldBe` Just [ intInt 3 3, intInt 1 5]
         it "more emptyif tests" pending

   describe "filtration tests" $
      do 
         it "disjoint filter should filter out noncontainment" $
            filterDisjoint containmentInt [noncontainmentInt, anotherInt] 
               `shouldBe` [anotherInt]
         it "notDisjoint filter should keep noncontainment" $
            filterNotDisjoint containmentInt [noncontainmentInt, anotherInt] 
               `shouldBe` [noncontainmentInt]