{-# LANGUAGE TypeApplications #-}
module IntervalAlgebra.IntervalUtilitiesSpec (spec) where

import IntervalAlgebra ( Interval, Intervallic(unsafeInterval)
                        , IntervalCombinable(..)
                        , IntervalAlgebraic(..)
                        , IntervalRelation (..) )
import IntervalAlgebra.Arbitrary ()
import IntervalAlgebra.IntervalUtilities
import Test.Hspec ( it, shouldBe, describe, Spec, pending )
import Test.QuickCheck
import Data.List(sort)

intInt :: Int -> Int -> Interval Int
intInt = unsafeInterval

containmentInt :: Interval Int
containmentInt = unsafeInterval (0 :: Int) (10 :: Int)

noncontainmentInt :: Interval Int
noncontainmentInt = unsafeInterval (4 :: Int) (10 :: Int)

anotherInt :: Interval Int
anotherInt = unsafeInterval (15 :: Int) (20 :: Int)

gapInt :: Interval Int
gapInt = unsafeInterval (10 :: Int) (15 :: Int)

prop_combineIntervals1:: (IntervalAlgebraic a, IntervalCombinable a)=>
     [Interval a] 
   -> Property
prop_combineIntervals1 x = 
   (length x > 2) ==> relations ci == replicate (length ci - 1) Before
      where ci = combineIntervals (sort x)

prop_gaps1:: (IntervalAlgebraic a, IntervalCombinable a)=>
     [Interval a] 
   -> Property
prop_gaps1 x = 
   (length x > 2) ==> relations gs == replicate (length gs - 1) Before
      where gs = gaps (sort x)


spec :: Spec
spec = do
   describe "combineIntervals unit tests" $
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
      it "combineIntervals [intInt 0 10, intInt 2 7, intInt 10 12, intInt 13 15]" $
         combineIntervals [intInt 0 10, intInt 2 7, intInt 10 12, intInt 13 15] 
            `shouldBe` [intInt 0 12, intInt 13 15]
      it "after combining, only relation should be Before" $ 
         property (prop_combineIntervals1 @Int) 

   
   describe "gaps tests" $
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
         -- it "durations of empty list is empty list" $
         --    durations [] `shouldBe` []
         it "durations of [containmentInt, anotherInt] is [10, 5]" $
            durations [containmentInt, anotherInt] `shouldBe` [10, 5]

   describe "clip tests" $
      do 
         it "clip disjoint should be Nothing" $
           clip containmentInt gapInt `shouldBe` Nothing
         it "clip Interval (4, 10) Interval (0, 10) should be Interval (4, 10)" $
           clip noncontainmentInt containmentInt `shouldBe` 
             Just (unsafeInterval 4 10)
         it "clip x y === intersect sort x y " pending

   describe "relations tests" $
      do 
         it "relations [(0, 10), (4, 10), (10, 15), (15, 20)] == [FinishedBy, Meets, Meets]" $
            relations [containmentInt, noncontainmentInt, gapInt, anotherInt] `shouldBe`
               [FinishedBy, Meets, Meets]
         it "more relations tests" pending

   describe "gapsWithin tests" $
      do 
         it "gapsWithin (1, 10) [(0,5), (7,9), (12,15)] should be [(5,7), (9,10)]" $
            gapsWithin (intInt 1 10) [intInt 0 5, intInt 7 9, intInt 12 15] 
               `shouldBe` [intInt 5 7, intInt 9 10]
         it "more gapsWithin tests" pending