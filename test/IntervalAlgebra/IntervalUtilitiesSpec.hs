module IntervalAlgebra.IntervalUtilitiesSpec (spec) where

import IntervalAlgebra ( Interval, Intervallic(unsafeInterval), IntervalRelation (FinishedBy, Meets) )
import IntervalAlgebra.IntervalUtilities
import Test.Hspec ( it, shouldBe, describe, Spec, pending )

containmentInt :: Interval Int
containmentInt = unsafeInterval (0 :: Int) (10 :: Int)

noncontainmentInt :: Interval Int
noncontainmentInt = unsafeInterval (4 :: Int) (10 :: Int)

anotherInt :: Interval Int
anotherInt = unsafeInterval (15 :: Int) (20 :: Int)

gapInt :: Interval Int
gapInt = unsafeInterval (10 :: Int) (15 :: Int)

spec :: Spec
spec = do
   describe "combineIntervals unit tests" $
    do
      it "noncontainmentInt combined into containmentInt" $
         combineIntervals [containmentInt, noncontainmentInt] `shouldBe` [containmentInt]
      it "noncontainmentInt combined into containmentInt; anotherInt unchanged" $
         combineIntervals [containmentInt, noncontainmentInt, anotherInt] `shouldBe` [containmentInt, anotherInt]
      it "idempotency of containmentInt" $
         combineIntervals [containmentInt] `shouldBe` [containmentInt]
      it "idempotency of noncontainmentInt" $
         combineIntervals [noncontainmentInt] `shouldBe` [noncontainmentInt]
   
   describe "gaps unit tests" $
    do 
      it "no gaps in containmentInt and noncontainmentInt" $
         gaps [containmentInt, noncontainmentInt] `shouldBe` []
      it "no gaps in containmentInt" $
         gaps [containmentInt] `shouldBe` []
      it "single gap between containmentInt and anotherInt" $
         gaps [containmentInt, anotherInt] `shouldBe` [gapInt]
  
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