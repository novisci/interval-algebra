module IntervalAlgebra.IntervalUtilitiesSpec (spec) where

import IntervalAlgebra ( Interval, Intervallic(unsafeInterval) )
import IntervalAlgebra.IntervalUtilities ( combineIntervals, gaps, durations )
import Test.Hspec ( it, shouldBe, describe, Spec )

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