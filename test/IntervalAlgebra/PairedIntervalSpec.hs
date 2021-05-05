module IntervalAlgebra.PairedIntervalSpec (spec) where

import Test.Hspec ( it, describe, Spec, shouldBe )
import IntervalAlgebra.PairedInterval
import IntervalAlgebra

type TestPair = PairedInterval String Int

mkTestPr :: String -> Int -> Int -> TestPair
mkTestPr x i j = mkPairedInterval x (unsafeInterval i j)

t1 :: TestPair
t1 = mkTestPr "hi" 0 5
t2 :: TestPair
t2 = mkTestPr "bye" 6 10
t3 :: TestPair
t3 = mkTestPr "hello" 0 5

spec :: Spec
spec = do
    describe "tests on paired intervals" $
        do 
            it "t1 is before t2" $
                (t1 `before` t2) `shouldBe` True
            it "duration of t1 is 5" $
                duration t1 `shouldBe` 5
            it "t1 is equal to t3" $
                 (t1 `equals` t3) `shouldBe` True
                