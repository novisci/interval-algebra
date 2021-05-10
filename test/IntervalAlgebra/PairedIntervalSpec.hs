module IntervalAlgebra.PairedIntervalSpec (spec) where

import Test.Hspec                       ( it, describe, Spec, shouldBe )
import IntervalAlgebra.PairedInterval   ( PairedInterval, mkPairedInterval )
import IntervalAlgebra                  ( beginerval
                                        , IntervalSizeable(duration)
                                        , IntervalAlgebraic(equals, before) )

type TestPair = PairedInterval String Int

mkTestPr :: String -> Int -> Int -> TestPair
mkTestPr x i j = mkPairedInterval x (beginerval i j)

t1 :: TestPair
t1 = mkTestPr "hi" 5 0
t2 :: TestPair
t2 = mkTestPr "bye" 4 6
t3 :: TestPair
t3 = mkTestPr "hello" 5 0

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
                