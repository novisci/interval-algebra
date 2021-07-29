module IntervalAlgebra.PairedIntervalSpec (spec) where

import Test.Hspec                       ( it, describe, Spec, shouldBe )
import IntervalAlgebra.PairedInterval   ( PairedInterval
                                        , makePairedInterval
                                        , intervals )
import IntervalAlgebra                  ( beginerval
                                        , IntervalSizeable(duration)
                                        , equals
                                        , before )
import Data.Bifunctor                   ( Bifunctor(bimap) )
import Data.Bool
import Data.Time                        ( Day(ModifiedJulianDay)
                                        , fromGregorian )

type TestPair = PairedInterval String Int

mkTestPr :: String -> Int -> Int -> TestPair
mkTestPr x i j = makePairedInterval x (beginerval i j)

t1 :: TestPair
t1 = mkTestPr "hi" 5 0
t2 :: TestPair
t2 = mkTestPr "bye" 4 6
t3 :: TestPair
t3 = mkTestPr "hello" 5 0

spec :: Spec
spec = do
    describe "Basic tests of paired intervals" $
        do 
            it "the same pairInterval should be equal" $ t1 == t1 `shouldBe` True 
            it "different pairInterval should not be equal" $ t1 /= t2 `shouldBe` True  
            it "bimapping into a different type" $ 
                bimap (== "hi") ModifiedJulianDay (makePairedInterval "hi" (beginerval 5 0))
                    `shouldBe `makePairedInterval True (beginerval 5 (fromGregorian 1858 11 17))

    describe "tests on paired intervals" $
        do 
            it "t1 is before t2" $
                (t1 `before` t2) `shouldBe` True
            it "duration of t1 is 5" $
                duration t1 `shouldBe` 5
            it "t1 is equal to t3" $
                 (t1 `equals` t3) `shouldBe` True
            it "t1 is LT t2" $
                 (t1 < t2) `shouldBe` True
            it "getintervals [t1, t2, t3]" $
                intervals [t1, t2, t3] `shouldBe`
                 [beginerval 5 0, beginerval 4 6, beginerval 5 0]
                