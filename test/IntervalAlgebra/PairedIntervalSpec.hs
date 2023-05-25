module IntervalAlgebra.PairedIntervalSpec
  ( spec
  ) where

import           Data.Bool
import           Data.Time                      (Day (ModifiedJulianDay),
                                                 fromGregorian)
import           IntervalAlgebra                (Intervallic (..),
                                                 SizedIv (duration), before,
                                                 beginerval, equals,
                                                 toEnumInterval)
import           IntervalAlgebra.PairedInterval (Empty (..), PairedInterval,
                                                 intervals, makePairedInterval)
import           Test.Hspec                     (Spec, describe, it, shouldBe)

type TestPair = PairedInterval String Int

mkTestPr :: String -> Int -> Int -> TestPair
mkTestPr x i j = makePairedInterval x (beginerval i j)

t1 :: TestPair
t1 = mkTestPr "hi" 5 0
t2 :: TestPair
t2 = mkTestPr "bye" 4 6
t3 :: TestPair
t3 = mkTestPr "hello" 5 0

-- insta

spec :: Spec
spec = do
  describe "Basic tests of paired intervals" $ do
    it "the same pairInterval should be equal" $ t1 == t1 `shouldBe` True
    it "different pairInterval should not be equal" $ t1 /= t2 `shouldBe` True
    -- NOTE toEnum (fromGregorian 1858 11 17) is 0, since that date is the
    -- origin in the modified Julian calendar.
    it "toEnumInterval into PairedInterval b Day"
      $          toEnumInterval (makePairedInterval "hi" (beginerval 5 0))
      `shouldBe` makePairedInterval "hi"
                                    (beginerval 5 (fromGregorian 1858 11 17))
    it "show paired interval" $ show t1 `shouldBe` "{(0, 5), \"hi\"}"

    describe "tests on paired intervals" $ do
      it "t1 is before t2" $ (t1 `before` t2) `shouldBe` True
      it "duration of t1 is 5" $ duration (getInterval t1) `shouldBe` 5
      it "t1 is equal to t3" $ (t1 `equals` t3) `shouldBe` True
      it "t1 is LT t2" $ (t1 < t2) `shouldBe` True
      it "getintervals [t1, t2, t3]"
        $          intervals [t1, t2, t3]
        `shouldBe` [beginerval 5 0, beginerval 4 6, beginerval 5 0]

    describe "tests on empty" $ do
      it "show empty" $ show Empty `shouldBe` "Empty"
      it "combine emptyies" $ Empty <> Empty `shouldBe` Empty
      it "monoid empty" $ (mempty :: Empty) `shouldBe` Empty
      it "monoid <>" $ Empty <> Empty `shouldBe` Empty
      it "ord empty" $ Empty < Empty `shouldBe` False
      it "ord empty" $ Empty <= Empty `shouldBe` True
