{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module IntervalAlgebraSpec
  ( spec
  ) where

import           Control.Applicative       (liftA2)
import           Data.Either               (isRight)
import           Data.Fixed                (Pico)
import           Data.Maybe                (fromJust, isJust, isNothing)
import           Data.Set                  (Set, disjointUnion, fromList,
                                            member)
import           Data.Time                 as DT (Day (..), DiffTime,
                                                  NominalDiffTime, UTCTime (..),
                                                  fromGregorian,
                                                  picosecondsToDiffTime,
                                                  secondsToDiffTime)
import           GHC.Real                  (Rational (..), Real (..))
import           IntervalAlgebra           as IA
import           IntervalAlgebra.Arbitrary (genDay)
import           Test.Hspec                (Spec, describe, hspec, it, shouldBe)
import           Test.Hspec.QuickCheck     (modifyMaxDiscardRatio,
                                            modifyMaxSuccess)
import           Test.QuickCheck           (Arbitrary (arbitrary), Gen (..),
                                            Property, Testable (property),
                                            forAll, generate, quickCheck,
                                            (.&&.), (===), (==>))

-- Convenience aliases
interval :: (iv ~ Interval a, SizedIv iv, Ord a, Ord (Moment iv)) => a -> a -> iv
interval = curry safeInterval

mkIntrvl :: Int -> Int -> Interval Int
mkIntrvl = beginerval

prop_expandl_end
  :: (SizedIv (Interval a), Show a, Eq a) => Moment (Interval a) -> Interval a -> Property
prop_expandl_end d i = end (expandl d i) === end i


prop_expandr_begin
  :: (SizedIv (Interval a), Show a, Eq a) => Moment (Interval a) -> Interval a -> Property
prop_expandr_begin d i = begin (expandr d i) === begin i

-- | The relation between x and z should be an element of the set of the
--   composed relations between x y and between y z.
prop_compose :: (Ord a, SizedIv (Interval a)) => Interval a -> Interval a -> Interval a -> Property
prop_compose x y z =
  member (relate x z) (compose (relate x y) (relate y z)) === True

-- | If two intervals are disjoint and not meeting, then there should be a gap
-- between the two (by ><), after the intervals are sorted.
prop_combinable_gap_exists :: (Ord a, SizedIv (Interval a), Ord (Moment (Interval a))) => Interval a -> Interval a -> Property
prop_combinable_gap_exists x y =
  (before <|> after) x y ==> isJust ((><) (min x y) (max x y))

-- | If two intervals are not disjoint or meeting, then there should be NO gap
-- between the two (by ><), after the intervals are sorted.
prop_combinable_nogap_exists :: (Ord a, SizedIv (Interval a), Ord (Moment (Interval a))) => Interval a -> Interval a -> Property
prop_combinable_nogap_exists x y =
  (predicate $ complement $ fromList [Before, After]) x y
    ==> isNothing ((><) (min x y) (max x y))

  {- Properties of SizedIv -}

-- When @Point iv@ is @Ord@,
--
-- prop> ivBegin i < ivEnd i
prop_validIv :: forall a. (SizedIv (Interval a), Ord a, Show a, Ord (Moment (Interval a))) => a -> a -> Property
prop_validIv b e = (ivBegin i < ivEnd i) === True where i = safeInterval (b, e)

-- When @iv@ is @Eq@,
--
-- prop> interval (ivBegin i) (ivEnd i) == i
prop_validIv' :: forall a. (SizedIv (Interval a), Ord a, Show a, Ord (Moment (Interval a))) => a -> a -> Property
prop_validIv' b e = interval (ivBegin i) (ivEnd i) === i where i = interval b e

-- When @iv@ is @Ord@, for all @i == interval b e@,
--
-- prop> ivExpandr d i >= i
-- prop> ivExpandl d i <= i
prop_ivExpandr, prop_ivExpandl :: forall a. (SizedIv (Interval a), Ord (Interval a), Show (Interval a)) => Moment (Interval a) -> Interval a -> Property
prop_ivExpandr d i = (ivExpandr d i >= i) === True
prop_ivExpandl d i = (ivExpandl d i <= i) === True

-- When @Moment iv@ is @Ord@,
--
-- prop> duration (interval b e) >= moment
-- prop> duration (ivExpandr d i) >= duration i
-- prop> duration (ivExpandl d i) >= duration i
prop_duration :: forall a. (SizedIv (Interval a), Ord a, Ord (Moment (Interval a)), Show (Interval a)) => Moment (Interval a) -> a -> a -> Property
prop_duration d b e = p1 .&&. p2 .&&. p3
  where i = interval b e
        m = moment @(Interval a)
        dur = duration i
        p1 = (dur >= m) === True
        p2 = (duration (ivExpandr d i) >= dur) === True
        p3 = (duration (ivExpandl d i) >= dur) === True

{- Specs -}
spec :: Spec
spec = do
  describe "Basic Interval unit tests of typeclass and creation methods" $ do
    it "equality works"
      $          beginerval 6 (1 :: Int)
      ==         beginerval 6 1
      `shouldBe` True
    it "equality works"
      $          beginerval 0    (1 :: Int)
      ==         beginerval (-1) 1
      `shouldBe` True
    it "equality works"
      $          enderval 1 (2 :: Int)
      ==         beginerval 1 1
      `shouldBe` True
    it "not equality works"
      $          enderval 5 (2 :: Int)
      /=         beginerval 1 1
      `shouldBe` True

    it "beginervalMoment duration is moment"
      $          duration (beginervalMoment (-13 :: Int))
      `shouldBe` (moment @(Interval Int))
    it "endervalMoment duration is moment"
      $          duration (endervalMoment (26 :: Int))
      `shouldBe` (moment @(Interval Int))

    it "parsing fails on bad inputs" $ parseInterval 10 0 `shouldBe` Left
      (IA.ParseErrorInterval "0<=10")
    it "parsing fails on bad inputs" $ parseInterval 0 0 `shouldBe` Left
      (IA.ParseErrorInterval "0<=0")
    it "parsing works on good inputs" $ parseInterval 0 10 `shouldBe` Right
      (beginerval 10 (0 :: Int))

    it "show displays intervals as expected"
      $          show (beginerval 10 (0 :: Int))
      `shouldBe` "(0, 10)"

    -- NOTE toEnum (fromGregorian 1858 11 17) is 0,
    -- since that date is the origin in the modified
    -- Julian calendar.
    it "fromEnumInterval converts Interval Day"
      $          fromEnumInterval (beginerval 0 (fromGregorian 1858 11 17))
      `shouldBe` beginerval 0 0

    it "(0, 2) <= (1, 3) is True"
      $          beginerval 2 (0 :: Int)
      <=         beginerval 2 1
      `shouldBe` True

    it "(1, 2) < (0, 3) is True"
      $          beginerval 2 (1 :: Int)
      <          beginerval 3 0
      `shouldBe` False
    it "(0, 2) < (1, 3) is True"
      $          beginerval 2 (0 :: Int)
      <          beginerval 2 1
      `shouldBe` True
    it "(0, 2) < (0, 3) is True"
      $          beginerval 2 (0 :: Int)
      <          beginerval 3 0
      `shouldBe` True

  describe "Basic IntervalRelation unit tests" $ do
    it "equality of IntervalRelations" $ Before == Before `shouldBe` True
    it "equality of IntervalRelations" $ Before /= After `shouldBe` True

    it "Bounds are set correctly" $ minBound @IntervalRelation `shouldBe` Before
    it "Bounds are set correctly" $ maxBound @IntervalRelation `shouldBe` After

    it "show Before is Before" $ show Before `shouldBe` "Before"

  describe "Relate unit tests" $ do
    it "relate before"
      $          relate (beginerval 1 (0 :: Int)) (beginerval 1 2)
      `shouldBe` Before
    it "relate after"
      $          relate (beginerval 1 (2 :: Int)) (beginerval 1 0)
      `shouldBe` After
    it "relate meets"
      $          relate (beginerval 1 (0 :: Int)) (beginerval 1 1)
      `shouldBe` Meets
    it "relate metBy"
      $          relate (beginerval 1 (1 :: Int)) (beginerval 1 0)
      `shouldBe` MetBy
    it "relate overlaps"
      $          relate (beginerval 3 (0 :: Int)) (beginerval 5 2)
      `shouldBe` Overlaps
    it "relate overlappedBy"
      $          relate (beginerval 5 (2 :: Int)) (beginerval 3 0)
      `shouldBe` OverlappedBy
    it "relate starts"
      $          relate (beginerval 3 (0 :: Int)) (beginerval 5 0)
      `shouldBe` Starts
    it "relate startedBy"
      $          relate (beginerval 5 (0 :: Int)) (beginerval 3 0)
      `shouldBe` StartedBy
    it "relate finishes"
      $          relate (enderval 3 (0 :: Int)) (enderval 5 0)
      `shouldBe` Finishes
    it "relate finishedBy"
      $          relate (enderval 5 (0 :: Int)) (enderval 3 0)
      `shouldBe` FinishedBy
    it "relate during"
      $          relate (beginerval 1 (1 :: Int)) (beginerval 3 0)
      `shouldBe` During
    it "relate Contains"
      $          relate (beginerval 3 (0 :: Int)) (beginerval 1 1)
      `shouldBe` Contains

  describe "IntervalRelation algebraic operations" $ do
    it "converse of Before is After"
      $          converse (fromList [Before])
      `shouldBe` fromList [After]

    it "union of IntervalRelations"
      $          union (fromList [Before]) (fromList [After])
      `shouldBe` fromList [Before, After]
    it "intersection of IntervalRelations"
      $          intersection (fromList [Before]) (fromList [After])
      `shouldBe` fromList []

  describe "SizedIv tests" $ do
    it "moment is 1" $ moment @(Interval Int) `shouldBe` 1
    it "expandl doesn't change end" $ property (prop_expandl_end @Int)
    it "expandr doesn't change begin" $ property (prop_expandr_begin @Int)
    it "expand 0 5 Interval (0, 1) should be Interval (0, 6)"
      $          expand 0 5 (beginerval (1 :: Int) (0 :: Int))
      `shouldBe` beginerval (6 :: Int) (0 :: Int)
    it "expand 5 0 Interval (0, 1) should be Interval (-5, 1)"
      $          expand 5 0 (beginerval (1 :: Int) (0 :: Int))
      `shouldBe` beginerval (6 :: Int) (-5 :: Int)
    it "expand 5 5 Interval (0, 1) should be Interval (-5, 6)"
      $          expand 5 5 (beginerval (1 :: Int) (0 :: Int))
      `shouldBe` beginerval (11 :: Int) (-5 :: Int)
    it "expand -1 5 Interval (0, 1) should be Interval (-5, 6)"
      $          expand (-1) 5 (beginerval (1 :: Int) (0 :: Int))
      `shouldBe` beginerval (6 :: Int) (0 :: Int)
    it "expand 5 -5 Interval (0, 1) should be Interval (-5, 1)"
      $          expand 5 (-5) (beginerval (1 :: Int) (0 :: Int))
      `shouldBe` beginerval (6 :: Int) (-5 :: Int)
    it "expand moment 0 Interval (0, 1) should be Interval (-1, 1)"
      $          expand (moment @(Interval Int)) 0 (beginerval (1 :: Int) (0 :: Int))
      `shouldBe` beginerval (2 :: Int) (-1 :: Int)

    it "beginerval 2 10 should be Interval (10, 12)"
      $          Right (beginerval (2 :: Int) 10)
      `shouldBe` parseInterval (10 :: Int) (12 :: Int)
    it "beginerval 0 10 should be Interval (10, 11)"
      $          Right (beginerval (0 :: Int) 10)
      `shouldBe` parseInterval (10 :: Int) (11 :: Int)
    it "beginerval -2 10 should be Interval (10, 11)"
      $          Right (beginerval (-2 :: Int) 10)
      `shouldBe` parseInterval (10 :: Int) (11 :: Int)
    it "enderval 2 10 should be Interval (8, 10)"
      $          Right (enderval (2 :: Int) 10)
      `shouldBe` parseInterval (8 :: Int) (10 :: Int)
    it "enderval 0 10 should be Interval (9, 10)"
      $          Right (enderval (0 :: Int) 10)
      `shouldBe` parseInterval (9 :: Int) (10 :: Int)
    it "enderval -2 10 should be Interval (9, 10)"
      $          Right (enderval (-2 :: Int) 10)
      `shouldBe` parseInterval (9 :: Int) (10 :: Int)

    it "shiftFromBegin can convert Interval Int to Interval Int"
      $          shiftFromBegin (beginerval 2 (4 :: Int)) (beginerval 2 10)
      `shouldBe` beginerval 2 6 -- (6, 8)

    it "shiftFromEnd can convert Interval Int to Interval Int"
      $          shiftFromEnd (beginerval 2 (4 :: Int)) (beginerval 2 10)
      `shouldBe` beginerval 2 4 -- (4, 6)

    it "momentize works"
      $          momentize (beginerval 2 (fromGregorian 2001 1 1))
      `shouldBe` beginerval 1 (fromGregorian 2001 1 1)

  describe "SizedIv properties for Interval Int" $ do
    it "validIv" $ property (prop_validIv @Int)
    it "validIv'" $ property (prop_validIv' @Int)
    it "ivExpandr" $ property (prop_ivExpandr @Int)
    it "ivExpandl" $ property (prop_ivExpandl @Int)
    it "duration" $ property (prop_duration @Int)

  describe "SizedIv properties for Interval Day" $ do
    it "validIv" $ forAll (liftA2 (,) genDay genDay) (uncurry prop_validIv)
    it "validIv'" $ forAll (liftA2 (,) genDay genDay) (uncurry prop_validIv')
    it "ivExpandr" $ property (prop_ivExpandr @Day)
    it "ivExpandl" $ property (prop_ivExpandl @Day)
    it "duration" $ forAll (do
      m <- arbitrary
      b <- genDay
      e <- genDay
      pure (m, b, e)
      ) (\(m, b, e) -> prop_duration m b e)


  describe "Intervallic tests" $
    --  modifyMaxSuccess (*10000) $
                                 do
    it "(startedBy <|> overlappedBy) Interval (0, 9) Interval (-1, 4) is True"
      $          (startedBy <|> overlappedBy) (mkIntrvl 9 0) (mkIntrvl 5 (-1))
      `shouldBe` True
    it "(startedBy <|> overlappedBy) Interval (0, 9) Interval (0, 4) is True"
      $          (startedBy <|> overlappedBy) (mkIntrvl 9 0) (mkIntrvl 4 0)
      `shouldBe` True
    it "(startedBy <|> overlappedBy) Interval (0, 9) Interval (-1, 9) is False"
      $          (startedBy <|> overlappedBy) (mkIntrvl 9 0) (mkIntrvl 10 (-1))
      `shouldBe` False
    it "disjoint x y same as explicit union of predicates"
      $          disjoint (mkIntrvl 2 0) (mkIntrvl 2 3)
      `shouldBe` (before <|> after <|> meets <|> metBy) (mkIntrvl 2 0)
                                                        (mkIntrvl 2 3)
    it "within x y same as explicit union of predicates"
      $          within (mkIntrvl 2 3) (mkIntrvl 2 3)
      `shouldBe` (starts <|> during <|> finishes <|> equals) (mkIntrvl 2 3)
                                                             (mkIntrvl 2 3)
    it "prop_compose holds" $ property (prop_compose @Int)

  describe "(.+.) tests" $ do
    it "join non-meeting intervals is Nothing"
      $          beginerval 2 (0 :: Int)
      .+.        beginerval 6 5
      `shouldBe` Nothing
    it "join meeting intervals is Just _"
      $          beginerval 2 (0 :: Int)
      .+.        beginerval 6 2
      `shouldBe` Just (beginerval 8 0)

    it "gap of disjoint intervals should be something"
      $ property (prop_combinable_gap_exists @Int)
    it "gap of disjoint intervals should be something"
      $ property (prop_combinable_gap_exists @Day)
    it "gap of disjoint intervals should be something"
      $ property (prop_combinable_gap_exists @UTCTime)

    it "gap of nondisjoint, nonmeeting intervals should be nothing"
      $ property (prop_combinable_nogap_exists @Int)
    it "gap of nondisjoint, nonmeeting intervals should be nothing"
      $ property (prop_combinable_nogap_exists @Day)

  describe "Interval Algebra relation unit tests for synonyms" $ do
    it "(0, 2) precedes (10, 12)"
      $          beginerval 2 (0 :: Int)
      `precedes` beginerval 2 10
      `shouldBe` True
    it "precedes matches before"
      $          beginerval 10 (0 :: Int)
      `precedes` beginerval 1  11
      `shouldBe` beginerval 10 (0 :: Int)
      `before`   beginerval 1  11
    it "(10, 12) precededBy (0, 2)"
      $          precededBy (beginerval 2 10) (beginerval 2 (0 :: Int))
      `shouldBe` True
    it "precededBy matches after"
      $          precededBy (beginerval 1 11) (beginerval 10 (0 :: Int))
      `shouldBe` after (beginerval 1 11) (beginerval 10 (0 :: Int))
    it "concur matches notDdisjoint"
      $          concur (beginerval 1 11) (beginerval 10 (0 :: Int))
      `shouldBe` notDisjoint (beginerval 1 11) (beginerval 10 (0 :: Int))
    it "concur matches notDisjoint"
      $          concur (beginerval 1 0) (beginerval 10 (0 :: Int))
      `shouldBe` notDisjoint (beginerval 1 0) (beginerval 10 (0 :: Int))

