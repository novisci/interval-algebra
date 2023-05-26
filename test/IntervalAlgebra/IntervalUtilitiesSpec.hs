{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{- HLINT ignore -}
module IntervalAlgebra.IntervalUtilitiesSpec
  ( spec
  ) where

import           Control.Monad                     (liftM2)
import           Data.List                         (sort)
import           Data.Maybe                        (fromJust, isJust, isNothing)
import           Data.Set                          (Set, difference, fromList,
                                                    member, toList)
import qualified Data.Set                          (null)
import           Data.Time                         (Day, UTCTime)
import           IntervalAlgebra                   (Interval,
                                                    IntervalRelation (..),
                                                    Intervallic (..),
                                                    SizedIv (..), beginerval,
                                                    complement, converse,
                                                    disjointRelations, duration,
                                                    intervalRelations, moment,
                                                    predicate, rangeInterval,
                                                    safeInterval, starts,
                                                    strictWithinRelations,
                                                    withinRelations, (.+.),
                                                    (><))
import           IntervalAlgebra.Arbitrary         (arbitraryWithRelation)
import           IntervalAlgebra.IntervalUtilities (clip, combineIntervals,
                                                    combineIntervalsFromSorted,
                                                    durations, gaps, intersect,
                                                    relations)
import           IntervalAlgebra.PairedInterval    (PairedInterval, getPairData,
                                                    makePairedInterval,
                                                    trivialize)
import           Test.Hspec                        (Spec, describe, it,
                                                    shouldBe)
import           Test.Hspec.QuickCheck             (modifyMaxDiscardRatio,
                                                    modifyMaxSuccess)
import           Test.QuickCheck                   (Arbitrary (arbitrary, shrink),
                                                    Arbitrary1 (liftArbitrary),
                                                    Property,
                                                    Testable (property),
                                                    elements, listOf,
                                                    orderedList, resize,
                                                    sublistOf, suchThat, (===),
                                                    (==>))

-- Types for testing

-- SmallInterval is just to test properties for which events of interest are so
-- rare QuickCheck gives up, e.g. filterEquals
newtype SmallInterval
  = SmallInterval { unSmall :: Interval Int }
  deriving (Eq, Show)

instance Arbitrary SmallInterval where
  arbitrary = SmallInterval . beginerval 0 <$> elements [0 .. 10]

-- A "state" here is just used test formMeetingSequence
newtype Events a
  = Events { getEvents :: [StateEvent a] }
  deriving (Eq, Ord, Show)

newtype State
  = State [Bool]
  deriving (Eq, Show)

instance Semigroup State where
  State x <> State y = State $ zipWith (||) x y

instance Monoid State where
  mempty = State [False, False, False]

newtype StateEvent a = MkEvent { getEvent :: PairedInterval State a }
   deriving (Eq, Ord, Show)

unEvents :: [StateEvent a] -> [PairedInterval State a]
unEvents = fmap getEvent

-- Type for checking arbitraryWithRelation
-- A target and reference pair, where targetInterval satisfies at least one of
-- refRelations relative to refInterval
data IntervalReferenced = IntervalReferenced
  { refInterval    :: Interval Int
  , refRelations   :: Set IntervalRelation
  , targetInterval :: Maybe (Interval Int)
  }
  deriving (Eq, Show)


mkEv :: (SizedIv (Interval a), Ord a, Ord (Moment (Interval a))) => (a, a) -> b -> PairedInterval b a
mkEv i s = makePairedInterval s (safeInterval i)

instance Arbitrary State where
  arbitrary = State <$> suchThat (listOf arbitrary) (\x -> length x == 3)

-- SmallInterval again to address issue of generating from too large a possible
-- range of intervals
instance Arbitrary (StateEvent Int) where
  arbitrary = liftM2 (\x y -> MkEvent $ makePairedInterval x y)
                     arbitrary
                     (unSmall <$> arbitrary)

instance Arbitrary (Events Int) where
  arbitrary = Events <$> orderedList

-- restricted refIv to decrease rareness causing quickcheck to quit
instance Arbitrary IntervalReferenced where
  arbitrary = do
    refIv <- liftM2 beginerval (elements [1 .. 3]) (elements [0 .. 3])
    rels  <- fromList <$> sublistOf (toList intervalRelations)
    iv    <- arbitraryWithRelation refIv rels
    pure $ IntervalReferenced refIv rels iv

-- Testing functions
checkSeqStates :: (Intervallic i) => [i Int] -> Bool
checkSeqStates x = (length x > 1) || all (== Meets) (relations x)

-- Creation functions
iv :: Int -> Int -> Interval Int
iv = beginerval

evpi :: Int -> Int -> [Bool] -> StateEvent Int
evpi i j s = MkEvent $ makePairedInterval (State s) (beginerval i j)

-- Test cases
containmentInt :: Interval Int
containmentInt = iv (10 :: Int) (0 :: Int)

noncontainmentInt :: Interval Int
noncontainmentInt = iv 6 4

anotherInt :: Interval Int
anotherInt = iv 5 (15 :: Int)

gapInt :: Interval Int
gapInt = iv 5 (10 :: Int)

meets1 :: [Interval Int]
meets1 = [iv 2 0, iv 2 2]

meets2 :: [Interval Int]
meets2 = [iv 2 0, iv 2 2, iv 10 4, iv 2 14]

meets3 :: [PairedInterval Int Int]
meets3 = map (uncurry makePairedInterval)
             [(5, iv 2 0), (5, iv 2 2), (9, iv 10 4), (10, iv 2 14)]

meets3eq :: [PairedInterval Int Int]
meets3eq =
  map (uncurry makePairedInterval) [(5, iv 4 0), (9, iv 10 4), (10, iv 2 14)]

c0in :: [StateEvent Int]
c0in =
  [ evpi 9 1 [True, False, False]
  , evpi 8 2 [True, False, False]
  , evpi 3 5 [False, True, False]
  , evpi 3 6 [False, True, False]
  ]
c0out :: [StateEvent Int]
c0out =
  [ evpi 4 1 [True, False, False]
  , evpi 4 5 [True, True, False]
  , evpi 1 9 [True, False, False]
  ]

c1in :: [StateEvent Int]
c1in = [evpi 4 1 [True, False, False], evpi 4 6 [False, True, False]]
c1out :: [StateEvent Int]
c1out =
  [ evpi 4 1 [True, False, False]
  , evpi 1 5 [False, False, False]
  , evpi 4 6 [False, True, False]
  ]

c2in :: [StateEvent Int]
c2in = [evpi 4 1 [True, False, False], evpi 5 5 [False, True, False]]
c2out :: [StateEvent Int]
c2out = [evpi 4 1 [True, False, False], evpi 5 5 [False, True, False]]

c3in :: [StateEvent Int]
c3in = [evpi 4 1 [True, False, False], evpi 6 4 [False, True, False]]
c3out :: [StateEvent Int]
c3out =
  [ evpi 3 1 [True, False, False]
  , evpi 1 4 [True, True, False]
  , evpi 5 5 [False, True, False]
  ]

c4in :: [StateEvent Int]
c4in =
  [ evpi 1 (-1) [True, True, False]
  , evpi 1 3    [True, False, True]
  , evpi 1 3    [False, False, False]
  ]
c4out :: [StateEvent Int]
c4out =
  [ evpi 1 (-1) [True, True, False]
  , evpi 3 0    [False, False, False]
  , evpi 1 3    [True, False, True]
  ]



c5in :: [PairedInterval State Int]
c5in =
  [ mkEv (-63, 21) (State [False, True, True])
  , mkEv (-56, 20) (State [True, True, True])
  , mkEv (1  , 41) (State [False, True, False])
  , mkEv (11 , 34) (State [True, False, True])
  , mkEv (27 , 28) (State [False, True, True])
  ]

c5out :: [PairedInterval State Int]
c5out =
  [ mkEv (-63, -56) (State [False, True, True])
  , mkEv (-56, 34)  (State [True, True, True])
  , mkEv (34 , 41)  (State [False, True, False])
  ]

-- Properties

-- arbitraryWithRelation props
-- 'tautology' because this repeats the logic of arbitraryWithRelation
prop_withRelation_tautology :: IntervalReferenced -> Bool
prop_withRelation_tautology ir
  | isEnclose && isMom = isNothing iv
  | otherwise          = isJust iv && predicate rels refIv (fromJust iv)
 where
  refIv = refInterval ir
  iv    = targetInterval ir
  rels  = refRelations ir
  isEnclose =
    Data.Set.null $ Data.Set.difference rels (converse strictWithinRelations)
  isMom = duration refIv == moment @(Interval Int)


-- Check that the only relation remaining after applying a function is Before
prop_before
  :: (SizedIv (Interval a), Ord a) => ([Interval a] -> [Interval a]) -> [Interval a] -> Property
prop_before f x = relations ci === replicate (length ci - 1) Before
  where ci = f (sort x)

prop_combineIntervals1 :: (SizedIv (Interval a), Ord a, Show a, Eq a) => [Interval a] -> Property
prop_combineIntervals1 = prop_before combineIntervals

prop_gaps1 :: (SizedIv (Interval a), Ord a, Ord (Moment (Interval a))) => [Interval a] -> Property
prop_gaps1 = prop_before gaps

prop_filtration :: (SizedIv (Interval a), Ord a) =>
   (Interval a ->  [Interval a] -> [Interval a])
   -> Set IntervalRelation
   -> Interval a
   -> [Interval a]
   -> Property
prop_filtration fltr s x l =
   not (null res) ==> and (fmap (predicate s x) res) === True
  where res = fltr x l

prop_clip_intersect
  :: (Show a, Ord a, SizedIv (Interval a), Ord (Moment (Interval a)))
  => Interval a
  -> Interval a
  -> Property
prop_clip_intersect x y = clip x y === intersect (min x y) (max x y)

-- RUNNER

spec :: Spec
spec = do
  describe "gaps tests" $ modifyMaxSuccess (* 10) $ do
    it "no gaps in containmentInt and noncontainmentInt"
      $          gaps [containmentInt, noncontainmentInt]
      `shouldBe` []
    it "no gaps in containmentInt" $ gaps [containmentInt] `shouldBe` []
    it "single gap between containmentInt and anotherInt"
      $          gaps [containmentInt, anotherInt]
      `shouldBe` [gapInt]
    it "after gaps, only relation should be Before" $ property (prop_gaps1 @Int)

  describe "durations unit tests" $ do
    it "durations of containmentInt is 10"
      $          durations [containmentInt]
      `shouldBe` [10]
    it "durations of empty list is empty list"
      $          durations ([] :: [Interval Int])
      `shouldBe` []
    it "durations of [containmentInt, anotherInt] is [10, 5]"
      $          durations [containmentInt, anotherInt]
      `shouldBe` [10, 5]

  describe "clip tests" $ do
    it "clip disjoint should be Nothing"
      $          clip containmentInt gapInt
      `shouldBe` Nothing
    it "clip Interval (4, 10) Interval (0, 10) should be Interval (4, 10)"
      $          clip noncontainmentInt containmentInt
      `shouldBe` Just (iv 6 4)
    it "clip x y === intersect sort x y " $ property (prop_clip_intersect @Int)

  describe "relations tests" $ do
    it
        "relations [(0, 10), (4, 10), (10, 15), (15, 20)] == [FinishedBy, Meets, Meets]"
      $ relations [containmentInt, noncontainmentInt, gapInt, anotherInt]
      `shouldBe` [FinishedBy, Meets, Meets]
    it "relations of [] shouldBe []"
      $          relations ([] :: [Interval Int])
      `shouldBe` []
    it "relations of singleton shouldBe []"
      $          relations [containmentInt]
      `shouldBe` []
    it "length of relations result should be 1 less then length of input"
      $ property
          (\x ->
            not (null x)
              ==> length (relations x)
              === length (x :: [Interval Int])
              -   1
          )

  describe "intersection tests" $ do
    it "intersection of (0, 2) (2, 4) should be Nothing"
      $          intersect (iv 2 0) (iv 2 2)
      `shouldBe` Nothing
    it "intersection of (0, 2) (3, 4) should be Nothing"
      $          intersect (iv 2 0) (iv 1 3)
      `shouldBe` Nothing
    it "intersection of (2, 4) (0, 2) should be Nothing"
      $          intersect (iv 2 2) (iv 2 0)
      `shouldBe` Nothing

  describe "intersection tests" $ do
    it "intersection of (0, 2) (2, 4) should be Nothing"
      $          intersect (iv 2 0) (iv 2 2)
      `shouldBe` Nothing
    it "intersection of (0, 2) (3, 4) should be Nothing"
      $          intersect (iv 2 0) (iv 1 3)
      `shouldBe` Nothing
    it "intersection of (2, 4) (0, 2) should be Nothing"
      $          intersect (iv 2 2) (iv 2 0)
      `shouldBe` Nothing
    it "intersection of (0, 2) (1, 3) should be Just (1, 2)"
      $          intersect (iv 2 0) (iv 2 1)
      `shouldBe` Just (iv 1 1)
    it "intersection of (0, 2) (-1, 3) should be Just (0, 2)"
      $          intersect (iv 2 0) (iv 4 (-1))
      `shouldBe` Just (iv 2 0)
    it "intersection of (0, 2) (0, 2) should be Just (0, 2)"
      $          intersect (iv 2 0) (iv 2 0)
      `shouldBe` Just (iv 2 0)
    it "intersection of (0, 2) (-1, 1) should be Just (0, 1)"
      $          intersect (iv 2 0) (iv 2 (-1))
      `shouldBe` Just (iv 1 0)
    it "intersection of (0, 3) (1, 2) should be Just (1, 2)"
      $          intersect (iv 3 0) (iv 1 1)
      `shouldBe` Just (iv 1 1)
  describe "rangeInterval unit tests" $ do
    it "range of empty list returns Nothing"
      $          rangeInterval ([] :: [Interval Int])
      `shouldBe` Nothing
    it "rangeInterval returns the containing interval"
      $          rangeInterval [beginerval 0 (1 :: Int), beginerval 3 (-1)]
      `shouldBe` (Just $ beginerval 3 (-1))
    it "disjoint intervals"
      $          rangeInterval [beginerval 10 (1 :: Int), beginerval 1 (-1)]
      `shouldBe` (Just $ beginerval 12 (-1))
    it "order of list does not matter"
      $          rangeInterval [beginerval 10 (1 :: Int), beginerval 1 (-1)]
      `shouldBe` rangeInterval [beginerval 1 (-1), beginerval 10 (1 :: Int)]
    it "works on Right"
      $          rangeInterval (Right $ beginerval 10 (1 :: Int))
      `shouldBe` (Just $ beginerval 10 (1 :: Int))
    it "Left variant returns Nothing"
      $          rangeInterval (Left $ beginerval 10 (1 :: Int))
      `shouldBe` (Nothing :: Maybe (Interval Int))
  describe "combineIntervals unit tests" $ do
    it "noncontainmentInt combined into containmentInt"
      $          combineIntervals [containmentInt, noncontainmentInt]
      `shouldBe` [containmentInt]
    it "noncontainmentInt combined into containmentInt; anotherInt unchanged"
      $ combineIntervals [containmentInt, noncontainmentInt, anotherInt]
      `shouldBe` [containmentInt, anotherInt]
    it "idempotency of containmentInt"
      $          combineIntervals [containmentInt]
      `shouldBe` [containmentInt]
    it "idempotency of noncontainmentInt"
      $          combineIntervals [noncontainmentInt]
      `shouldBe` [noncontainmentInt]
    it "combineIntervals [] should be []"
      $          combineIntervals ([] :: [Interval Int])
      `shouldBe` []
    it "combineIntervals works on sorted intervals"
      $          combineIntervals [iv 10 0, iv 5 2, iv 2 10, iv 2 13]
      `shouldBe` [iv 12 0, iv 2 13]
    it "combineIntervalsFromSorted works on sorted intervals"
      $          combineIntervalsFromSorted [iv 10 0, iv 5 2, iv 2 10, iv 2 13]
      `shouldBe` [iv 12 0, iv 2 13]
    it "combineIntervals works on unsorted intervals"
      $          combineIntervals [iv 2 13, iv 10 0, iv 2 10, iv 5 2]
      `shouldBe` [iv 12 0, iv 2 13]

  describe "combineIntervals property tests" $ modifyMaxSuccess (* 10) $ do
    it "after combining, only relation should be Before"
      $ property (prop_combineIntervals1 @Int)
    it "after combining, only relation should be Before"
      $ property (prop_combineIntervals1 @Day)
    it "after combining, only relation should be Before"
      $ property (prop_combineIntervals1 @UTCTime)
