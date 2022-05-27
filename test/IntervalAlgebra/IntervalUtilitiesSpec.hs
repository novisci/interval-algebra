{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications  #-}
{- HLINT ignore -}
module IntervalAlgebra.IntervalUtilitiesSpec
  ( spec
  ) where

import           Control.Monad                  ( liftM2 )
import           Data.List                      ( sort )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , isNothing
                                                )
import           Data.Set                       ( Set
                                                , difference
                                                , fromList
                                                , member
                                                , toList
                                                )
import qualified Data.Set                       ( null )
import           Data.Time                      ( Day
                                                , UTCTime
                                                )
import           IntervalAlgebra                ( Interval
                                                , IntervalCombinable(..)
                                                , IntervalRelation(..)
                                                , IntervalSizeable
                                                , Intervallic(..)
                                                , beginerval
                                                , complement
                                                , converse
                                                , disjointRelations
                                                , duration
                                                , intervalRelations
                                                , moment
                                                , predicate
                                                , rangeInterval
                                                , safeInterval
                                                , starts
                                                , strictWithinRelations
                                                , withinRelations
                                                )
import           IntervalAlgebra.Arbitrary      ( arbitraryWithRelation )
import           IntervalAlgebra.IntervalUtilities
                                                ( clip
                                                , combineIntervals
                                                , durations
                                                , filterAfter
                                                , filterBefore
                                                , filterConcur
                                                , filterContains
                                                , filterDisjoint
                                                , filterDuring
                                                , filterEnclose
                                                , filterEnclosedBy
                                                , filterEquals
                                                , filterFinishedBy
                                                , filterFinishes
                                                , filterMeets
                                                , filterMetBy
                                                , filterNotDisjoint
                                                , filterOverlappedBy
                                                , filterOverlaps
                                                , filterStartedBy
                                                , filterStarts
                                                , filterWithin
                                                , foldMeetingSafe
                                                , formMeetingSequence
                                                , gaps
                                                , gapsL
                                                , gapsWithin
                                                , intersect
                                                , nothingIfAll
                                                , nothingIfAny
                                                , nothingIfNone
                                                , relationsL
                                                )
import           IntervalAlgebra.PairedInterval ( PairedInterval
                                                , getPairData
                                                , makePairedInterval
                                                , trivialize
                                                )
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldBe
                                                )
import           Test.Hspec.QuickCheck          ( modifyMaxDiscardRatio
                                                , modifyMaxSuccess
                                                )
import           Test.QuickCheck                ( (===)
                                                , (==>)
                                                , Arbitrary(arbitrary, shrink)
                                                , Arbitrary1(liftArbitrary)
                                                , Property
                                                , Testable(property)
                                                , elements
                                                , listOf
                                                , orderedList
                                                , resize
                                                , sublistOf
                                                , suchThat
                                                )
import           Witherable                     ( Filterable )

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


mkEv :: IntervalSizeable a a => (a, a) -> b -> PairedInterval b a
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
checkSeqStates :: (Intervallic i Int) => [i Int] -> Bool
checkSeqStates x = (length x > 1) || all (== Meets) (relationsL x)

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
  isMom = duration refIv == moment @Int


-- Check that the only relation remaining after applying a function is Before
prop_before
  :: (Ord a) => ([Interval a] -> [Interval a]) -> [Interval a] -> Property
prop_before f x = relationsL ci === replicate (length ci - 1) Before
  where ci = f (sort x)

prop_combineIntervals1 :: (Ord a, Show a, Eq a) => [Interval a] -> Property
prop_combineIntervals1 = prop_before combineIntervals

prop_gaps1 :: (Ord a) => [Interval a] -> Property
prop_gaps1 = prop_before gapsL

-- In the case that that the input is not null, then
-- * all relationsL should be `Meets` after formMeetingSequence
prop_formMeetingSequence0 :: Events Int -> Property
prop_formMeetingSequence0 x =
  not (null es)
    ==> all (== Meets) (relationsL $ formMeetingSequence (unEvents es))
    === True
  where es = getEvents x

-- In the case that the input has
-- *     at least one Before relation between consequent pairs
-- * AND does not have any empty states
--
-- THEN the number empty states in the output should smaller than or equal to
--      the number before relationsL in the output
prop_formMeetingSequence1 :: Events Int -> Property
prop_formMeetingSequence1 x =
  (beforeCount > 0 && not
      (any (\x -> getPairData x == State [False, False, False])
           (unEvents $ getEvents x)
      )
    )
    ==> beforeCount
    >=  emptyCount
 where
  res         = formMeetingSequence (unEvents $ getEvents x)
  beforeCount = lengthWhen (== Before) (relationsL (unEvents $ getEvents x))
  emptyCount  = lengthWhen (\x -> getPairData x == mempty) res
  lengthWhen f = length . filter f

-- Check that formMeetingSequence doesn't return an empty list unless input is
-- empty.
prop_formMeetingSequence2 :: Events Int -> Property
prop_formMeetingSequence2 x = not (null $ getEvents x) ==> not $ null res
  where res = formMeetingSequence (unEvents $ getEvents x)

class ( Ord a ) => FiltrationProperties a  where
   prop_filtration ::
      (Interval a ->  [Interval a] -> [Interval a])
      -> Set IntervalRelation
      -> Interval a
      -> [Interval a]
      -> Property
   prop_filtration fltr s x l =
      not (null res) ==> and (fmap (predicate s x) res) === True
     where res = fltr x l

   prop_filterOverlaps :: Interval a
      -> [Interval a]
      -> Property
   prop_filterOverlaps = prop_filtration filterOverlaps (fromList [Overlaps])

   prop_filterOverlappedBy :: Interval a
      -> [Interval a]
      -> Property
   prop_filterOverlappedBy = prop_filtration filterOverlappedBy (fromList [OverlappedBy])

   prop_filterBefore :: Interval a
      -> [Interval a]
      -> Property
   prop_filterBefore = prop_filtration filterBefore (fromList [Before])

   prop_filterAfter :: Interval a
      -> [Interval a]
      -> Property
   prop_filterAfter = prop_filtration filterAfter (fromList [After])

   prop_filterStarts :: Interval a
      -> [Interval a]
      -> Property
   prop_filterStarts = prop_filtration filterStarts (fromList [Starts])

   prop_filterStartedBy :: Interval a
      -> [Interval a]
      -> Property
   prop_filterStartedBy = prop_filtration filterStartedBy (fromList [StartedBy])

   prop_filterFinishes :: Interval a
      -> [Interval a]
      -> Property
   prop_filterFinishes = prop_filtration filterFinishes (fromList [Finishes])

   prop_filterFinishedBy :: Interval a
      -> [Interval a]
      -> Property
   prop_filterFinishedBy = prop_filtration filterFinishedBy (fromList [FinishedBy])

   prop_filterMeets :: Interval a
      -> [Interval a]
      -> Property
   prop_filterMeets = prop_filtration filterMeets (fromList [Meets])

   prop_filterMetBy :: Interval a
      -> [Interval a]
      -> Property
   prop_filterMetBy = prop_filtration filterMetBy (fromList [MetBy])

   prop_filterDuring :: Interval a
      -> [Interval a]
      -> Property
   prop_filterDuring = prop_filtration filterDuring (fromList [During])

   prop_filterContains :: Interval a
      -> [Interval a]
      -> Property
   prop_filterContains = prop_filtration filterContains (fromList [Contains])

   prop_filterEquals :: Interval a
      -> [Interval a]
      -> Property
   prop_filterEquals = prop_filtration filterEquals (fromList [Equals])

   prop_filterDisjoint :: Interval a
      -> [Interval a]
      -> Property
   prop_filterDisjoint = prop_filtration filterDisjoint disjointRelations

   prop_filterNotDisjoint :: Interval a
      -> [Interval a]
      -> Property
   prop_filterNotDisjoint = prop_filtration filterNotDisjoint (complement disjointRelations)

   prop_filterWithin :: Interval a
      -> [Interval a]
      -> Property
   prop_filterWithin = prop_filtration filterWithin withinRelations

   prop_filterEnclosedBy :: Interval a
      -> [Interval a]
      -> Property
   prop_filterEnclosedBy = prop_filtration filterEnclosedBy withinRelations

   prop_filterEnclose :: Interval a
      -> [Interval a]
      -> Property
   prop_filterEnclose = prop_filtration filterEnclose (converse withinRelations)

   prop_filterConcur :: Interval a
      -> [Interval a]
      -> Property
   prop_filterConcur = prop_filtration filterConcur (complement disjointRelations)

instance FiltrationProperties Int

prop_clip_intersect
  :: (Show a, Ord a, IntervalSizeable a b)
  => Interval a
  -> Interval a
  -> Property
prop_clip_intersect x y = clip x y === intersect (min x y) (max x y)

-- NOTE: use this instead of prop_filterEquals
prop_small_filterEquals :: SmallInterval -> [SmallInterval] -> Property
prop_small_filterEquals x l =
  not (null res) ==> and (fmap (predicate s i) res) === True
 where
  i   = unSmall x
  li  = map unSmall l
  res = filterEquals i li
  s   = fromList [Equals]

-- RUNNER

spec :: Spec
spec = do
  describe "gaps tests" $ modifyMaxSuccess (* 10) $ do
    it "no gaps in containmentInt and noncontainmentInt"
      $          gapsL [containmentInt, noncontainmentInt]
      `shouldBe` []
    it "no gaps in containmentInt" $ gapsL [containmentInt] `shouldBe` []
    it "single gap between containmentInt and anotherInt"
      $          gapsL [containmentInt, anotherInt]
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

  describe "relationsL tests" $ do
    it
        "relationsL [(0, 10), (4, 10), (10, 15), (15, 20)] == [FinishedBy, Meets, Meets]"
      $ relationsL [containmentInt, noncontainmentInt, gapInt, anotherInt]
      `shouldBe` [FinishedBy, Meets, Meets]
    it "relationsL of [] shouldBe []"
      $          relationsL ([] :: [Interval Int])
      `shouldBe` []
    it "relationsL of singleton shouldBe []"
      $          relationsL [containmentInt]
      `shouldBe` []
    it "length of relationsL result should be 1 less then length of input"
      $ property
          (\x ->
            not (null x)
              ==> length (relationsL x)
              === length (x :: [Interval Int])
              -   1
          )

  describe "gapsWithin tests" $ do
    it "gapsWithin (1, 10) [(0,5), (7,9), (12,15)] should be [(5,7), (9,10)]"
      $          gapsWithin (iv 9 1) [iv 5 0, iv 2 7, iv 3 12]
      `shouldBe` Just [iv 2 5, iv 1 9]
    it "gapsWithin (1, 10) [(-1, 0), (12,15)] should be [(5,7), (9,10)]"
      $          gapsWithin (iv 9 1) [iv 1 (-1), iv 3 12]
      `shouldBe` Nothing
    it "gapsWithin (0, 455) [(0, 730), (731, 762), (763, 793)]"
      $ gapsWithin (safeInterval (0 :: Int, 455))
                   (fmap safeInterval [(0, 730), (731, 762), (763, 793)])
      `shouldBe` Just []
    it "gapsWithin (1, 10) [] should be []"
      $          gapsWithin (iv 9 1) ([] :: [Interval a])
      `shouldBe` Nothing

  describe "emptyIf tests" $ do
    it "emptyIfNone (starts (3, 5)) [(3,4), (5,6)] should be empty"
      $          nothingIfNone (starts (iv 2 3)) [iv 1 3, iv 1 5]
      `shouldBe` Nothing
    it "emptyIfNone (starts (3, 5)) [(3,6), (5,6)] shoiuld be input"
      $          nothingIfNone (starts (iv 2 3)) [iv 3 3, iv 1 5]
      `shouldBe` Just [iv 3 3, iv 1 5]

  describe "filtration tests" $ modifyMaxDiscardRatio (* 2) $ do
    it "disjoint filter should filter out noncontainment"
      $          filterDisjoint containmentInt [noncontainmentInt, anotherInt]
      `shouldBe` [anotherInt]
    it "notDisjoint filter should keep noncontainment"
      $ filterNotDisjoint containmentInt [noncontainmentInt, anotherInt]
      `shouldBe` [noncontainmentInt]
    it "filterBefore property" $ property (prop_filterBefore @Int)
    it "filterAfter property" $ property (prop_filterAfter @Int)
    it "filterOverlaps property" $ property (prop_filterOverlaps @Int)
    it "filterOverlappedBy property" $ property (prop_filterOverlappedBy @Int)
    it "filterStarts property" $ property (prop_filterStarts @Int)
    it "filterStartedBy property" $ property (prop_filterStartedBy @Int)
    it "filterFinishes property" $ property (prop_filterFinishes @Int)
    it "filterFinishedBy property" $ property (prop_filterFinishedBy @Int)
    it "filterMeets property" $ property (prop_filterMeets @Int)
    it "filterMetBy property" $ property (prop_filterMetBy @Int)
    it "filterDuring property" $ property (prop_filterDuring @Int)
    it "filterContains property" $ property (prop_filterContains @Int)
    it "filterEquals property" $ property prop_small_filterEquals
    it "filterDisjoint property" $ property (prop_filterDisjoint @Int)
    it "filterNotDisjoint property" $ property (prop_filterNotDisjoint @Int)
    it "filterWithin property" $ property (prop_filterWithin @Int)
    it "filterConcur property" $ property (prop_filterConcur @Int)
    it "filterEnclose property" $ property (prop_filterEnclose @Int)
    it "filterEnclosedBy property" $ property (prop_filterEnclosedBy @Int)

  describe "nothingIf unit tests" $ do
    it "nothing from nothingIfAll"
      $          nothingIfAll (starts (iv 2 3)) [iv 3 3, iv 4 3]
      `shouldBe` Nothing
    it "something from nothingIfAll"
      $          nothingIfAll (starts (iv 2 3)) [iv 3 0, iv 4 3]
      `shouldBe` Just [iv 3 0, iv 4 3]
    it "nothing from nothingIfAny"
      $          nothingIfAny (starts (iv 2 3)) [iv 3 3, iv 1 5]
      `shouldBe` Nothing
    it "something from nothingIfAny"
      $          nothingIfAny (starts (iv 2 3)) [iv 3 1, iv 1 5]
      `shouldBe` Just [iv 3 1, iv 1 5]

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
    it "combineIntervals [(0, 10), (2, 7), (10, 12), (13, 15)]"
      $          combineIntervals [iv 10 0, iv 5 2, iv 2 10, iv 2 13]
      `shouldBe` [iv 12 0, iv 2 13]

  describe "combineIntervals property tests" $ modifyMaxSuccess (* 10) $ do
    it "after combining, only relation should be Before"
      $ property (prop_combineIntervals1 @Int)
    it "after combining, only relation should be Before"
      $ property (prop_combineIntervals1 @Day)
    it "after combining, only relation should be Before"
      $ property (prop_combineIntervals1 @UTCTime)

  describe "foldMeets unit tests" $ do
    it "foldMeetingSafe meets1"
      $          foldMeetingSafe (trivialize meets1)
      `shouldBe` trivialize [iv 4 0]
    it "foldMeetingSafe meets2"
      $          foldMeetingSafe (trivialize meets2)
      `shouldBe` trivialize [iv 16 0]
    it "foldMeetingSafe meets3" $ foldMeetingSafe meets3 `shouldBe` meets3eq

  describe "formMeetingSequence unit tests" $ do
    it "formMeetingSequence unit test 0"
      $          formMeetingSequence (unEvents c0in)
      `shouldBe` unEvents c0out
    it "formMeetingSequence unit test 1"
      $          formMeetingSequence (unEvents c1in)
      `shouldBe` unEvents c1out
    it "formMeetingSequence unit test 2"
      $          formMeetingSequence (unEvents c2in)
      `shouldBe` unEvents c2out
    it "formMeetingSequence unit test 3"
      $          formMeetingSequence (unEvents c3in)
      `shouldBe` unEvents c3out
    it "formMeetingSequence unit test 4"
      $          formMeetingSequence (unEvents c4in)
      `shouldBe` unEvents c4out
    it "formMeetingSequence unit test 5"
      $          formMeetingSequence c5in
      `shouldBe` c5out
    it "formMeetingSequence unit test 6"
      $          formMeetingSequence ([] :: [PairedInterval State Int])
      `shouldBe` []

  describe "formMeetingSequence property tests" $ modifyMaxSuccess (* 2) $ do
    it "prop_formMeetingSequence0" $ property prop_formMeetingSequence0
     -- 2022-05-18 - BS
    -- Commmenting out this test as the execution of the test suite blows up
    -- when this property check is included.
    -- TODO: consider whether this check is worthwhile.
    -- it "prop_formMeetingSequence1" $ property prop_formMeetingSequence1
    it "prop_formMeetingSequence2" $ property prop_formMeetingSequence2

  -- describe "arbitraryWithRelation property tests" $ do
  --   it "prop_withRelation_tautology" $ property prop_withRelation_tautology
