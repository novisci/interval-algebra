{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
module IntervalAlgebra.IntervalUtilitiesSpec (spec) where

import Test.Hspec.QuickCheck              ( modifyMaxSuccess )
import Test.Hspec                         ( Spec
                                          , it, shouldBe, describe, pending, xcontext )
import Test.QuickCheck                    ( Property
                                          , Testable(property)
                                          , Arbitrary(arbitrary, shrink)
                                          , suchThat
                                          , (===), (==>)
                                          , Arbitrary1 (liftArbitrary), listOf, orderedList)
import Data.List                          (sort)
import IntervalAlgebra.Arbitrary          ( )
import IntervalAlgebra                    ( Interval
                                          , Intervallic(..)
                                          , IntervalCombinable(..)
                                          , IntervalRelation (..)
                                          , beginerval
                                          , IntervalSizeable
                                          , starts )
import IntervalAlgebra.IntervalUtilities  ( gaps
                                          , durations
                                          , intersect
                                          , relations
                                          , clip
                                          , gapsWithin
                                          , nothingIfNone
                                          , filterDisjoint
                                          , filterNotDisjoint
                                          , combineIntervals
                                          , foldMeetingSafe
                                          , formMeetingSequence )
import IntervalAlgebra.PairedInterval     ( trivialize
                                          , makePairedInterval
                                          , PairedInterval, getPairData )
import Control.Monad                      ( liftM2 )
import Data.Foldable

-- Types for testing

-- A "state" here is just used test formMeetingSequence 
newtype Events a = Events {getEvents :: [PairedInterval State a]}
   deriving (Eq, Show, Ord)
newtype State = State [Bool] deriving (Show, Eq)
instance Semigroup State where
     State x <> State y = State $ zipWith (||) x y
instance Monoid State where
    mempty = State [False, False, False]
type StateEvent a = PairedInterval State a

instance Arbitrary State where
   arbitrary =  State <$> suchThat (listOf arbitrary) (\x -> length x == 3)

instance Arbitrary (PairedInterval State Int) where
   arbitrary = liftM2 makePairedInterval arbitrary arbitrary

instance Arbitrary (Events Int) where
   arbitrary = Events <$> orderedList


-- Testing functions
checkSeqStates :: (Intervallic i Int)=> [i Int] -> Bool
checkSeqStates x = (length x > 1) || all (== Meets) (relations x)

-- Creation functions
iv :: Int -> Int -> Interval Int
iv = beginerval

evpi :: Int -> Int -> [Bool] -> StateEvent Int
evpi i j s = makePairedInterval (State s) (beginerval i j)

-- Test cases
containmentInt :: Interval Int
containmentInt = iv (10 :: Int) (0 :: Int)

noncontainmentInt :: Interval Int
noncontainmentInt = iv  6 4

anotherInt :: Interval Int
anotherInt = iv 5 (15 :: Int)

gapInt :: Interval Int
gapInt = iv 5 (10 :: Int)

meets1 :: [Interval Int]
meets1 = [iv 2 0, iv 2 2]

meets2 :: [Interval Int]
meets2 = [iv 2 0, iv 2 2, iv 10 4, iv 2 14]

meets3 :: [PairedInterval Int Int]
meets3 = map (uncurry makePairedInterval) [
      (5,  iv 2 0)
    , (5,  iv 2 2)
    , (9,  iv 10 4)
    , (10, iv 2 14)]

meets3eq :: [PairedInterval Int Int]
meets3eq = map (uncurry makePairedInterval) [
      (5,  iv 4 0)
    , (9,  iv 10 4)
    , (10, iv 2 14)]

c0in :: [StateEvent Int]
c0in =
  [ evpi 9 1  [True, False, False]
  , evpi 8 2  [True, False, False]
  , evpi 3 5  [False, True, False]
  , evpi 3 6  [False, True, False]]
c0out :: [StateEvent Int]
c0out =
  [ evpi 4 1  [True, False, False]
  , evpi 4 5  [True, True, False]
  , evpi 1 9  [True, False, False]]

c1in :: [StateEvent Int]
c1in =
  [ evpi 4 1  [True, False, False ]
  , evpi 4 6  [False, True, False ]]
c1out :: [StateEvent Int]
c1out =
  [ evpi 4 1  [True, False, False]
  , evpi 1 5  [False, False, False]
  , evpi 4 6  [False, True, False]]

c2in :: [StateEvent Int]
c2in =
  [ evpi 4 1  [True, False, False ]
  , evpi 5 5  [False, True, False ]]
c2out :: [StateEvent Int]
c2out =
  [ evpi 4 1  [True, False, False]
  , evpi 5 5  [False, True, False]]

c3in :: [StateEvent Int]
c3in =
  [ evpi 4 1  [True, False, False ]
  , evpi 6 4  [False, True, False ]]
c3out :: [StateEvent Int]
c3out =
  [ evpi 3 1  [True, False, False]
  , evpi 1 4  [True, True, False]
  , evpi 5 5  [False, True, False]]

c4in :: [StateEvent Int]
c4in =
  [ evpi 1 (-1)  [True, True, False ]
  , evpi 1 3     [True, False, True ]
  , evpi 1 3     [False, False, False]]
c4out :: [StateEvent Int]
c4out =
  [ evpi 1 (-1)  [True, True, False ]
  , evpi 3 0     [False, False, False]
  , evpi 1 3     [True, False, True ]]

-- Properties

-- Check that the only relation remaining after applying a function is Before
prop_before:: (Ord a)=>
      ([Interval a] -> [Interval a])
   -> [Interval a]
   -> Property
prop_before f x = relations ci === replicate (length ci - 1) Before
      where ci = f (sort x)

prop_combineIntervals1:: (Ord a, Show a, Eq a)=>
     [Interval a]
   -> Property
prop_combineIntervals1 = prop_before combineIntervals

prop_gaps1:: (Ord a)=>
     [Interval a]
   -> Property
prop_gaps1 = prop_before gaps

-- In the case that that the input has
-- *     at least one Before relation between consequent pairs
-- * AND does not have any empty states
--
-- THEN the number empty states in the output should smaller than or equal to
--      the number before relations in the output 
prop_formMeetingSequence1::
     Events Int
   -> Property
prop_formMeetingSequence1 x =
   ( beforeCount > 0 &&
     not (any (\x -> getPairData x == State [False, False, False]) (getEvents x))
   ) ==> beforeCount >= emptyCount
   where res = formMeetingSequence (getEvents x)
         beforeCount = lengthWhen (== Before) (relations (getEvents x))
         emptyCount  = lengthWhen (\x -> getPairData x == mempty ) res
         lengthWhen f x = length $ filter f x

-- Check that formMeetingSequence doesn't return an empty list unless input is 
-- empty.
prop_formMeetingSequence2::
     Events Int
   -> Property
prop_formMeetingSequence2 x = not (null $ getEvents x) ==> not $ null res
   where res = formMeetingSequence (getEvents x)

spec :: Spec
spec = do
   describe "gaps tests" $
    modifyMaxSuccess (*10) $
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
         it "durations of empty list is empty list" $
            durations  ([] :: [Interval Int])  `shouldBe` []
         it "durations of [containmentInt, anotherInt] is [10, 5]" $
            durations [containmentInt, anotherInt] `shouldBe` [10, 5]

   describe "clip tests" $
      do
         it "clip disjoint should be Nothing" $
           clip containmentInt gapInt `shouldBe` Nothing
         it "clip Interval (4, 10) Interval (0, 10) should be Interval (4, 10)" $
           clip noncontainmentInt containmentInt `shouldBe`
             Just (iv 6 4)
         it "clip x y === intersect sort x y " pending

   describe "relations tests" $
         do
            it "relations [(0, 10), (4, 10), (10, 15), (15, 20)] == [FinishedBy, Meets, Meets]" $
               relations [containmentInt, noncontainmentInt, gapInt, anotherInt] `shouldBe`
                  [FinishedBy, Meets, Meets]
            it "relations of [] shouldBe []" $
               relations ([] :: [Interval Int]) `shouldBe` []
            it "relations of singleton shouldBe []" $
               relations [containmentInt] `shouldBe` []
            it "more relations tests" pending

   describe "gapsWithin tests" $
      do
         it "gapsWithin (1, 10) [(0,5), (7,9), (12,15)] should be [(5,7), (9,10)]" $
            gapsWithin (iv 9 1) [iv 5 0, iv 2 7, iv 3 12]
               `shouldBe` Just [iv 2 5, iv 1 9]
         it "gapsWithin (1, 10) [] should be []" $
             gapsWithin (iv 9 1) ([] :: [Interval a]) `shouldBe` Nothing
         it "more gapsWithin tests" pending

   describe "emptyIf tests" $
      do
         it "emptyIfNone (starts (3, 5)) [(3,4), (5,6)] should be empty" $
            nothingIfNone (starts (iv 2 3)) [iv 1 3, iv 1 5]
               `shouldBe` Nothing
         it "emptyIfNone (starts (3, 5)) [(3,6), (5,6)] shoiuld be input" $
            nothingIfNone (starts (iv 2 3)) [iv 3 3, iv 1 5]
               `shouldBe` Just [ iv 3 3, iv 1 5]
         it "more emptyif tests" pending

   describe "filtration tests" $
      do
         it "disjoint filter should filter out noncontainment" $
            filterDisjoint containmentInt [noncontainmentInt, anotherInt]
               `shouldBe` [anotherInt]
         it "notDisjoint filter should keep noncontainment" $
            filterNotDisjoint containmentInt [noncontainmentInt, anotherInt]
               `shouldBe` [noncontainmentInt]

   describe "intersection tests" $
      do
         it "intersection of (0, 2) (2, 4) should be Nothing" $
            intersect (iv 2 0) (iv 2 2)    `shouldBe` Nothing
         it "intersection of (0, 2) (3, 4) should be Nothing" $
            intersect (iv 2 0) (iv 1 3)    `shouldBe` Nothing
         it "intersection of (2, 4) (0, 2) should be Nothing" $
            intersect (iv 2 2) (iv 2 0)    `shouldBe` Nothing
         it "intersection of (0, 2) (1, 3) should be Just (1, 2)" $
            intersect (iv 2 0) (iv 2 1)    `shouldBe` Just (iv 1 1)
         it "intersection of (0, 2) (-1, 3) should be Just (0, 2)" $
            intersect (iv 2 0) (iv 4 (-1)) `shouldBe` Just (iv 2 0)
         it "intersection of (0, 2) (0, 2) should be Just (0, 2)" $
            intersect (iv 2 0) (iv 2 0)    `shouldBe` Just (iv 2 0)
         it "intersection of (0, 2) (-1, 1) should be Just (0, 1)" $
            intersect (iv 2 0) (iv 2 (-1)) `shouldBe` Just (iv 1 0)
         it "intersection of (0, 3) (1, 2) should be Just (1, 2)" $
            intersect (iv 3 0) (iv 1 1)    `shouldBe` Just (iv 1 1)

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
         it "combineIntervals [] should be []" $
               combineIntervals ([] :: [Interval Int]) `shouldBe` []
         it "combineIntervals [(0, 10), (2, 7), (10, 12), (13, 15)]" $
               combineIntervals [iv 10 0, iv 5 2, iv 2 10, iv 2 13]
                  `shouldBe` [iv 12 0, iv 2 13]

   describe "combineIntervals property tests" $
      modifyMaxSuccess (*10) $
      do
         it "after combining, only relation should be Before" $
               property ( prop_combineIntervals1 @Int)

   describe "foldMeets unit tests" $
      do
         it "foldMeetingSafe meets1" $
               foldMeetingSafe (trivialize meets1) `shouldBe`
                  trivialize [iv 4 0]
         it "foldMeetingSafe meets2" $
               foldMeetingSafe (trivialize meets2) `shouldBe`
                  trivialize [iv 16 0]
         it "foldMeetingSafe meets3" $
               foldMeetingSafe meets3 `shouldBe` meets3eq

   describe "formMeetingSequence unit tests" $
      do
         it "formMeetingSequence 0" $
            formMeetingSequence c0in `shouldBe` c0out
         it "formMeetingSequence 1"$
            formMeetingSequence c1in `shouldBe` c1out
         it "formMeetingSequence 2"$
            formMeetingSequence c2in `shouldBe` c2out
         it "formMeetingSequence 3"$
            formMeetingSequence c3in `shouldBe` c3out
         it "formMeetingSequence 4"$
            formMeetingSequence c4in `shouldBe` c4out

   describe "formMeetingSequence property tests" $
      do
          it "prop_formMeetingSequence1" $
            property prop_formMeetingSequence1
          it "prop_formMeetingSequence2" $
            property prop_formMeetingSequence2