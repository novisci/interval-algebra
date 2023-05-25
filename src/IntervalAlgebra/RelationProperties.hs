{-|
Module      : Interval Algebra Axioms
Description : Properties of Intervals
Copyright   : (c) NoviSci, Inc 2020-2022
                  TargetRWE, 2023
License     : BSD3
Maintainer  : bsaul@novisci.com 2020-2022, bbrown@targetrwe.com 2023

This module exports property-based tests for the axioms in section 1 of [Allen
and Hayes (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x).  The
notation below is that of the original paper.

This module is useful if creating a new instance of interval types that you
want to test.

-}
{- HLINT ignore -}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module IntervalAlgebra.RelationProperties where

import           Data.Maybe                        (fromJust, isJust, isNothing)
import           Data.Set                          (Set, disjointUnion,
                                                    fromList, member)
import           Data.Time                         as DT (Day, NominalDiffTime,
                                                          UTCTime)
import           IntervalAlgebra.Arbitrary
import           IntervalAlgebra.Core
import           IntervalAlgebra.IntervalUtilities ((.+.))
import           Test.QuickCheck                   (Arbitrary (arbitrary),
                                                    Property, (===), (==>))

allIArelations :: (SizedIv (Interval a), Ord a) => [ComparativePredicateOf1 (Interval a)]
allIArelations =
  [ equals
  , meets
  , metBy
  , before
  , after
  , starts
  , startedBy
  , finishes
  , finishedBy
  , overlaps
  , overlappedBy
  , during
  , contains
  ]

-- A collection of properties for the interval algebra. Some of these come from
-- figure 2 in  [Allen and Hayes
-- (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x).

-- | For any two pair of intervals exactly one 'IntervalRelation' should hold
prop_exclusiveRelations::  (SizedIv (Interval a), Ord a) => Interval a -> Interval a -> Property
prop_exclusiveRelations x y =
  (  1 == length (filter id $ map (\r -> r x y) allIArelations)) === True

-- | Given a set of interval relations and predicate function, test that the
-- predicate between two interval is equivalent to the relation of two intervals
-- being in the set of relations.
prop_predicate_unions :: (SizedIv (Interval a), Ord a) =>
      Set IntervalRelation
    -> ComparativePredicateOf2 (Interval a) (Interval a)
    -> Interval a
    -> Interval a
    -> Property
prop_predicate_unions s pred i0 i1 =
  pred i0 i1 === (relate i0 i1 `elem` s)

prop_IAbefore :: forall a. (SizedIv (Interval a), Ord a, Ord (Moment (Interval a))) => Interval a -> Interval a -> Property
prop_IAbefore i j =
  before i j ==> (i `meets` k) && (k `meets` j)
    where k = safeInterval (end i, begin j)

prop_IAstarts:: (SizedIv (Interval a), Ord a, Ord (Moment (Interval a))) => Interval a -> Interval a -> Property
prop_IAstarts i j
  | starts i j = (j == fromJust (i .+. k)) === True
  | otherwise     = starts i j === False
    where k = safeInterval (end i, end j)

prop_IAfinishes:: (SizedIv (Interval a), Ord a, Ord (Moment (Interval a))) => Interval a -> Interval a -> Property
prop_IAfinishes i j
  | finishes i j = (j == fromJust ( k .+. i)) === True
  | otherwise       = finishes i j === False
    where k = safeInterval (begin j, begin i)

prop_IAoverlaps:: forall a. (SizedIv (Interval a), Ord a, Ord (Moment (Interval a))) => Interval a -> Interval a -> Property
prop_IAoverlaps i j
  | overlaps i j = ((i == fromJust ( k .+. l )) &&
                      (j == fromJust ( l .+. m ))) === True
  | otherwise       = overlaps i j === False
    where k = safeInterval (begin i, begin j)
          l = safeInterval (begin j, end i)
          m = safeInterval (end i, end j)

prop_IAduring:: forall a. (SizedIv (Interval a), Ord a, Ord (Moment (Interval a))) => Interval a -> Interval a-> Property
prop_IAduring i j
  | during i j = (j == fromJust ( fromJust (k .+. i) .+. l)) === True
  | otherwise     = during i j === False
    where k = safeInterval (begin j, begin i)
          l = safeInterval (end i, end j)

prop_disjoint_predicate :: (SizedIv (Interval a), Ord a) =>
      Interval a
    -> Interval a
    -> Property
prop_disjoint_predicate = prop_predicate_unions disjointRelations disjoint

prop_notdisjoint_predicate :: (SizedIv (Interval a), Ord a) =>
      Interval a
    -> Interval a
    -> Property
prop_notdisjoint_predicate =
  prop_predicate_unions (complement disjointRelations) notDisjoint

prop_concur_predicate :: (SizedIv (Interval a), Ord a) =>
      Interval a
    -> Interval a
    -> Property
prop_concur_predicate =
  prop_predicate_unions (complement disjointRelations) concur

prop_within_predicate :: (SizedIv (Interval a), Ord a) =>
      Interval a
    -> Interval a
    -> Property
prop_within_predicate = prop_predicate_unions withinRelations within

prop_enclosedBy_predicate :: (SizedIv (Interval a), Ord a) =>
      Interval a
    -> Interval a
    -> Property
prop_enclosedBy_predicate = prop_predicate_unions withinRelations enclosedBy

prop_encloses_predicate :: (SizedIv (Interval a), Ord a) =>
      Interval a
    -> Interval a
    -> Property
prop_encloses_predicate = prop_predicate_unions (converse withinRelations) encloses
