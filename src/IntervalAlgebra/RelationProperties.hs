{-|
Module      : Interval Algebra Axioms
Description : Properties of Intervals
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

This module exports a single typeclass @IntervalAxioms@ which contains 
property-based tests for the axioms in section 1 of [Allen and Hayes (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x).
The notation below is that of the original paper.

This module is useful if creating a new instance of interval types that you want to test.

-}
{- HLINT ignore -}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module IntervalAlgebra.RelationProperties
  ( IntervalRelationProperties(..)
  ) where

import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , isNothing
                                                )
import           Data.Set                       ( Set
                                                , disjointUnion
                                                , fromList
                                                , member
                                                )
import           Data.Time                     as DT
                                                ( Day
                                                , NominalDiffTime
                                                , UTCTime
                                                )
import           IntervalAlgebra.Arbitrary
import           IntervalAlgebra.Core
import           Test.QuickCheck                ( (===)
                                                , (==>)
                                                , Arbitrary(arbitrary)
                                                , Property
                                                )

allIArelations :: (Ord a) => [ComparativePredicateOf1 (Interval a)]
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

-- | A collection of properties for the interval algebra. Some of these come from 
--   figure 2 in  [Allen and Hayes (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x).
class ( IntervalSizeable a b ) => IntervalRelationProperties a b where

    -- | For any two pair of intervals exactly one 'IntervalRelation' should hold
    prop_exclusiveRelations::  Interval a -> Interval a -> Property
    prop_exclusiveRelations x y =
      (  1 == length (filter id $ map (\r -> r x y) allIArelations)) === True

    -- | Given a set of interval relations and predicate function, test that the 
    -- predicate between two interval is equivalent to the relation of two intervals 
    -- being in the set of relations.
    prop_predicate_unions :: Ord a =>
          Set IntervalRelation
        -> ComparativePredicateOf2 (Interval a) (Interval a)
        -> Interval a
        -> Interval a
        -> Property
    prop_predicate_unions s pred i0 i1 =
      pred i0 i1 === (relate i0 i1 `elem` s)

    prop_IAbefore :: Interval a -> Interval a -> Property
    prop_IAbefore i j =
      before i j ==> (i `meets` k) && (k `meets` j)
        where k = beginerval (diff (begin j) (end i)) (end i)

    prop_IAstarts:: Interval a -> Interval a -> Property
    prop_IAstarts i j
      | starts i j = (j == fromJust (i .+. k)) === True
      | otherwise     = starts i j === False
        where k = beginerval (diff (end j) (end i)) (end i)

    prop_IAfinishes:: Interval a -> Interval a -> Property
    prop_IAfinishes i j
      | finishes i j = (j == fromJust ( k .+. i)) === True
      | otherwise       = finishes i j === False
        where k = beginerval (diff (begin i) (begin j)) (begin j)

    prop_IAoverlaps:: Interval a -> Interval a -> Property
    prop_IAoverlaps i j
      | overlaps i j = ((i == fromJust ( k .+. l )) &&
                          (j == fromJust ( l .+. m ))) === True
      | otherwise       = overlaps i j === False
        where k = beginerval (diff (begin j) (begin i)) (begin i)
              l = beginerval (diff (end i)   (begin j)) (begin j)
              m = beginerval (diff (end j)   (end i))   (end i)

    prop_IAduring:: Interval a -> Interval a-> Property
    prop_IAduring i j
      | during i j = (j == fromJust ( fromJust (k .+. i) .+. l)) === True
      | otherwise     = during i j === False
        where k = beginerval (diff (begin i) (begin j)) (begin j)
              l = beginerval (diff (end j)   (end i))   (end i)

    prop_disjoint_predicate :: (Ord a) =>
          Interval a
        -> Interval a
        -> Property
    prop_disjoint_predicate = prop_predicate_unions disjointRelations disjoint

    prop_notdisjoint_predicate :: (Ord a) =>
          Interval a
        -> Interval a
        -> Property
    prop_notdisjoint_predicate =
      prop_predicate_unions (complement disjointRelations) notDisjoint

    prop_concur_predicate :: (Ord a) =>
          Interval a
        -> Interval a
        -> Property
    prop_concur_predicate =
      prop_predicate_unions (complement disjointRelations) concur

    prop_within_predicate :: (Ord a) =>
          Interval a
        -> Interval a
        -> Property
    prop_within_predicate = prop_predicate_unions withinRelations within

    prop_enclosedBy_predicate :: (Ord a) =>
          Interval a
        -> Interval a
        -> Property
    prop_enclosedBy_predicate = prop_predicate_unions withinRelations enclosedBy

    prop_enclose_predicate :: (Ord a) =>
          Interval a
        -> Interval a
        -> Property
    prop_enclose_predicate = prop_predicate_unions (converse withinRelations) enclose

instance IntervalRelationProperties Int Int
instance IntervalRelationProperties Day Integer
instance IntervalRelationProperties UTCTime NominalDiffTime
