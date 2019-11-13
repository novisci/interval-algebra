{-# LANGUAGE FunctionalDependencies #-}

{-|
Module      : Interval Algebra Filtrations
Description : Offers functions for filtering list of intervals based on a 
              reference interval.
Copyright   : (c) NoviSci, Inc 2019
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental

TODO
-}

module IntervalAlgebra.IntervalFilter (
  IntervalFilter(..)
) where

import IntervalAlgebra

{- | 
TODO: describe this class.

TODO: generalize the class to handle generalized "filterable" containers (not
just lists).
-}

class (IntervalAlgebraic a) => IntervalFilter a where

    -- |Creates a function for filtering a list of Interval as based on a predicate
    filterMaker :: ComparativePredicateOf (Interval a) 
                   -> Interval a 
                   -> ([Interval a] 
                   -> [Interval a])
    filterMaker f p = filter (`f` p)

    -- | Filter a list of Interval as to those overlapping the Interval a p
    filterOverlaps :: Interval a -> ([Interval a] -> [Interval a])
    filterOverlaps = filterMaker overlaps

    -- | Filter a list of Interval as to those overlapped by the Interval a p
    filterOverlappedBy :: Interval a -> ([Interval a] -> [Interval a])
    filterOverlappedBy = filterMaker overlappedBy

    -- | Filter a list of Interval as to those before the Interval a p
    filterBefore :: Interval a -> ([Interval a] -> [Interval a])
    filterBefore = filterMaker before

    -- | Filter a list of Interval as to those before the Interval a p
    filterAfter :: Interval a -> ([Interval a] -> [Interval a])
    filterAfter = filterMaker after

    -- | Filter a list of Interval as to those meeting the Interval a p
    filterMeets :: Interval a -> ([Interval a] -> [Interval a])
    filterMeets = filterMaker meets

    -- | Filter a list of Interval as to those meeting the Interval a p
    filterMetBy :: Interval a -> ([Interval a] -> [Interval a])
    filterMetBy = filterMaker metBy

    -- | Filter a list of Interval as to those during the Interval a p
    filterDuring :: Interval a -> ([Interval a] -> [Interval a])
    filterDuring = filterMaker during

    -- | Filter a list of Interval as to those containing the Interval a p
    filterContains :: Interval a -> ([Interval a] -> [Interval a])
    filterContains = filterMaker contains
