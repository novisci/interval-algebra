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

class (IntervalAlgebraic b a) => IntervalFilter b a | a -> b where

    -- |Creates a function for filtering a list of Intrvl (b a)s based on a predicate
    filterMaker :: ComparativePredicateOf (Intrvl (b a)) 
                   -> Intrvl (b a) 
                   -> ([Intrvl (b a)] 
                   -> [Intrvl (b a)])
    filterMaker f p = filter (`f` p)

    -- | Filter a list of Intrvl (b a)s to those overlapping the Intrvl (b a) p
    filterOverlaps :: Intrvl (b a) -> ([Intrvl (b a)] -> [Intrvl (b a)])
    filterOverlaps = filterMaker overlaps

    -- | Filter a list of Intrvl (b a)s to those overlapped by the Intrvl (b a) p
    filterOverlappedBy :: Intrvl (b a) -> ([Intrvl (b a)] -> [Intrvl (b a)])
    filterOverlappedBy = filterMaker overlappedBy

    -- | Filter a list of Intrvl (b a)s to those before the Intrvl (b a) p
    filterBefore :: Intrvl (b a) -> ([Intrvl (b a)] -> [Intrvl (b a)])
    filterBefore = filterMaker before

    -- | Filter a list of Intrvl (b a)s to those before the Intrvl (b a) p
    filterAfter :: Intrvl (b a) -> ([Intrvl (b a)] -> [Intrvl (b a)])
    filterAfter = filterMaker after

    -- | Filter a list of Intrvl (b a)s to those meeting the Intrvl (b a) p
    filterMeets :: Intrvl (b a) -> ([Intrvl (b a)] -> [Intrvl (b a)])
    filterMeets = filterMaker meets

    -- | Filter a list of Intrvl (b a)s to those meeting the Intrvl (b a) p
    filterMetBy :: Intrvl (b a) -> ([Intrvl (b a)] -> [Intrvl (b a)])
    filterMetBy = filterMaker metBy

    -- | Filter a list of Intrvl (b a)s to those during the Intrvl (b a) p
    filterDuring :: Intrvl (b a) -> ([Intrvl (b a)] -> [Intrvl (b a)])
    filterDuring = filterMaker during

    -- | Filter a list of Intrvl (b a)s to those containing the Intrvl (b a) p
    filterContains :: Intrvl (b a) -> ([Intrvl (b a)] -> [Intrvl (b a)])
    filterContains = filterMaker contains
