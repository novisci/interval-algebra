module IntervalAlgebra.IntervalFilter (
  filterOverlaps,
  filterOverlappedBy,
  filterBefore,
  filterAfter,
  filterMeets,
  filterMetBy,
  filterDuring,
  filterContains
) where

import IntervalAlgebra


{- | 
TODO: describe this class.

TODO: generalize the class to handle generalized "filterable" containers (not
just lists).
-}

class IntervalAlgebraic a => IntervalFilter a where

    -- |Creates a function for filtering a list of Intrvl as based on a predicate
    filterMaker :: ComparativePredicateOf (Intrvl a) -> Intrvl a -> ([Intrvl a] -> [Intrvl a])
    filterMaker f p = filter (`f` p)

    -- | Filter a list of Intrvl as to those overlapping the Intrvl a p
    filterOverlaps :: Intrvl a -> ([Intrvl a] -> [Intrvl a])
    filterOverlaps = filterMaker overlaps

    -- | Filter a list of Intrvl as to those overlapped by the Intrvl a p
    filterOverlappedBy :: Intrvl a -> ([Intrvl a] -> [Intrvl a])
    filterOverlappedBy = filterMaker overlappedBy

    -- | Filter a list of Intrvl as to those before the Intrvl a p
    filterBefore :: Intrvl a -> ([Intrvl a] -> [Intrvl a])
    filterBefore = filterMaker before

    -- | Filter a list of Intrvl as to those before the Intrvl a p
    filterAfter :: Intrvl a -> ([Intrvl a] -> [Intrvl a])
    filterAfter = filterMaker after

    -- | Filter a list of Intrvl as to those meeting the Intrvl a p
    filterMeets :: Intrvl a -> ([Intrvl a] -> [Intrvl a])
    filterMeets = filterMaker meets

    -- | Filter a list of Intrvl as to those meeting the Intrvl a p
    filterMetBy :: Intrvl a -> ([Intrvl a] -> [Intrvl a])
    filterMetBy = filterMaker metBy

    -- | Filter a list of Intrvl as to those during the Intrvl a p
    filterDuring :: Intrvl a -> ([Intrvl a] -> [Intrvl a])
    filterDuring = filterMaker during

    -- | Filter a list of Intrvl as to those containing the Intrvl a p
    filterContains :: Intrvl a -> ([Intrvl a] -> [Intrvl a])
    filterContains = filterMaker contains
