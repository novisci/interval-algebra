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

-- |Creates a function for filtering a list of Periods based on a predicate
filterMaker :: PredicateOf Period -> Period -> ([Period] -> [Period])
filterMaker f p = filter (\x -> f x p)

-- | Filter a list of Periods to those overlapping the Period p
filterOverlaps :: Period -> ([Period] -> [Period])
filterOverlaps p = filterMaker overlaps p

-- | Filter a list of Periods to those overlapped by the Period p
filterOverlappedBy :: Period -> ([Period] -> [Period])
filterOverlappedBy p = filterMaker overlappedBy p

-- | Filter a list of Periods to those before the Period p
filterBefore :: Period -> ([Period] -> [Period])
filterBefore p = filterMaker before p

-- | Filter a list of Periods to those before the Period p
filterAfter :: Period -> ([Period] -> [Period])
filterAfter p = filterMaker after p

-- | Filter a list of Periods to those meeting the Period p
filterMeets :: Period -> ([Period] -> [Period])
filterMeets p = filterMaker meets p

-- | Filter a list of Periods to those meeting the Period p
filterMetBy :: Period -> ([Period] -> [Period])
filterMetBy p = filterMaker metBy p

-- | Filter a list of Periods to those during the Period p
filterDuring :: Period -> ([Period] -> [Period])
filterDuring p = filterMaker during p

-- | Filter a list of Periods to those containing the Period p
filterContains :: Period -> ([Period] -> [Period])
filterContains p = filterMaker contains p
