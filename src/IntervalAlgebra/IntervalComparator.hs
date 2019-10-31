module IntervalAlgebra.IntervalComparator (
      extentPeriods
    , pairPeriods
    , comparePeriodPairs
    , comparePeriodPairsList
) where

import IntervalAlgebra


type PeriodPairs = [(Period, Period)]
type PeriodComparator a = (Period -> Period -> a)

-- | Form the extentPeriod for each element in a PeriodPairs.
extentPeriods :: PeriodPairs -> [Period]
extentPeriods = map (uncurry extentPeriod)


{-
 Functions for comparing and combining multiple Periods
-}

{- | Builds a list of lists of pairs of each successive head Period with the 
remaining tail Periods after applying headf to the head Period and 
tailf to the tail Periods. Returns a list of PeriodPairs of length n - 1, 
where n is the length of the input list. 
-}

pairPeriods :: (Period -> Period) -> ([Period] -> [Period]) -> [Period] -> [PeriodPairs]
pairPeriods _ _ []    = []
pairPeriods headf tailf (x:xs)
  | null xs   = []
  | otherwise = [(s, e) | s <- [headf x], e <- tailf xs] :
    pairPeriods headf tailf xs

{-
  Functions for deriving new information from a Period, pairs for Periods, or
  lists of Periods
-}

-- | 
comparePeriodPairs :: PeriodComparator a -> PeriodPairs -> [a]
comparePeriodPairs f = map (uncurry f)

-- | 
-- An example:
-- let zz = pairPeriods id id s3
-- let ff x = duration.extentPeriod x
-- comparePeriodPairsList ff zz
-- [[6,12,15,19],[12,15,19],[3,7],[7]]
comparePeriodPairsList :: PeriodComparator a -> [PeriodPairs] -> [[a]]
comparePeriodPairsList f = map (comparePeriodPairs f)



