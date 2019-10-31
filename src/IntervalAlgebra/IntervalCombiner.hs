module IntervalAlgebra.IntervalCombiner (
     (<<>>)
) where

import IntervalAlgebra


{- | Link two lists of Periods by creating a linking period from the begin of 
the last period in the first list and the end of the first period in the 
second list -}
(<<>>) :: [Period] -> [Period] -> [Period]
(<<>>) xl yl
   | null xl   = yl
   | null yl   = xl
   | otherwise = init xl ++ [period (begin x) (end y)] ++ tailList yl
   where x = last xl
         y = head yl
{-
-- | Collapse two lists of Periods such that if the last period of the first 
--   list and the first period of the second overlap they are linked by `<<>>`. 
--   Otherwise, the lists are concatenated.
(<++>) :: [Period] -> [Period] -> [Period]
(<++>) xl yl
   | null xl         = yl
   | null yl         = xl
   | x `overlaps` y  = xl <<>> yl
   | x `meets` y     = xl <<>> yl
   | x `before` y    = xl ++ yl
   where x = last xl
         y = head yl
-}
-- | TODO
--   Note this behavior on overlapping periods:
--   s3 = map toPeriod [(1, 3), (1, 7), (13, 13), (13, 16), (20, 20)]
--   *Hasklepias> periodGaps s3
--   [(1, 1),(7, 16),(20, 20)]
--
--   But for non overlapping periods:
--   *Hasklepias> periodGaps (collapsePeriods s3)
--   [(1, 1),(7, 13),(16, 20)]
{-
(<-->) :: [Period] -> [Period] -> [Period]
(<-->) [] []     = []
(<-->) [] (y:ys) = beginEndPoint y
(<-->) (x:xs) [] = beginEndPoint x
(<-->) (x:xs) (y:ys)
   | x `meets` y || x `overlaps` y   
                     = init xs ++ 
                       init (beginEndPoint x) ++ tail (beginEndPoint y) ++
                       ys
   | x `before` y    = init xs ++ 
                       beginEndPoint x <<>> beginEndPoint y ++ 
                       ys
   where x = last xs
--         y = head yl
-}

{-
-- | Traverses over a list of periods collapsing the periods by `<++>` to create
--   a list of non-overlapping periods.
collapsePeriods :: [Period] -> [Period]
collapsePeriods = foldr ((<++>) . (: [])) []

-- | TODO
periodGaps :: [Period] -> [Period]
periodGaps = foldr ((<-->) . (: [])) []
-}



-- | Returns an empty list in the case of an empty list.
tailList :: [a] -> [a]
tailList (_:xs)   = xs
tailList []       = []
