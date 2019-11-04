{-
* Period manipulations

Functions for basic manipulations of a single period or element-wise in a 
list of Periods.
-}

-- |Constructor for Period data from Int
{-
period :: Int -> Int -> Period
period a b
  | b <  (a + 1) = error "b < (a + 1)" -- TODO: handle this in a more Haskelly way
--  | b == a       = Point a a
--  | b == (a + 1) = Moment   (a, (a + 1))
  | otherwise    = Period (a, b)
-}
-- |Creates point from a single Int
--point :: Int -> Period
--point a = Point a a

{-
-- | Expands each period in a list to the left by i
expandlPeriods  :: (Periodable a, Num a) => a -> [Period a] -> [Period a]
expandlPeriods i = map (expandl i)

-- | Expands each period in a list to the right by i
expandrPeriods :: (Periodable a, Num a) => a -> [Period a] -> [Period a]
expandrPeriods i = map (expandr i)
-}

{-
-- | Contract a period to a Point at its begin
beginPoint :: Period -> Period
beginPoint x = point (begin x)

-- | Contract each period in the list to its begin point
beginPoints :: [Period] -> [Period]
beginPoints = map beginPoint

-- | Contract a period to a Point at its end
endPoint :: Period -> Period
endPoint x = point (end x)

-- | Contract each period in the list to its end point
endPoints :: [Period] -> [Period]
endPoints = map endPoint

-- | Form a list of two points from the begin and end of a period. If x is 
--   already a point, returns [x].
beginEndPoint :: Period -> [Period]
beginEndPoint x
    | isPoint x = [x]
    | otherwise = [beginPoint x, endPoint x]
-}

-- | Returns True if a Period has length 0. False else.
-- isPoint :: Period -> Bool
-- isPoint x = duration x == 0 