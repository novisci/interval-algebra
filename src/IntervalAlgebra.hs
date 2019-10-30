module IntervalAlgebra(
  Period,
  PredicateOf,
  period,
  point,
  isPoint,
  toPeriod,
  begin,
  end,
  beginPoint,
  beginPoints,
  endPoint,
  endPoints,
  beginEndPoint,
  expandl,
  expandlPeriods,
  expandr,
  expandrPeriods,
  extentPeriod,
  extentPeriods,
  collapsePeriods,
  periodGaps,
  meets,
  metBy,
  before,
  after,
  overlaps,
  overlappedBy,
  mverlaps,
  mverlappedBy,
  starts,
  startedBy,
  finishes,
  finishedBy,
  during,
  contains,
  disjoint,
  duration,
  durations,
  pairPeriods,
  (<<>>),
  (<++>),
  comparePeriodPairs,
  comparePeriodPairsList
) where

{-
_TODO list_
* look at Data.Sequence (or other options) for listlike [Period] type 
   * Consider performance implications for various types:
     https://github.com/haskell-perf/sequences
   * e.g. Seq are very fast for appending but slower for filter operations
* make Period type use newtype rather than data (see Toying.hs)
-}

type PredicateOf a = (a -> a -> Bool) 

-- |The Periodic class specifies the functions and operators for interval
-- algebra.
class Periodic a where
    -- | Does x meet y? Does y meet x?
    meets, metBy             :: PredicateOf a

    -- | Is x before y? Is x after y?
    before, after            :: PredicateOf a
    
    -- | Does x overlap y? Is x overlapped by y?
    overlaps, overlappedBy   :: PredicateOf a
    
    -- | Does x meet or overlap y? Is x met or overlapped by y?
    mverlaps, mverlappedBy   :: PredicateOf a
    
    -- | Does x begin y? Is x begined by y?
    starts, startedBy        :: PredicateOf a
    
    -- | Does x finishes y? Is x finished by y?
    finishes, finishedBy     :: PredicateOf a
    
    -- | Is x during y? Does x contain y?
    during, contains         :: PredicateOf a
    
    -- | Are x and y disjoint?
    disjoint                 :: PredicateOf a
    
    -- | What is the duration of x?
    duration                 :: a -> Int

    -- default function definitions
    metBy         = flip meets
    after         = flip before
    overlappedBy  = flip overlaps
    mverlappedBy  = flip mverlaps
    startedBy     = flip starts
    finishedBy    = flip finishes
    contains      = flip during
    disjoint x y  = before x y || after x y

-- |For now, a Period is defined in terms of Int
-- TODO: Generalize the notion of a Period to derive from arbitrary Ord types
-- see Toying.hs

data Period = 
    Point    { begin :: Int, end :: Int}
  | Moment   { begin :: Int, end :: Int}
  | Interval { begin :: Int, end :: Int}
  deriving (Eq, Read)

instance Periodic Period where
  {- These functions assume x <= y. TODO: formalize this notion -}
  meets    x y  = (x /= y) && (begin y) == end x
    -- if statement handles case that points can't meet
    -- TODO: handle this more elegantly in the IA type system
  before   x y  = end x < begin y
  starts   x y  = (x <= y) && ((begin x) == (begin y))
  finishes x y  = if y <= x then (end x)   == (end y)   else False
  during   x y  = (begin x) >= (begin y) && (end x) <= (end y)
  overlaps x y  = 
    if x <= y 
      then end x < end y && end x > begin y 
    else False
  mverlaps x y  = meets x y || overlaps x y
  duration x    = (end x) - (begin x)

instance Ord Period where
  (<=) x y
    | (begin x) <  (begin y) = True
    | (begin x) == (begin y) = end x <= end y
    | otherwise = False
  (<)  x y 
    | (begin x) <  (begin y) = True
    | (begin x) == (begin y) = end x < end y
    | otherwise = False
    --  || (starts x y && (end x < end y))
  (>=) x y = not (x < y)
  (>)  x y = not (x <= y)

instance Show Period where
   show x = "(" ++ show (begin x) ++ ", " ++ show (end x) ++ ")"

type PeriodPairs = [(Period, Period)]
type PeriodComparator a = (Period -> Period -> a)

{-
 Functions for basic manipulations of a single period or element-wise in a 
 list of Periods
-}

-- |Constructor for Period data from Int
period :: Int -> Int -> Period
period a b
  | b < a        = error "b < a" -- TODO: handle this in a more Haskelly way
  | b == a       = Point a a
  | b == (a + 1) = Moment a (a + 1)
  | otherwise    = Interval a b

-- |Creates point from a single Int
point :: Int -> Period
point a = Point a a

-- |Converts a pairs of Int to a Period
toPeriod :: (Int, Int) -> Period
toPeriod = uncurry period

-- | Expands a period to left by l and to the right by r
-- TODO: handle cases that l or r are negative
expand :: Int -> Int -> Period -> Period
expand l r p = period s e
  where s = min (begin p - l) (begin p)
        e = max (end p + r)   (end p)


-- | Expands a period to left by i
expandl :: Int -> Period -> Period
expandl i = expand i 0

-- | Expands each period in a list to the left by i
expandlPeriods  :: Int -> [Period] -> [Period]
expandlPeriods i = map (expandl i)

-- | Expands a period to right by i
expandr :: Int -> Period -> Period
expandr = expand 0

-- | Expands each period in a list to the right by i
expandrPeriods  :: Int -> [Period] -> [Period]
expandrPeriods i = map (expandr i)

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

{-
 Functions for comparing and combining multiple Periods
-}

-- | From a pair of periods form a new period from the min of the begin points
--   to the max of the end points.
extentPeriod :: Period -> Period -> Period
extentPeriod p1 p2 = period a b 
    where a = min (begin p1) (begin p2)
          b = max (end p1)   (end p2)

-- | Form the extentPeriod for each element in a PeriodPairs.
extentPeriods :: PeriodPairs -> [Period]
extentPeriods = map (uncurry extentPeriod)

-- | Link two lists of Periods by creating a linking period from the begin of 
--   the last period in the first list and the end of the first period in the 
--   second list
(<<>>) :: [Period] -> [Period] -> [Period]
(<<>>) xl yl
   | null xl   = yl
   | null yl   = xl
   | otherwise = init xl ++ [period (begin x) (end y)] ++ tailList yl
   where x = last xl
         y = head yl

-- | Collapse two lists of Periods such that if the last period of the first 
--   list and the first period of the second overlap they are linked by `<<>>`. 
--   Otherwise, the lists are concatenated.
(<++>) :: [Period] -> [Period] -> [Period]
(<++>) xl yl
   | null xl         = yl
   | null yl         = xl
   | x `mverlaps` y  = xl <<>> yl
   | x `before` y    = xl ++ yl
   where x = last xl
         y = head yl

-- | TODO
--   Note this behavior on overlapping periods:
--   s3 = map toPeriod [(1, 3), (1, 7), (13, 13), (13, 16), (20, 20)]
--   *Hasklepias> periodGaps s3
--   [(1, 1),(7, 16),(20, 20)]
--
--   But for non overlapping periods:
--   *Hasklepias> periodGaps (collapsePeriods s3)
--   [(1, 1),(7, 13),(16, 20)]
(<-->) :: [Period] -> [Period] -> [Period]
(<-->) xl yl
   | null xl         = beginEndPoint y
   | null yl         = beginEndPoint x
   | x `mverlaps` y  = init xl ++ 
                       init (beginEndPoint x) ++ tail (beginEndPoint y) ++
                       tailList yl
   | x `before` y    = init xl ++ 
                       (beginEndPoint x) <<>> (beginEndPoint y) ++ 
                       tailList yl
   where x = last xl
         y = head yl

-- | Traverses over a list of periods collapsing the periods by `<++>` to create
--   a list of non-overlapping periods.
collapsePeriods :: [Period] -> [Period]
collapsePeriods x = foldr ((<++>) . (\ z -> [z])) [] x

-- | TODO
periodGaps :: [Period] -> [Period]
periodGaps = foldr ((<-->) . (\ z -> [z])) []

-- | Builds a list of lists of pairs of each successive head Period with the 
--   remaining tail Periods after applying headf to the head Period and 
--   tailf to the tail Periods. Returns a list of PeriodPairs of length n - 1, 
--   where n is the length of the input list. 
pairPeriods :: (Period -> Period) -> ([Period] -> [Period]) -> [Period] -> [PeriodPairs]
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

-- | Returns True if a Period has length 0. False else.
isPoint :: Period -> Bool
isPoint x = duration x == 0 

-- | Returns a list of durations from a list of periods.
durations :: [Period] -> [Int]
durations = map duration

{-
 Utility functions
-}

-- | Returns an empty list in the case of an empty list.
tailList :: [a] -> [a]
tailList (_:xs)   = xs
tailList []       = []
