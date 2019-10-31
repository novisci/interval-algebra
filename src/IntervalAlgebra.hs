module IntervalAlgebra(
    -- Types
      Period
    , PredicateOf

    -- Constructors and accessors
    , period
    , point
    , begin
    , end

    -- IA relations
    , meets
    , metBy
    , before
    , after
    , overlaps
    , overlappedBy
    , starts
    , startedBy
    , finishes
    , finishedBy
    , during
    , contains

    -- Utilities
    , isPoint
    , toPeriod
    , beginPoint
    , beginPoints
    , endPoint
    , endPoints
    , beginEndPoint
    , expandl
    , expandlPeriods
    , expandr
    , expandrPeriods
    , extentPeriod
    , disjoint
    , duration
    , durations
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

{- | The Periodic class specifies the functions and relational operators 
according to Allen's interval algebra. -}

class Periodic a where
    -- | Does x meet y? Does y meet x?
    meets, metBy             :: PredicateOf a

    -- | Is x before y? Is x after y?
    before, after            :: PredicateOf a
    
    -- | Does x overlap y? Is x overlapped by y?
    overlaps, overlappedBy   :: PredicateOf a
    
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
  meets    x y  = (x /= y) && begin y == end x
    -- if statement handles case that points can't meet
    -- TODO: handle this more elegantly in the IA type system
  before   x y  = end x < begin y
  starts   x y  = (x <= y) && (begin x) == begin y
  finishes x y  = if y <= x then (end x) == end y else False
  during   x y  = (begin x) >= begin y && (end x) <= end y
  overlaps x y  = x <= y  && end x < end y && end x > begin y 
  duration x    = (end x) - begin x

instance Ord Period where
  (<=) x y
    | begin x <  begin y = True
    | begin x == begin y = end x <= end y
    | otherwise = False
  (<)  x y 
    | begin x <  begin y = True
    | begin x == begin y = end x < end y
    | otherwise = False
    --  || (starts x y && (end x < end y))
  (>=) x y = not (x < y)
  (>)  x y = not (x <= y)

instance Show Period where
   show x = "(" ++ show (begin x) ++ ", " ++ show (end x) ++ ")"

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

-- | From a pair of periods form a new period from the min of the begin points
--   to the max of the end points.
extentPeriod :: Period -> Period -> Period
extentPeriod p1 p2 = period a b 
    where a = min (begin p1) (begin p2)
          b = max (end p1)   (end p2)

-- | Returns True if a Period has length 0. False else.
isPoint :: Period -> Bool
isPoint x = duration x == 0 

-- | Returns a list of durations from a list of periods.
durations :: [Period] -> [Int]
durations = map duration
