{-|
Module      : Interval Algebra Utilities
Description : Functions for operating on containers of Intervals.
Copyright   : (c) NoviSci, Inc 2020-2022
                  TargetRWE, 2023
License     : BSD3
Maintainer  : bsaul@novisci.com 2020-2022, bbrown@targetrwe.com 2023
Stability   : experimental

-}

{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

module IntervalAlgebra.IntervalUtilities
  (

    -- * Fold over sequential intervals
    combineIntervals
  , combineIntervalsFromSorted
  , rangeInterval

    -- * Combining intervals
  , (><)
  , (.+.)

    -- * Functions for manipulating intervals
  , lookback
  , lookahead

    -- * Gaps
  , gaps
  , pairGaps

    -- * Misc utilities
  , relations
  , intersect
  , clip
  , durations
  ) where

import           Control.Applicative            (Applicative (pure), liftA2,
                                                 (<$>), (<*>))
import qualified Control.Foldl                  as L
import           Control.Monad                  (Functor (fmap))
import           Data.Bool                      (Bool (..), not, otherwise,
                                                 (&&), (||))
import           Data.Eq                        (Eq ((==)))
import           Data.Foldable                  (Foldable (foldl', foldr, null, toList),
                                                 all, any, or)
import           Data.Function                  (flip, ($), (.))
import           Data.List                      (map, reverse, sortOn)
import           Data.Maybe                     (Maybe (..), maybe, maybeToList)
import           Data.Monoid                    (Monoid (mempty))
import           Data.Ord                       (Ord (max, min), (<), (>=))
import           Data.Semigroup                 (Semigroup ((<>)))
import           Data.Traversable               (Traversable (sequenceA))
import           Data.Tuple                     (fst, uncurry)
import           GHC.Int                        (Int)
import           GHC.Show                       (Show)
import           IntervalAlgebra.Core
import           IntervalAlgebra.PairedInterval (PairedInterval, equalPairData,
                                                 getPairData,
                                                 makePairedInterval)
import           Safe                           (headMay, initSafe, lastMay,
                                                 tailSafe)
import           Witherable                     (Filterable (filter),
                                                 Witherable (..), catMaybes,
                                                 mapMaybe)

{- $setup
>>> import GHC.List ( (++), zip )
>>> import IntervalAlgebra.IntervalDiagram
>>> import Prettyprinter ( pretty )
-}

-------------------------------------------------
-- Unexported utilties used in functions below --
-------------------------------------------------


-- | Gets the durations of gaps (via '(><)') between all pairs of the input.
pairGaps
  :: (Intervallic i, SizedIv (Interval a), Ord a, Ord (Moment (Interval a)))
  => [i a]
  -> [Maybe (Moment (Interval a))]
pairGaps es = fmap (fmap duration . uncurry (><)) (pairs es)
-- Generate all pair-wise combinations of a single list.
-- pairs :: [a] -> [(a, a)]
-- copied from the hgeometry library
-- (https://hackage.haskell.org/package/hgeometry-0.12.0.4/docs/src/Data.Geometry.Arrangement.Internal.html#allPairs)
 where
  pairs = go
   where
    go []       = []
    go (x : xs) = fmap (x, ) xs <> go xs

-- | Creates a new @Interval@ of a provided lookback duration ending at the
--   'begin' of the input interval.
--
-- >>> lookback 4 (beginerval 10 (1 :: Int))
-- (-3, 1)
lookback
  :: (Intervallic i, SizedIv (Interval a), Ord (Moment (Interval a)))
  => Moment (Interval a)   -- ^ lookback duration
  -> i a
  -> Interval a
lookback d x = enderval d (begin x)

-- | Creates a new @Interval@ of a provided lookahead duration beginning at the
--   'end' of the input interval.
--
-- >>> lookahead 4 (beginerval 1 (1 :: Int))
-- (2, 6)
lookahead
  :: (Intervallic i, SizedIv (Interval a), Ord (Moment (Interval a)))
  => Moment (Interval a)   -- ^ lookahead duration
  -> i a
  -> Interval a
lookahead d x = beginerval d (end x)

-- | Returns a list of the 'IntervalRelation' between each consecutive pair of @i a@.
--
-- >>> relations [beginerval 1 0, beginerval 1 1]
-- [Meets]
-- >>> relations [beginerval 1 0, beginerval 1 1, beginerval 2 1]
-- [Meets,Starts]
-- >>> relations [beginerval 1 0]
-- []
relations
  :: ( Intervallic i
     , Iv (Interval a)
     )
  => [i a]
  -> [IntervalRelation]
relations []           = []
relations [x]          = []
relations (x : y : xs) = relate x y : relations (y : xs)

-- | Forms 'Just' a new interval from the intersection of two intervals,
--   provided the intervals are not 'disjoint'.
--
-- >>> intersect (bi 5 0) (bi 2 3)
-- Just (3, 5)
--
intersect
  :: (Intervallic i, SizedIv (Interval a), Ord a, Ord (Moment (Interval a))) => i a -> i a -> Maybe (Interval a)
intersect x y | disjoint x y = Nothing
              | otherwise    = Just $ safeInterval (b, e)
 where
  b = max (begin x) (begin y)
  e = min (end x) (end y)

{- | Returns a list of intervals consisting of the gaps between
consecutive intervals in the input, after they have been sorted by
interval ordering.

>>> x1 = bi 4 1
>>> x2 = bi 4 8
>>> x3 = bi 3 11
>>> ivs = [x1, x2, x3]
>>> ivs
[(1, 5),(8, 12),(11, 14)]
>>> gaps ivs
[(5, 8)]
>>> pretty $ standardExampleDiagram (zip ivs ["x1", "x2", "x3"]) []
 ----          <- [x1]
        ----   <- [x2]
           --- <- [x3]
==============

>>> x1 = bi 4 1
>>> x2 = bi 3 7
>>> x3 = bi 2 13
>>> ivs = [x1, x2, x3]
>>> ivs
[(1, 5),(7, 10),(13, 15)]
>>> gapIvs = gaps ivs
>>> gapIvs
[(5, 7),(10, 13)]
>>> :{
  pretty $
    standardExampleDiagram (zip ivs ["x1", "x2", "x3"]) [(gapIvs, "gapIvs")]
:}
 ----           <- [x1]
       ---      <- [x2]
             -- <- [x3]
     --   ---   <- [gapIvs]
===============
-}
gaps :: (
  SizedIv (Interval a),
  Intervallic i,
  Ord a,
  Ord (Moment (Interval a))
  ) =>
  [i a] ->
  [Interval a]
gaps xs = mapMaybe (uncurry (><)) $ pair $ sortOn getInterval xs
  where pair []           = []
        pair [x]          = []
        pair (x : y : ys) = (x, y) : pair (y : ys)

-- | Returns the 'duration' of each 'Intervallic i a' in the 'Functor' @f@.
--
-- >>> durations [bi 9 1, bi 10 2, bi 1 5 :: Interval Int]
-- [9,10,1]
--
durations :: (Functor f, Intervallic i, SizedIv (Interval a)) => f (i a) -> f (Moment (Interval a))
durations = fmap (duration . getInterval)

-- | In the case that x y are not disjoint, clips y to the extent of x.
--
-- >>> clip (bi 5 0) ((bi 3 3) :: Interval Int)
-- Just (3, 5)
--
-- >>> clip (bi 3 0) ((bi 2 4) :: Interval Int)
-- Nothing
--
clip
  :: (Intervallic i0, Intervallic i1, SizedIv (Interval a), Ord a, Ord (Moment (Interval a)))
  => i0 a
  -> i1 a
  -> Maybe (Interval a)
clip x y
  | overlaps x y     = Just $ safeInterval (begin y, end x)
  | overlappedBy x y = Just $ safeInterval (begin x, end y)
  | jx x y           = Just (getInterval x)
  | jy x y           = Just (getInterval y)
  | otherwise        = Nothing {- disjoint x y case -}
 where
  jy = equals <|> startedBy <|> contains <|> finishedBy
  jx = starts <|> during <|> finishes
{-# INLINABLE clip #-}

{- | Returns a list of intervals where any intervals that meet or share support
are combined into one interval. This function sorts the input. If you know the
input intervals are sorted, use @combineIntervalsLFromSorted@.

>>> x1 = bi 10 0
>>> x2 = bi 5 2
>>> x3 = bi 2 10
>>> x4 = bi 2 13
>>> ivs = [x1, x2, x3, x4]
>>> ivs
[(0, 10),(2, 7),(10, 12),(13, 15)]
>>> xComb = combineIntervals ivs
>>> xComb
[(0, 12),(13, 15)]
>>> :{
pretty $
  standardExampleDiagram
    (zip ivs ["x1", "x2", "x3", "x4"])
    [(xComb, "xComb")]
:}
----------      <- [x1]
  -----         <- [x2]
          --    <- [x3]
             -- <- [x4]
------------ -- <- [xComb]
===============
-}
combineIntervals :: (SizedIv (Interval a), Intervallic i, Ord a) => [i a] -> [Interval a]
combineIntervals = combineIntervalsFromSorted . sortOn getInterval

{- | Returns a list of intervals where any intervals that meet or share support
are combined into one interval. The operation is applied cumulatively, from left
to right, so
__to work properly, the input list should be sorted in increasing order__.

>>> combineIntervalsFromSorted [bi 10 0, bi 5 2, bi 2 10, bi 2 13]
[(0, 12),(13, 15)]

>>> combineIntervalsFromSorted [bi 10 0, bi 5 2, bi 0 8]
[(0, 10)]
-}
combineIntervalsFromSorted
  :: forall a i . (Ord a, Intervallic i, SizedIv (Interval a)) => [i a] -> [Interval a]
combineIntervalsFromSorted = reverse . foldl' op []
 where
  op []       y = [getInterval y]
  op (x : xs) y = if x `before` y
    -- Since x <= y, not (x `before` y) iff they meet or share support
    then yiv : x : xs
    else extenterval x yiv : xs
    where yiv = getInterval y

{- | @Maybe@ form an @Interval a@ from @Control.Foldl t => t (Interval a)@
spanning the range of all intervals in the list, i.e. whose @begin@ is the
minimum of @begin@ across intervals in the list and whose @end@ is the maximum
of @end@.

>>> rangeInterval ([] :: [Interval Int])
Nothing

>>> x1 = bi 2 2
>>> x2 = bi 3 6
>>> x3 = bi 4 7
>>> ivs = [x1, x2, x3] :: [Interval Int]
>>> ivs
[(2, 4),(6, 9),(7, 11)]
>>> spanIv = rangeInterval ivs
>>> spanIv
Just (2, 11)
>>> :{
case spanIv of
  Nothing -> pretty ""
  (Just x) -> pretty $ standardExampleDiagram
    (zip (ivs ++ [x]) ["x1", "x2", "x3", "spanIv"])
    []
:}
  --        <- [x1]
      ---   <- [x2]
       ---- <- [x3]
  --------- <- [spanIv]
===========

>>> rangeInterval (Nothing :: Maybe (Interval Int))
Nothing
>>> rangeInterval (Just (bi 1 0))
Just (0, 1)
-}
rangeInterval :: (L.Foldable t, Ord a, SizedIv (Interval a)) => t (Interval a) -> Maybe (Interval a)
rangeInterval = L.fold (liftA2 extenterval <$> L.minimum <*> L.maximum)

  {- Combining intervals -}

-- | If @x@ is 'before' @y@, then form a new @Just Interval a@ from the
--   interval in the "gap" between @x@ and @y@ from the 'end' of @x@ to the
--   'begin' of @y@. Otherwise, 'Nothing'.
(><) :: (Iv (Interval a), Ord (Moment (Interval a)), SizedIv (Interval a), Intervallic i) => i a -> i a -> Maybe (Interval a)
(><) x y
  | x `before` y = Just $ safeInterval (end x, begin y)
  | otherwise    = Nothing

-- | Maybe form a new @Interval a@ by the union of two @Interval a@s that 'meets'.
(.+.) :: (Iv (Interval a), Ord (Moment (Interval a)), SizedIv (Interval a), Intervallic i) => i a -> i a -> Maybe (Interval a)
(.+.) x y
  | x `meets` y = Just $ safeInterval (begin x, end y)
  | otherwise   = Nothing
