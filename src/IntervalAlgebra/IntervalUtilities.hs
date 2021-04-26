{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : Interval Algebra Utilities
Description : Functions for operating on containers of Intervals.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

{-# LANGUAGE FlexibleContexts #-}
module IntervalAlgebra.IntervalUtilities (
      combineIntervals
    , gaps
    , durations
    , clip
    , relations
    , gapsWithin
    , emptyIf
    , emptyIfNone
    , emptyIfAny
    , emptyIfAll
) where

import GHC.Base
    ( (++), map, foldr, otherwise, ($), (.), (<*>), seq, not
    , Semigroup((<>)), Functor(fmap), Maybe(..)
    , Int, Bool)
import GHC.Num ()
import Data.Tuple ( uncurry )
import Data.Foldable ( Foldable(null, foldl'), all, any )
import Data.Monoid ( (<>), Monoid(mempty) )
import IntervalAlgebra
    ( Interval, Intervallic(..), IntervalAlgebraic(..)
    , IntervalCombinable(..), IntervalSizeable(..)
    , IntervalFilterable(..)
    , IntervalRelation(..))
import Data.Maybe (mapMaybe)
import Data.List ( (++), head, init, last, tail, zip )
import Witherable ( Filterable )

intInt :: Int -> Int -> Interval Int
intInt = unsafeInterval

-- | Box to avoid overlapping instances
-- TODO: avoid the head/tail footguns
newtype Box a = Box { unBox :: [a] }
instance (IntervalCombinable a) => Semigroup (Box (Interval a)) where
    Box x <> Box y
       | null x         = Box y
       | null y         = Box x
       | otherwise      = Box $ init x ++ (lx <+> fy) ++ tail y
       where lx = last x
             fy = head y

-- | Returns a list of intervals where any intervals that meet or share support
--   are combined into one interval. *To work properly, the input list should 
--   be sorted*. 
--
-- >>> combineIntervals [intInt 0 10, intInt 2 7, intInt 10 12, intInt 13 15]
-- [(0, 12),(13, 15)]
combineIntervals :: (IntervalCombinable a) => [Interval a] -> [Interval a]
combineIntervals l = unBox $ foldl' (<>) (Box []) (map (\z -> Box [z]) l)

-- | Returns a (possibly empty) list of intervals consisting of the gaps between
--   intervals in the input list. *To work properly, the input list should be sorted*.
gaps :: (IntervalCombinable a) => [Interval a] -> [Interval a]
gaps l = mapMaybe (uncurry (><)) ((zip <*> tail) l)

-- | Returns the 'duration' of each 'Interval' in the 'Functor' @f@.
--
-- >>> durations [intInt 1 10, intInt 2 12, intInt 5 6]
-- [9,10,1]
durations :: (Functor f, IntervalSizeable a b) => f (Interval a) -> f b
durations = fmap duration


-- | In the case that x y are not disjoint, clips y to the extent of x.
-- 
-- >>> clip (intInt 0 5) (intInt 3 6)
-- Just (3, 5)
--
-- >>> clip (intInt 0 3) (intInt 4 6)
-- Nothing
clip :: (IntervalAlgebraic a, IntervalSizeable a b)=>
       Interval a
    -> Interval a
    -> Maybe (Interval a)
clip x y
   | overlaps x y     = Just $ enderval   (diff (end x) (begin y)) (end x)
   | overlappedBy x y = Just $ beginerval (diff (end y) (begin x)) (begin x)
   | jx x y           = Just x
   | jy x y           = Just y
   | disjoint x y     = Nothing
   where jy = equals <|> startedBy <|> contains <|> finishedBy
         jx = starts <|> during <|> finishes

-- | Finds the 'IntervalRelation' between each consecutive pair of intervals.
-- 
-- >>> relations [intInt 0 1, intInt 1 2] 
relations :: (IntervalAlgebraic a)=> [Interval a] -> [IntervalRelation a]
-- TODO: generalize to collections besides list
relations x = map (uncurry relate) ((zip <*> tail) x)

-- | Applies 'gaps' to all the non-disjoint intervals in @x@ that are *not* disjoint
-- from @i@. Intervals that 'overlaps' or are 'overlappedBy' @i@ are 'clip'ped to @i@.
--
-- >>> gapsWithin (intInt 1 10) [intInt 0 5, intInt 7 9, intInt 12 15]
-- [(5, 7),(9, 10)]
gapsWithin :: (IntervalSizeable a b, IntervalCombinable a, IntervalFilterable [] a)=>
      Interval a  -- ^ i
  -> [Interval a] -- ^ x
  -> [Interval a]
-- TODO: generalize to collections besides list
gapsWithin i x = gaps $ enderval 0 (begin i) :
                        mapMaybe (clip i) (filterNotDisjoint i x) ++
                        [beginerval 0 (end i)]

-- | Given a predicate combinator, a predicate, and list of intervals, returns 
--   the input unchanged if the predicate combinator is 'True'. Otherwise, returns
--   an empty list. See 'emptyIfAny' and 'emptyIfNone' for examples.
emptyIf :: (Monoid (f (Interval a)), Foldable f, IntervalFilterable f a)=>
     ((Interval a -> Bool) -> f (Interval a) -> Bool) -- ^ e.g. 'any' or 'all'
  -> (Interval a -> Bool) -- ^ predicate to apply to each element of input list
  -> f (Interval a)
  -> f (Interval a)
emptyIf g f x = if g f x then mempty else x

-- | Returns the empty monoid structure if *none* of the element of input satisfy
--   the predicate condition.
-- 
-- For example, the following returns the empty list because none of the intervals
-- in the input list 'starts' (3, 5).
-- >>> emptyIfNone (starts (intInt 3 5)) [intInt 3 4, intInt 5 6]
-- []
--
-- In the following, (3, 5) 'starts' (3, 6), so the input is returned.
-- >>> emptyIfNone (starts (intInt 3 5)) [intInt 3 6, intInt 5 6]
-- [(3, 6),(5, 6)]
emptyIfNone :: (Monoid (f (Interval a)), Foldable f, IntervalFilterable f a)=>
    (Interval a -> Bool) -- ^ predicate to apply to each element of input list
  -> f (Interval a)
  -> f (Interval a)
emptyIfNone = emptyIf (\f x -> (not.any f) x)

-- | Returns the empty monoid structure if *any* of the element of input satisfy
--   the predicate condition
emptyIfAny :: (Monoid (f (Interval a)), Foldable f, IntervalFilterable f a)=>
    (Interval a -> Bool) -- ^ predicate to apply to each element of input list
  -> f (Interval a)
  -> f (Interval a)
emptyIfAny = emptyIf any

-- | Returns the empty monoid structure if *all* of the element of input satisfy
--   the predicate condition
emptyIfAll :: (Monoid (f (Interval a)), Foldable f, IntervalFilterable f a)=>
    (Interval a -> Bool) -- ^ predicate to apply to each element of input list
  -> f (Interval a)
  -> f (Interval a)
emptyIfAll = emptyIf all
