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

module IntervalAlgebra.IntervalUtilities (
      combineIntervals
    , gaps
    , durations
    , clip
    , relations
) where

import GHC.Base
    ( (++), map, foldr, otherwise, ($), (.), (<*>)
    , Semigroup((<>)), Functor(fmap), Maybe(..))
import Data.Foldable()
import Prelude (uncurry, zip, Num)
import IntervalAlgebra
    ( Interval, Intervallic(..), IntervalAlgebraic(..)
    , IntervalCombinable(..), IntervalSizeable(..)
    , IntervalRelation(..))
import Data.Maybe (mapMaybe)
import Data.List ( (++), null, any, head, init, last, tail )

-- | Box to avoid overlapping instances
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
combineIntervals :: (IntervalCombinable a) => [Interval a] -> [Interval a]
combineIntervals l = unBox $ foldr ((<>) . (\z -> Box [z])) (Box []) l

-- | Returns a (possibly empty) list of intervals consisting of the gaps between
--   intervals in the input list. *To work properly, the input list should be sorted*.
gaps :: (IntervalCombinable a) => [Interval a] -> [Interval a]
gaps l = mapMaybe (uncurry (><)) ((zip <*> tail) l)

-- | Returns the 'duration' of each 'Interval' in the 'Functor' @f@.
durations :: (Functor f, IntervalSizeable a b) => f (Interval a) -> f b
durations = fmap duration


-- | In the case that x y are not disjoint, clips y to the extent of x.
clip :: (IntervalAlgebraic a, IntervalSizeable a b)=>
       Interval a
    -> Interval a
    -> Maybe (Interval a)
clip x y
   | overlaps x y     = Just $ enderval (diff (end x) (begin y)) (end x)
   | overlappedBy x y = Just $ beginerval (diff (end y) (begin x)) (begin x)
   | jx x y           = Just x
   | jy x y           = Just y
   | disjoint x y     = Nothing
   where jy = equals <|> startedBy <|> contains <|> finishedBy
         jx = starts <|> during <|> finishes

-- | Finds the 'IntervalRelation' between each consecutive pair of intervals.
relations :: (IntervalAlgebraic a)=> [Interval a] -> [IntervalRelation a]
relations x = map (uncurry relate) ((zip <*> tail) x)
-- TODO: generalize to collections besides list