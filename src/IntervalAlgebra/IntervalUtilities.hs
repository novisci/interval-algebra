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
    , combineIntervals'
    , gaps
    , gaps'
    , durations
    , clip
    , relations
    , relations'
    , gapsWithin
    , emptyIf
    , emptyIfNone
    , emptyIfAny
    , emptyIfAll
) where

import GHC.Base
    ( otherwise, ($), (.), (<*>), seq, not
    , Semigroup((<>))
    , Functor(fmap)
    , Applicative(pure)
    , Int, Bool)
import GHC.Num ()
import Data.Tuple ( fst )
import Data.Foldable ( Foldable(null, foldl', toList), all, any )
import Data.Monoid ( (<>), Monoid(mempty) )
import IntervalAlgebra
    ( Interval, Intervallic(..), IntervalAlgebraic(..)
    , IntervalCombinable(..), IntervalSizeable(..)
    , IntervalFilterable(..)
    , IntervalRelation(..))
import Data.Maybe (mapMaybe, catMaybes, fromMaybe, Maybe(..))
import Data.List ( (++), map, head, init, last, tail )
import Witherable ( Filterable )

-------------------------------------------------
-- Unexported utilties used in functions below --
-------------------------------------------------

intInt :: Int -> Int -> Interval Int
intInt = unsafeInterval

-- Fold over consecutive pairs of foldable structure and collect the results in 
-- a monoidal structure.
foldlAccume :: (Foldable f, Applicative m, Monoid (m a))=>
      (b -> b -> a) -- ^ @f@: a function to apply to consecutive elements of @f b@
    -> f b
    -> m a
foldlAccume f x = fst $ foldl' (applyAccume f) (mempty, Nothing) x

-- Apply a function and accumulate the results in a monoidal structure.
applyAccume :: (Monoid (f a), Applicative f) =>
       (b -> b -> a)  -- ^ @f@: a function combining two @b@s to get an @a@
    -> (f a, Maybe b) -- ^ a pair (accumulating monoid for @b@s, optional @a@)
    -> b              -- ^ this will be the second argument to @f@
    -> (f a, Maybe b)
applyAccume f (fs, Nothing) x = (fs, Just x)
applyAccume f (fs, Just x)  y  = (fs <> pure (f x y), Just y)

-- Lifts a list to a foldable, applicative monoid 
liftListToFoldable :: (Applicative f
                      , Monoid (f a)
                      , Foldable f) =>
    [a] -> f a
liftListToFoldable = foldl' (\x y -> x <> pure y) mempty

-- Box to avoid overlapping instances
newtype Box a = Box { unBox :: [a] }

-- Defines how a Box of Intervals are combined. Specifically, the last element of
-- x and first element of y are combined by '<+>'.
instance (IntervalCombinable a) => Semigroup (Box (Interval a)) where
    Box x <> Box y
       | null x         = Box y
       | null y         = Box x
       | otherwise      = Box $ init x ++ (lx <+> fy) ++ tail y
       where lx = last x
             fy = head y

-------------------------------------------------

-- | Returns a container of intervals where any intervals that meet or share support
--   are combined into one interval. *To work properly, the input should 
--   be sorted*. See 'combineIntervals'' for a version that works only on lists.
--
-- >>> combineIntervals [intInt 0 10, intInt 2 7, intInt 10 12, intInt 13 15]
-- [(0, 12),(13, 15)]
combineIntervals :: (IntervalCombinable a
         , Applicative f
         , Monoid (f (Interval a))
         , Foldable f) =>
      f (Interval a) ->
      f (Interval a)
combineIntervals x = liftListToFoldable (combineIntervals' $ toList x)

-- | Returns a list of intervals where any intervals that meet or share support
--   are combined into one interval. *To work properly, the input list should 
--   be sorted*. 
--
-- >>> combineIntervals' [intInt 0 10, intInt 2 7, intInt 10 12, intInt 13 15]
-- [(0, 12),(13, 15)]
combineIntervals' :: (IntervalCombinable a) => [Interval a] -> [Interval a]
combineIntervals' l = unBox $ foldl' (<>) (Box []) (map (\z -> Box [z]) l)

-- | Returns a (possibly empty) container of intervals consisting of the gaps 
--   between intervals in the input. *To work properly, the input should be
--   sorted*. See 'gaps'' for a version that returns a list.
--
-- >>> gaps [intInt 1 5, intInt 8 12, intInt 11 14]
-- [(5, 8)]
gaps :: (IntervalCombinable a
         , Applicative f
         , Monoid (f (Interval a))
         , Foldable f) =>
      f (Interval a) ->
      f (Interval a)
gaps x = liftListToFoldable (gaps' x)

-- | Returns a (possibly empty) list of intervals consisting of the gaps between
--   intervals in the input container. *To work properly, the input should be 
--   sorted*. This version outputs a list. See 'gaps' for a version that lifts
--   the result to same input structure @f@.
gaps' :: (IntervalCombinable a
         , Applicative f
         , Monoid (f (Interval a))
         , Foldable f) =>
      f (Interval a) ->
      [Interval a]
gaps' x = catMaybes (foldlAccume (><) x)

-- | Returns the 'duration' of each 'Interval' in the 'Functor' @f@.
--
-- >>> durations [intInt 1 10, intInt 2 12, intInt 5 6]
-- [9,10,1]
durations :: (Functor f, IntervalSizeable a b)=>
       f (Interval a)
    -> f b
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

-- | Returns a list of the 'IntervalRelation' between each consecutive pair 
--   of intervals. This the specialized form of 'relations'' which can return
--   any 'Applicative', 'Monoid' structure.
--
-- >>> relations [intInt 0 1, intInt 1 2] 
-- [Meets]
relations :: (IntervalAlgebraic a, Foldable f)=>
       f (Interval a)
    -> [IntervalRelation a]
relations = relations'

-- | A generic form of 'relations' which can output any 'Applicative' and 
--   'Monoid' structure. 
-- >>> (relations' [intInt 0 1, intInt 1 2]) :: [IntervalRelation Int]
-- [Meets]
relations' :: ( IntervalAlgebraic a
              , Foldable f
              , Applicative m
              , Monoid (m (IntervalRelation a)) )=>
        f (Interval a)
     -> m (IntervalRelation a)
relations' = foldlAccume relate

-- | Applies 'gaps' to all the non-disjoint intervals in @x@ that are *not* disjoint
-- from @i@. Intervals that 'overlaps' or are 'overlappedBy' @i@ are 'clip'ped 
-- to @i@, so that all the intervals are 'within' @i@. If the input @x@ is empty,
-- then 'mempty' is returned. 
--
-- >>> gapsWithin (intInt 1 10) [intInt 0 5, intInt 7 9, intInt 12 15]
-- [(5, 7),(9, 10)]
gapsWithin :: ( Applicative f
               , Foldable f
               , Monoid (f (Interval a))
               , IntervalSizeable a b
               , IntervalCombinable a
               , IntervalFilterable f a)=>
     Interval a     -- ^ i
  -> f (Interval a) -- ^ x
  -> f (Interval a)
gapsWithin i x 
  | null x = mempty  
  | otherwise = gaps $ pure s <> ivs <> pure e
        where s   = enderval   0 (begin i)
              e   = beginerval 0 (end i)
              nd  = toList (filterNotDisjoint i x)
              ivs = liftListToFoldable (mapMaybe (clip i) nd) 



-- | Given a predicate combinator, a predicate, and list of intervals, returns 
--   the input unchanged if the predicate combinator is @True@. Otherwise, returns
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
--
-- >>> emptyIfNone (starts (intInt 3 5)) [intInt 3 4, intInt 5 6]
-- []
--
-- In the following, (3, 5) 'starts' (3, 6), so the input is returned.
--
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
