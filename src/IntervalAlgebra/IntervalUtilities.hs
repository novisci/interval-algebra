{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
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
    , combineIntervals'
    , gaps
    , gaps'
    , durations
    , clip
    , relations
    , relations'
    , gapsWithin
    , nothingIf
    , nothingIfNone
    , nothingIfAny
    , nothingIfAll

    -- * Filtering functions
    , filterBefore
    , filterMeets
    , filterOverlaps
    , filterFinishedBy
    , filterContains
    , filterStarts
    , filterEquals
    , filterStartedBy
    , filterDuring
    , filterFinishes
    , filterOverlappedBy
    , filterMetBy
    , filterAfter
    , filterDisjoint
    , filterNotDisjoint
    , filterWithin

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
    , IntervalRelation(..)
    , ComparativePredicateOf
    , unsafeInterval
    , beginerval
    , enderval
    )
import Data.Maybe (mapMaybe, catMaybes, fromMaybe, Maybe(..))
import Data.List ( (++), map, head, init, last, tail )
import Witherable ( Filterable(filter) )

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
clip :: (IntervalAlgebraic Interval a, IntervalSizeable a b)=>
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
relations :: (IntervalAlgebraic Interval a, Foldable f)=>
       f (Interval a)
    -> [IntervalRelation (Interval a)]
relations = relations'

-- | A generic form of 'relations' which can output any 'Applicative' and 
--   'Monoid' structure. 
-- >>> (relations' [intInt 0 1, intInt 1 2]) :: [IntervalRelation Int]
-- [Meets]
relations' :: ( IntervalAlgebraic Interval a
              , Foldable f
              , Applicative m
              , Monoid (m (IntervalRelation (Interval a))) )=>
        f (Interval a)
     -> m (IntervalRelation (Interval a))
relations' = foldlAccume relate

-- | Applies 'gaps' to all the non-disjoint intervals in @x@ that are *not* disjoint
-- from @i@. Intervals that 'overlaps' or are 'overlappedBy' @i@ are 'clip'ped 
-- to @i@, so that all the intervals are 'within' @i@. If there are no gaps, then
-- 'Nothing' is returned.
--
-- >>> gapsWithin (intInt 1 10) [intInt 0 5, intInt 7 9, intInt 12 15]
-- Just [(5, 7),(9, 10)]
--
gapsWithin :: ( Applicative f
               , Foldable f
               , Monoid (f (Interval a))
               , IntervalSizeable a b
               , IntervalCombinable a
               , Filterable f
               , IntervalAlgebraic Interval a)=>
     Interval a     -- ^ i
  -> f (Interval a) -- ^ x
  -> Maybe (f (Interval a))
gapsWithin i x 
  | null ivs  = Nothing  
  | otherwise = Just $ gaps $ pure s <> ivs <> pure e
        where s   = enderval   0 (begin i)
              e   = beginerval 0 (end i)
              nd  = toList (filterNotDisjoint i x)
              ivs = liftListToFoldable (mapMaybe (clip i) nd) 

-- | Given a predicate combinator, a predicate, and list of intervals, returns 
--   the input unchanged if the predicate combinator is @True@. Otherwise, returns
--   an empty list. See 'nothingIfAny' and 'nothingIfNone' for examples.
nothingIf :: (Monoid (f (Interval a)), Filterable f, IntervalAlgebraic Interval a)=>
     ((Interval a -> Bool) -> f (Interval a) -> Bool) -- ^ e.g. 'any' or 'all'
  -> (Interval a -> Bool) -- ^ predicate to apply to each element of input list
  -> f (Interval a)
  -> Maybe (f (Interval a))
nothingIf quantifier predicate x = if quantifier predicate x then Nothing else Just x

-- | Returns the empty monoid structure if *none* of the element of input satisfy
--   the predicate condition.
-- 
-- For example, the following returns the empty list because none of the intervals
-- in the input list 'starts' (3, 5).
--
-- >>> nothingIfNone (starts (intInt 3 5)) [intInt 3 4, intInt 5 6]
-- Nothing
--
-- In the following, (3, 5) 'starts' (3, 6), so the input is returned.
--
-- >>> nothingIfNone (starts (intInt 3 5)) [intInt 3 6, intInt 5 6]
-- Just [(3, 6),(5, 6)]
--
nothingIfNone :: (Monoid (f (Interval a)), Foldable f, Filterable f, IntervalAlgebraic Interval a)=>
    (Interval a -> Bool) -- ^ predicate to apply to each element of input list
  -> f (Interval a)
  -> Maybe (f (Interval a))
nothingIfNone = nothingIf (\f x -> (not.any f) x)

-- | Returns the empty monoid structure if *any* of the element of input satisfy
--   the predicate condition
nothingIfAny :: (Monoid (f (Interval a)), Foldable f, Filterable f, IntervalAlgebraic Interval a)=>
    (Interval a -> Bool) -- ^ predicate to apply to each element of input list
  -> f (Interval a)
  -> Maybe (f (Interval a))
nothingIfAny = nothingIf any

-- | Returns the empty monoid structure if *all* of the element of input satisfy
--   the predicate condition
nothingIfAll :: (Monoid (f (Interval a)), Foldable f, Filterable f, IntervalAlgebraic Interval a)=>
    (Interval a -> Bool) -- ^ predicate to apply to each element of input list
  -> f (Interval a)
  -> Maybe (f (Interval a))
nothingIfAll = nothingIf all


{- | 
Filter functions provides means for filtering 'Filterable' containers of 
@'Interval'@s based on @'IntervalAlgebraic'@ relations.
-}

-- |Creates a function for filtering a 'Witherable.Filterable' of @Interval a@s based on a predicate
filterMaker :: (Filterable f, IntervalAlgebraic Interval a) =>
                 ComparativePredicateOf (Interval a)
                -> Interval a
                -> (f (Interval a) -> f (Interval a))
filterMaker f p = Witherable.filter (`f` p)

-- | Filter a 'Witherable.Filterable' of @Interval a@s to those that 'overlaps' the @Interval a@
--   in the first argument.
filterOverlaps :: (Filterable f, IntervalAlgebraic Interval a) => 
                  Interval a -> f (Interval a) -> f (Interval a)
filterOverlaps = filterMaker overlaps

-- | Filter a 'Witherable.Filterable' of @Interval a@s to those 'overlappedBy' the @Interval a@
--   in the first argument.
filterOverlappedBy :: (Filterable f, IntervalAlgebraic Interval a) => 
                      Interval a -> f (Interval a) -> f (Interval a)
filterOverlappedBy = filterMaker overlappedBy

-- | Filter a 'Witherable.Filterable' of Interval as to those 'before' the @Interval a@
--   in the first argument.
filterBefore :: (Filterable f, IntervalAlgebraic Interval a) => 
                Interval a -> f (Interval a) -> f (Interval a)
filterBefore = filterMaker before

-- | Filter a 'Witherable.Filterable' of Interval as to those 'after' the @Interval a@
--   in the first argument.
filterAfter :: (Filterable f, IntervalAlgebraic Interval a) => 
               Interval a -> f (Interval a) -> f (Interval a)
filterAfter = filterMaker after

-- | Filter a 'Witherable.Filterable' of Interval as to those 'starts' the @Interval a@
--   in the first argument.
filterStarts :: (Filterable f, IntervalAlgebraic Interval a) => 
               Interval a -> f (Interval a) -> f (Interval a)
filterStarts = filterMaker starts 

-- | Filter a 'Witherable.Filterable' of Interval as to those 'startedBy' the @Interval a@
--   in the first argument.
filterStartedBy :: (Filterable f, IntervalAlgebraic Interval a) => 
               Interval a -> f (Interval a) -> f (Interval a)
filterStartedBy = filterMaker startedBy 

-- | Filter a 'Witherable.Filterable' of Interval as to those 'finishes' the @Interval a@
--   in the first argument.
filterFinishes :: (Filterable f, IntervalAlgebraic Interval a) => 
               Interval a -> f (Interval a) -> f (Interval a)
filterFinishes = filterMaker finishes

-- | Filter a 'Witherable.Filterable' of Interval as to those 'finishedBy' the @Interval a@
--   in the first argument.
filterFinishedBy :: (Filterable f, IntervalAlgebraic Interval a) => 
               Interval a -> f (Interval a) -> f (Interval a)
filterFinishedBy = filterMaker finishedBy 

-- | Filter a 'Witherable.Filterable' of Interval as to those that 'meets' the @Interval a@
--   in the first argument.
filterMeets :: (Filterable f, IntervalAlgebraic Interval a) => 
               Interval a -> f (Interval a) -> f (Interval a)
filterMeets = filterMaker meets

-- | Filter a 'Witherable.Filterable' of Interval as to those 'metBy' the @Interval a@
--   in the first argument.
filterMetBy :: (Filterable f, IntervalAlgebraic Interval a) => 
               Interval a -> f (Interval a) -> f (Interval a)
filterMetBy = filterMaker metBy

-- | Filter a 'Witherable.Filterable' of Interval as to those 'during' the @Interval a@
--   in the first argument.
filterDuring :: (Filterable f, IntervalAlgebraic Interval a) => 
                Interval a -> f (Interval a) -> f (Interval a)
filterDuring = filterMaker during

-- | Filter a 'Witherable.Filterable' of Interval as to those that 'contains'
--   the @Interval a@ in the first argument.
filterContains :: (Filterable f, IntervalAlgebraic Interval a) => 
                  Interval a -> f (Interval a) -> f (Interval a)
filterContains = filterMaker contains

-- | Filter a 'Witherable.Filterable' of Interval as to those that 'equals'
--   the @Interval a@ in the first argument.
filterEquals :: (Filterable f, IntervalAlgebraic Interval a) => 
                  Interval a -> f (Interval a) -> f (Interval a)
filterEquals = filterMaker equals

-- | Filter a 'Witherable.Filterable' of Interval as to those that are 'disjoint'
--   from the @Interval a@ in the first argument.
filterDisjoint :: (Filterable f, IntervalAlgebraic Interval a) => 
                  Interval a -> f (Interval a) -> f (Interval a)
filterDisjoint = filterMaker disjoint

-- | Filter a 'Witherable.Filterable' of Interval as to those that are 'notDisjoint'
--   from the @Interval a@ in the first argument.
filterNotDisjoint :: (Filterable f, IntervalAlgebraic Interval a) => 
                     Interval a -> f (Interval a) -> f (Interval a)
filterNotDisjoint = filterMaker notDisjoint

-- | Filter a 'Witherable.Filterable' of Interval as to those that are 'within'
--   the @Interval a@ in the first argument.
filterWithin :: (Filterable f, IntervalAlgebraic Interval a) => 
                Interval a -> f (Interval a) -> f (Interval a)
filterWithin = filterMaker disjoint
