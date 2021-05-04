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
    , compareIntervals
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
    , filterConcur
    , filterWithin
    , filterEnclose
    , filterEnclosedBy

) where

import GHC.Base
    ( otherwise, ($), (.), (<*>), seq, not
    , Semigroup((<>))
    , Functor(fmap)
    , Applicative(pure)
    , Int, Bool, Ord)
import GHC.Show ( Show )
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

-- | Returns the 'duration' of each 'Intervallic i a' in the 'Functor' @f@.
--
-- >>> durations [intInt 1 10, intInt 2 12, intInt 5 6]
-- [9,10,1]
durations :: (Functor f, Intervallic i a, IntervalSizeable a b)=>
       f (i a)
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
relations :: (IntervalAlgebraic i a, Foldable f)=>
       f (i a)
    -> [IntervalRelation (i a)]
relations = relations'

-- | A generic form of 'relations' which can output any 'Applicative' and 
--   'Monoid' structure. 
-- >>> (relations' [intInt 0 1, intInt 1 2]) :: [IntervalRelation Int]
-- [Meets]
relations' :: ( IntervalAlgebraic i a
              , Foldable f
              , Applicative m
              , Monoid (m (IntervalRelation (i a))) )=>
        f (i a)
     -> m (IntervalRelation (i a))
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
nothingIf :: (Monoid (f (i a)), Filterable f, IntervalAlgebraic i a)=>
     ((i a -> Bool) -> f (i a) -> Bool) -- ^ e.g. 'any' or 'all'
  -> (i a -> Bool) -- ^ predicate to apply to each element of input list
  -> f (i a)
  -> Maybe (f (i a))
nothingIf quantifier predicate x = if quantifier predicate x then Nothing else Just x

-- | Returns the 'Nothing' if *none* of the element of input satisfy
--   the predicate condition.
-- 
-- For example, the following returns 'Nothing' because none of the intervals
-- in the input list 'starts' (3, 5).
--
-- >>> nothingIfNone (starts (intInt 3 5)) [intInt 3 4, intInt 5 6]
-- Nothing
--
-- In the following, (3, 5) 'starts' (3, 6), so 'Just' the input is returned.
--
-- >>> nothingIfNone (starts (intInt 3 5)) [intInt 3 6, intInt 5 6]
-- Just [(3, 6),(5, 6)]
--
nothingIfNone :: (Monoid (f (i a)), Foldable f, Filterable f, IntervalAlgebraic i a)=>
    (i a -> Bool) -- ^ predicate to apply to each element of input list
  -> f (i a)
  -> Maybe (f (i a))
nothingIfNone = nothingIf (\f x -> (not.any f) x)

-- | Returns 'Nothing' if *any* of the element of input satisfy the predicate condition.
nothingIfAny :: (Monoid (f (i a)), Foldable f, Filterable f, IntervalAlgebraic i a)=>
    (i a -> Bool) -- ^ predicate to apply to each element of input list
  -> f (i a)
  -> Maybe (f (i a))
nothingIfAny = nothingIf any

-- | Returns 'Nothing' if *all* of the element of input satisfy the predicate condition
nothingIfAll :: (Monoid (f (i a)), Foldable f, Filterable f, IntervalAlgebraic i a)=>
    (i a -> Bool) -- ^ predicate to apply to each element of input list
  -> f (i a)
  -> Maybe (f (i a))
nothingIfAll = nothingIf all

{- | 
Filter functions provides means for filtering 'Filterable' containers of 
@'Intervallic i a'@s based on @'IntervalAlgebraic'@ relations.
-}

-- | Lifts a predicate to be able to compare two different 'IntervalAlgebraic' 
--   structure by comparing the intervals contain within each. 
compareIntervals :: (IntervalAlgebraic i0 a, IntervalAlgebraic i1 a) =>
   ComparativePredicateOf (Interval a) 
    -> i0 a
    -> i1 a
    -> Bool
compareIntervals pf x y = pf (getInterval x) (getInterval y)

-- | Creates a function for filtering a 'Witherable.Filterable' of @i1 a@s 
--   by comparing the @Interval a@s that of an @i0 a@. 
filterMaker :: (Filterable f
                , IntervalAlgebraic Interval a
                , IntervalAlgebraic i0 a
                , IntervalAlgebraic i1 a) =>
        ComparativePredicateOf (Interval a)
      -> i0 a
      -> (f (i1 a) -> f (i1 a))
filterMaker f p = Witherable.filter (compareIntervals f p)

-- | Filter by 'overlaps'.
filterOverlaps :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterOverlaps = filterMaker overlaps

-- | Filter by 'overlappedBy'.
filterOverlappedBy :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterOverlappedBy = filterMaker overlappedBy

-- | Filter by 'before'.
filterBefore :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterBefore = filterMaker before

-- | Filter by 'after'.
filterAfter :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a) 
filterAfter = filterMaker after

-- | Filter by 'starts'.
filterStarts :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterStarts = filterMaker starts 

-- | Filter by 'startedBy'.
filterStartedBy :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterStartedBy = filterMaker startedBy 

-- | Filter by 'finishes'.
filterFinishes :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterFinishes = filterMaker finishes

-- | Filter by'finishedBy'.
filterFinishedBy :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterFinishedBy = filterMaker finishedBy 

-- | Filter by 'meets'.
filterMeets :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterMeets = filterMaker meets

-- | Filter by 'metBy'.
filterMetBy :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterMetBy = filterMaker metBy

-- | Filter by 'during'.
filterDuring :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterDuring = filterMaker during

-- | Filter by 'contains'.
filterContains :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterContains = filterMaker contains

-- | Filter by 'equals'.
filterEquals :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterEquals = filterMaker equals

-- | Filter by 'disjoint'.
filterDisjoint :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterDisjoint = filterMaker disjoint

-- | Filter by 'notDisjoint'.
filterNotDisjoint :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterNotDisjoint = filterMaker notDisjoint

-- | Filter by 'concur'.
filterConcur ::  (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterConcur = filterMaker concur

-- | Filter by 'within'.
filterWithin :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterWithin = filterMaker within

-- | Filter by 'enclose'.
filterEnclose :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterEnclose = filterMaker enclose

-- | Filter by 'enclosedBy'.
filterEnclosedBy :: (Filterable f
                  , IntervalAlgebraic Interval a
                  , IntervalAlgebraic i0 a
                  , IntervalAlgebraic i1 a) => 
                  i0 a -> f (i1 a) -> f (i1 a)
filterEnclosedBy = filterMaker enclosedBy
