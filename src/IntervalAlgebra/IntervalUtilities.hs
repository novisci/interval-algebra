{-|
Module      : Interval Algebra Utilities
Description : Functions for operating on containers of Intervals.
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental

In the examples below, @iv@ is a synonym for 'beginerval' used to save space.
-}

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}

module IntervalAlgebra.IntervalUtilities (
      relations
    , relations'
    , intersect
    , combineIntervals
    , combineIntervals'
    , gaps
    , gaps'
    , durations
    , clip
    , gapsWithin
    , nothingIf
    , nothingIfNone
    , nothingIfAny
    , nothingIfAll

    -- * Operations on Meeting sequences of intervals
    , foldMeetingSafe
    , formMeetingSequence

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

import GHC.Base         ( (<*>), seq)
import GHC.Show         ( Show )
import GHC.Num          ( )
import GHC.Int          ( Int ) 
import Control.Applicative
                        ( Applicative(pure) )
import Data.Bool        ( Bool, otherwise, not )
import Data.Eq          ( Eq((==)) )
import Data.Foldable    ( Foldable(null, foldl', toList), all, any )
import Data.Function    ( ($), (.), flip )
import Data.Functor     ( Functor(fmap) )
import Data.Monoid      ( Monoid(mempty) )
import Data.Maybe       ( Maybe(..), maybe, maybeToList, mapMaybe, catMaybes, fromMaybe )
import Data.List        ( (++), map )
import Data.Ord         ( Ord(min, max) )
import Data.Semigroup   ( Semigroup((<>)) )
import Data.Tuple       ( fst )
import Safe             ( headMay, lastMay, initSafe, tailSafe)
import Witherable       ( Filterable(filter) )
import IntervalAlgebra  ( Interval
                        , Intervallic(..)
                        , IntervalAlgebraic(..)
                        , IntervalCombinable(..)
                        , IntervalSizeable(..)
                        , IntervalRelation(..)
                        , ComparativePredicateOf
                        , beginerval
                        , enderval
                        , extenterval )
import IntervalAlgebra.PairedInterval
                        ( PairedInterval
                        , mkPairedInterval
                        , getPairData
                        , equalPairData )
-------------------------------------------------
-- Unexported utilties used in functions below --
-------------------------------------------------

-- Just a synonym used to examples to save typing
iv :: Int -> Int -> Interval Int
iv = beginerval

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
applyAccume f (fs, Just x)  y = (fs <> pure (f x y), Just y)

-- Lifts a list to a foldable, applicative monoid 
liftListToFoldable :: ( Applicative f
                      , Monoid (f a)
                      , Foldable f) =>
    [a] -> f a
liftListToFoldable = foldl' (\x y -> x <> pure y) mempty

-- Used to combine two lists by combining the last element of @x@ and the first 
-- element of @y@ by @f@. The combining function @f@ will generally return a 
-- singleton list in the case that the last of x and head of y can be combined
-- or a two element list in the case they cannot.
listCombiner :: (Maybe a -> Maybe a -> [a]) -- ^ f
                -> [a] -- ^ x
                -> [a] -- ^ y
                -> [a]
listCombiner f x y = initSafe x ++ f (lastMay x) (headMay y) ++ tailSafe y

-- | Returns a list of the 'IntervalRelation' between each consecutive pair 
--   of intervals. This the specialized form of 'relations'' which can return
--   any 'Applicative', 'Monoid' structure.
--
-- >>> relations [iv 1 0, iv 1 1] 
-- [Meets]
relations :: (IntervalAlgebraic i a, Foldable f)=>
       f (i a)
    -> [IntervalRelation (i a)]
relations = relations'

-- | A generic form of 'relations' which can output any 'Applicative' and 
--   'Monoid' structure. 
-- >>> (relations' [iv 1 0, iv 1 1]) :: [IntervalRelation (Interval Int)]
-- [Meets]
--
relations' :: ( IntervalAlgebraic i a
              , Foldable f
              , Applicative m
              , Monoid (m (IntervalRelation (i a))) )=>
        f (i a)
     -> m (IntervalRelation (i a))
relations' = foldlAccume relate

-- | Forms a 'Just' new interval from the intersection of two intervals, 
--   provided the intervals are not disjoint.
intersect :: (IntervalSizeable a b, IntervalAlgebraic i a) => 
    i a -> i a -> Maybe (Interval a)
intersect x y
    | disjoint x y = Nothing
    | otherwise    = Just $ beginerval (diff e b) b
        where b = max (begin x) (begin y)
              e = min (end x) (end y)

-- | Returns a (possibly empty) container of intervals consisting of the gaps 
--   between intervals in the input. *To work properly, the input should be
--   sorted*. See 'gaps'' for a version that returns a list.
--
-- >>> gaps [iv 4 1, iv 4 8, iv 3 11]
-- [(5, 8)]
gaps :: (IntervalCombinable Interval a
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
gaps' :: (IntervalCombinable Interval a
         , Applicative f
         , Monoid (f (Interval a))
         , Foldable f) =>
      f (Interval a) ->
      [Interval a]
gaps' x = catMaybes (foldlAccume (><) x)

-- | Returns the 'duration' of each 'Intervallic i a' in the 'Functor' @f@.
--
-- >>> durations [iv 9 1, iv 10 2, iv 1 5]
-- [9,10,1]
durations :: (Functor f, Intervallic i a, IntervalSizeable a b)=>
       f (i a)
    -> f b
durations = fmap duration

-- | In the case that x y are not disjoint, clips y to the extent of x.
-- 
-- >>> clip (iv 5 0) (iv 3 3)
-- Just (3, 5)
--
-- >>> clip (iv 3 0) (iv 2 4)
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

-- | Applies 'gaps' to all the non-disjoint intervals in @x@ that are *not* disjoint
-- from @i@. Intervals that 'overlaps' or are 'overlappedBy' @i@ are 'clip'ped 
-- to @i@, so that all the intervals are 'within' @i@. If there are no gaps, then
-- 'Nothing' is returned.
--
-- >>> gapsWithin (iv 9 1) [iv 5 0, iv 2 7, iv 3 12]
-- Just [(5, 7),(9, 10)]
--
gapsWithin :: ( Applicative f
               , Foldable f
               , Monoid (f (Interval a))
               , IntervalSizeable a b
               , IntervalCombinable Interval a
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

-- The Box is an internal type used to hold accumulated, combined intervals in 
-- 'combineIntervals''.
newtype Box a = Box { unBox :: [a] }

packBoxes :: [a] -> [Box a]
packBoxes  = Data.List.map (\z -> Box [z])

instance (Ord a, Show a, IntervalCombinable i a) => Semigroup (Box (i a)) where
    Box x <> Box y = Box $ listCombiner (<->) x y

-- | Returns a container of intervals where any intervals that meet or share support
--   are combined into one interval. *To work properly, the input should 
--   be sorted*. See 'combineIntervals'' for a version that works only on lists.
--
-- >>> combineIntervals [iv 10 0, iv 5 2, iv 2 10, iv 2 13]
-- [(0, 12),(13, 15)]
combineIntervals :: ( IntervalAlgebraic Interval a
                    , Applicative f
                    , Monoid (f (Interval a))
                    , Foldable f ) =>
      f (Interval a) ->
      f (Interval a)
combineIntervals x = liftListToFoldable (combineIntervals' $ toList x)

-- | Returns a list of intervals where any intervals that meet or share support
--   are combined into one interval. *To work properly, the input list should 
--   be sorted*. 
--
-- >>> combineIntervals' [iv 10 0, iv 5 2, iv 2 10, iv 2 13]
-- [(0, 12),(13, 15)]
combineIntervals' :: (IntervalAlgebraic Interval a) => 
        [Interval a] -> [Interval a]
combineIntervals' l = unBox $ foldl' (<>) (Box []) (packBoxes l)

-- Internal function for combining maybe intervals in the 'combineIntervals'' 
-- function
(<->) :: (IntervalCombinable i a) =>
       Maybe (i a)
    -> Maybe (i a)
    -> [i a]
(<->) Nothing Nothing   = []
(<->) Nothing (Just y)  = [y]
(<->) (Just x) Nothing  = [x]
(<->) (Just x) (Just y) = (<+>) x y



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
-- >>> nothingIfNone (starts (iv 2 3)) [iv 1 3, iv 1 5]
-- Nothing
--
-- In the following, (3, 5) 'starts' (3, 6), so 'Just' the input is returned.
--
-- >>> nothingIfNone (starts (iv 2 3)) [iv 3 3, iv 1 5]
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

-- | Folds over a list of Paired Intervals and in the case that the 'getPairData' 
--   is equal between two sequential meeting intervals, these two intervals are 
--   combined into one. This function is "safe" in the sense that if the input is
--   invalid and contains any sequential pairs of intervals with an @IntervalRelation@,
--   other than 'Meets', then the function returns an empty list. 
foldMeetingSafe :: ( IntervalAlgebraic (PairedInterval b) a, Eq b) =>
           [ PairedInterval b a ] -- ^ Be sure this only contains intervals 
                                  --   that sequentially 'meets'.
        -> [ PairedInterval b a ]
foldMeetingSafe l = maybe [] (getMeeting . foldMeeting) (parseMeeting l)

-- | Folds over a list of Meeting Paired Intervals and in the case that the 'getPairData' 
--   is equal between two sequential meeting intervals, these two intervals are 
--   combined into one.  
foldMeeting :: ( IntervalAlgebraic (PairedInterval b) a, Eq b) =>
            Meeting [PairedInterval b a ]
        ->  Meeting [PairedInterval b a ]
foldMeeting (Meeting l) = foldl' joinMeetingPairedInterval (Meeting []) (packMeeting l)

-- This type identifies that @a@ contains intervals that sequentially meet one 
-- another.
newtype Meeting a = Meeting { getMeeting :: a } deriving (Eq, Show)

-- Box up Meeting.
packMeeting :: [a] -> [Meeting [a]]
packMeeting = Data.List.map (\z -> Meeting [z])

-- Test a list of intervals to be sure they all meet; if not return Nothing.
parseMeeting :: (IntervalAlgebraic i a)=> [i a] -> Maybe (Meeting [i a])
parseMeeting x
    | all ( == Meets ) (relations x) = Just $ Meeting x
    | otherwise = Nothing

-- A specific case of 'joinMeeting' for @PairedIntervals@.
joinMeetingPairedInterval :: (Eq b, Ord a, Show a) =>
                  Meeting [PairedInterval b a]
               -> Meeting [PairedInterval b a]
               -> Meeting [PairedInterval b a]
joinMeetingPairedInterval = joinMeeting equalPairData

-- A general function for combining any two @Meeting [i a]@ by 'listCombiner'.
joinMeeting :: (IntervalAlgebraic i a) =>
       ComparativePredicateOf (i a)
    -> Meeting [ i a ]
    -> Meeting [ i a ]
    -> Meeting [ i a ]
joinMeeting f (Meeting x) (Meeting y) = Meeting $ listCombiner (join2MeetingWhen f) x y

-- The intervals @x@ and @y@ should meet! The predicate function @p@ determines
-- when the two intervals that meet should be combined.
join2MeetingWhen :: (IntervalAlgebraic i a) =>
       ComparativePredicateOf (i a)
    -> Maybe (i a)
    -> Maybe (i a)
    -> [i a]
join2MeetingWhen p Nothing Nothing   = []
join2MeetingWhen p Nothing (Just y)  = [y]
join2MeetingWhen p (Just x) Nothing  = [x]
join2MeetingWhen p (Just x) (Just y)
    | p x y      = [ setInterval y (extenterval x y) ]
    | otherwise  =  pure x <> pure y

{- | 
Takes two *ordered* events, x <= y, and "disjoins" them in the case that the
two events have different states, creating a sequence (list) of new events that 
sequentially meet one another. Since x <= y, there are 7 possible interval
relations between x and y. If the states of x and y are equal and x is not 
before y, then x and y are combined into a single event. 
-}
disjoinPaired :: ( Eq b
                 , Monoid b
                 , IntervalAlgebraic (PairedInterval b) a
                 , IntervalSizeable a c) =>
       (PairedInterval b) a
    -> (PairedInterval b) a
    -> Meeting [(PairedInterval b) a]
disjoinPaired o e
   | x `before` y      = Meeting [ x, evp e1 b2 mempty, y ]
   | x `meets` y       = foldMeeting $ Meeting [ x, y ]
   | x `overlaps` y    = foldMeeting $ Meeting [ evp b1 b2 s1, evp b2 e1 sc, evp e1 e2 s2 ]
   | x `finishedBy` y  = foldMeeting $ Meeting [ evp b1 b2 s1, ev i2 sc ]
   | x `contains` y    = foldMeeting $ Meeting [ evp b1 b2 s1, evp b2 e2 sc, evp e2 e1 s1 ]
   | x `starts` y      = foldMeeting $ Meeting [ ev i1 sc, evp e1 e2 s2 ]
   | x `equals` y      = Meeting [ ev i1 sc ]
   where x  = min o e
         y  = max o e
         i1 = getInterval x
         i2 = getInterval y
         s1 = getPairData x
         s2 = getPairData y
         sc = s1 <> s2
         b1 = begin x
         b2 = begin y
         e1 = end x
         e2 = end y
         ev = flip mkPairedInterval
         evp = \b e s -> ev (beginerval (diff e b) b) s

{- | 
The internal function for converting a non-disjoint, ordered sequence of
events into a disjoint, ordered sequence of events. The function operates
by recursion on a pair of events and the input events. The first of the 
is the accumulator set -- the disjoint events that need no longer be 
compared to input events. The second of the pair are disjoint events that
still need to be compared to be input events. 
-}
mtEvt :: ( Monoid b, Eq b, IntervalSizeable a c) =>
       ([(PairedInterval b) a ], [(PairedInterval b) a ])
    -> [(PairedInterval b) a ]
    -> [(PairedInterval b) a ]
mtEvt (acc, o:os) []     = acc ++ o:os           -- the "final" pattern
mtEvt (acc, [])   []     = acc                 -- another "final" pattern 
mtEvt (acc, [])   (e:es) = mtEvt (acc, [e]) es -- the "initialize" pattern
mtEvt (acc, o:os) (e:es)                       -- the "operating" patterns 
     -- If input event is equal to the first comparator, skip the comparison.
    | e == o    = mtEvt (acc, o:os) es

     {- If the period of o is either before or meets the period of e, then 
     the first of the combined events can be put into the accumulator. 
     That is, since the inputs events are ordered, once the beginning of o 
     is before or meets e, then we are assured that all periods up to the 
     beginning of o are fully disjoint and subsequent input events will 
     not overlap these in any way. -}
    | (before <|> meets) o e = mtEvt (acc ++ nh, mtEvt ([], nt) os ) es

    --The standard recursive operation.
    | otherwise = mtEvt (acc,  mtEvt ([], n) os ) es
  where n  = getMeeting $ disjoinPaired o e
        nh = maybeToList (headMay n)
        nt = tailSafe n

{- | 
Convert an ordered sequence of @PairedInterval b a@. that may have any interval relation
('before', 'starts', etc) into a sequence of sequentially meeting @PairedInterval b a@. 
That is, a sequence where one the end of one interval meets the beginning of 
the subsequent event. The 'getPairData' of the input @PairedIntervals@ are
combined using the Monoid '<>' function, hence the pair data must be a 
'Monoid' instance.
-}
formMeetingSequence :: ( Eq b
                       , Monoid b
                       , IntervalSizeable a c) =>
           [ PairedInterval b a ]
        -> [ PairedInterval b a ]
formMeetingSequence x = mtEvt ([], []) (mtEvt ([], []) x) 
   -- the second pass of mtEvt is to handle the situation where the first pass
   -- disjoins all the events correctly into a meeting sequence but -- due to 
   -- nesting of intervals in the input -- some of the sequential pairs have
   -- the same data after the first pass. The second pass merges any sequential
   -- intervals that have the same data.
   --
   -- There is probably a more efficient way to do this.