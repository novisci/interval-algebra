{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

-- |
-- Module      : Interval Algebra
-- Description : Implementation of Allen's interval algebra
-- Copyright   : (c) NoviSci, Inc 2020-2022
--                   TargetRWE, 2023
-- License     : BSD3
-- Maintainer  : bsaul@novisci.com 2020-2022, bbrown@targetrwe.com 2023
--
-- The @IntervalAlgebra@ module provides data types and related classes for the
-- interval-based temporal logic described in [Allen (1983)](https://doi.org/10.1145/182.358434)
-- and axiomatized in [Allen and Hayes (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x).
-- A good primer on Allen's algebra can be [found here](https://thomasalspaugh.org/pub/fnd/allen.html).
--
-- = Design
--
-- The module is built around three typeclasses designed to separate concerns of
-- constructing, relating, and combining types that contain @'Interval'@s:
--
-- 1. @'Intervallic'@ provides an interface to the data structures which contain an
--    @'Interval'@.
-- 2. @'SizedIv'@ provides a generic interface for creating and manipulating intervals.
--
-- 'SizedIv' types can be related via one of the 'IntervalRelation' s,
-- and 'Intervallic' types can be related via their underlying 'Interval' s, so
-- long as those are 'SizedIv'.
--
-- Many exports of this module require `FlexibleContexts` and `TypeFamilies`
-- extensions to be enabled.
module IntervalAlgebra.Core
  ( -- * Intervals
    Interval,
    SizedIv (..),
    Iv (..),
    PointedIv (..),
    Intervallic (..),
    ParseErrorInterval (..),
    begin,
    end,

    -- ** Create new intervals
    parseInterval,
    prsi,
    beginerval,
    bi,
    enderval,
    ei,
    safeInterval,
    si,

    -- ** Modify intervals within an @Intervallic@
    expand,
    expandl,
    expandr,

    -- * Interval Algebra

    -- ** Interval Relations and Predicates
    IntervalRelation (..),
    meets,
    metBy,
    before,
    after,
    overlaps,
    overlappedBy,
    finishedBy,
    finishes,
    contains,
    during,
    starts,
    startedBy,
    equals,

    -- ** Additional predicates and utilities
    precedes,
    precededBy,
    disjoint,
    notDisjoint,
    concur,
    within,
    encloses,
    enclosedBy,
    (<|>),
    predicate,
    unionPredicates,
    disjointRelations,
    withinRelations,
    strictWithinRelations,
    ComparativePredicateOf1,
    ComparativePredicateOf2,
    beginervalFromEnd,
    endervalFromBegin,
    beginervalMoment,
    endervalMoment,
    shiftFromBegin,
    shiftFromEnd,
    momentize,
    toEnumInterval,
    fromEnumInterval,

    -- ** Algebraic operations
    intervalRelations,
    relate,
    compose,
    complement,
    union,
    intersection,
    converse,
    converseRelation,

    -- * Combine two intervals
    extenterval,
  )
where

import           Control.Applicative (Applicative (pure), liftA2)
import           Control.DeepSeq     (NFData)
import           Data.Binary         (Binary)
import           Data.Fixed          (Pico)
import           Data.Function       (flip, id, ($), (.))
import           Data.Kind           (Type)
import           Data.Ord            (Ord (..), Ordering (..), max, min)
import           Data.Semigroup      (Semigroup ((<>)))
import qualified Data.Set            (Set, difference, fromList, intersection,
                                      map, toList, union)
import           Data.Time           as DT (Day, DiffTime, NominalDiffTime,
                                            UTCTime, addDays, addUTCTime,
                                            diffDays, diffUTCTime,
                                            nominalDiffTimeToSeconds,
                                            secondsToNominalDiffTime)
import           Data.Tuple          (fst, snd)
import           GHC.Generics        (Generic)
import           GHC.IO.Handle       (NewlineMode (inputNL))
import           Test.QuickCheck     (Arbitrary (..), resize, sized, suchThat)
import           Witch.Utility       (as)

-- $setup
-- >>> import IntervalAlgebra.IntervalDiagram
-- >>> :set -XTypeFamilies

-- | An @'Interval' a@ is a pair \( (x, y) \text{ such that } x < y\). To create
-- intervals use the @'parseInterval'@, @'beginerval'@, or @'enderval'@ functions.
newtype Interval a
  = Interval (a, a)
  deriving (Eq, Generic)

-- | A type identifying interval parsing errors.
newtype ParseErrorInterval
  = ParseErrorInterval String
  deriving (Eq, Show)

-- | Helper defining what a valid relation is between begin and end of an
-- Interval.
isValidBeginEnd :: (Ord a) => a -> a -> Bool
isValidBeginEnd b e = b < e

-- | Parse a pair of @a@s to create an @'Interval' a@. Note this
-- checks only that @begin < end@ and has no relation to checking
-- the conditions of 'SizedIv'.
--
-- >>> parseInterval 0 1
-- Right (0, 1)
--
-- >>> parseInterval 1 0
-- Left (ParseErrorInterval "0<=1")
parseInterval ::
  (Show a, Ord a) => a -> a -> Either ParseErrorInterval (Interval a)
parseInterval x y
  | isValidBeginEnd x y = Right $ Interval (x, y)
  | otherwise = Left $ ParseErrorInterval $ show y ++ "<=" ++ show x

-- | A synonym for `parseInterval`
prsi :: (Show a, Ord a) => a -> a -> Either ParseErrorInterval (Interval a)
prsi = parseInterval

instance (Show a, Ord a) => Show (Interval a) where
  show (Interval x) = "(" ++ show (fst x) ++ ", " ++ show (snd x) ++ ")"

instance Binary a => Binary (Interval a)

instance NFData a => NFData (Interval a)

{- INTERVALLIC -}

-- | The @'Intervallic'@ typeclass defines how to get and set the 'Interval'
-- content of a data structure. 'Intervallic' types can be compared via
-- 'IntervalRelation' s on their underlying 'Interval', and functions of this
-- module define versions of the methods from 'Iv', 'PointedIv' and 'SizedIv'
-- for instances of 'Intervallic' by applying them to the contained interval.
--
-- Only the canonical representation @'Interval'@ should define an instance of all four
-- classes.
--
-- 'PairedInterval' is the prototypical example of an 'Intervallic'.
--
-- >>> getInterval (Interval (0, 10))
-- (0, 10)
--
-- >>> begin (Interval (0, 10))
-- 0
--
-- >>> end (Interval (0, 10))
-- 10
class Intervallic i where
  -- | Get the interval from an @i a@.
  getInterval :: i a -> Interval a

  -- | Set the interval in an @i a@.
  setInterval :: i a -> Interval b -> i b

-- | Access the endpoints of an @i a@ .
begin, end :: forall i a. (SizedIv (Interval a), Intervallic i) => i a -> a
begin = ivBegin . getInterval
end = ivEnd . getInterval

-- | This *unexported* function is an internal convenience function for cases in
-- which @f@ is known to be strictly monotone.
imapStrictMonotone :: (Intervallic i) => (a -> b) -> i a -> i b
imapStrictMonotone f i = setInterval i (op f (getInterval i))
  where
    op f (Interval (b, e)) = Interval (f b, f e)

{- RELATIONS -}

-- | The 'IntervalRelation' type and the associated predicate functions enumerate
-- the thirteen possible ways that two @'SizedIv'@ objects may 'relate'
-- according to Allen's interval algebra. Constructors are shown with their
-- corresponding predicate function.
data IntervalRelation
  = -- | `before`
    Before
  | -- | `meets`
    Meets
  | -- | `overlaps`
    Overlaps
  | -- | `finishedBy`
    FinishedBy
  | -- | `contains`
    Contains
  | -- | `starts`
    Starts
  | -- | `equals`
    Equals
  | -- | `startedBy`
    StartedBy
  | -- | `during`
    During
  | -- | `finishes`
    Finishes
  | -- | `overlappedBy`
    OverlappedBy
  | -- | `metBy`
    MetBy
  | -- | `after`
    After
  deriving (Enum, Eq, Show)

instance Bounded IntervalRelation where
  minBound = Before
  maxBound = After

instance Ord IntervalRelation where
  compare x y = compare (fromEnum x) (fromEnum y)

-- | Does x `meets` y? Is x `metBy` y?
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 5 0
-- >>> y = bi 5 5
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
-- -----      <- [x]
--      ----- <- [y]
-- ==========
--
-- Examples:
--
-- >>> x `meets` y
-- True
--
-- >>> x `metBy` y
-- False
--
-- >>> y `meets` x
-- False
--
-- >>> y `metBy` x
-- True
meets,
  metBy ::
    (Iv (Interval a), Intervallic i0, Intervallic i1) =>
    ComparativePredicateOf2 (i0 a) (i1 a)
meets x y = ivMeets (getInterval x) (getInterval y)
metBy = flip meets

-- | Is x `before` y? Does x `precedes` y? Is x `after` y? Is x `precededBy` y?
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 3 0
-- >>> y = bi 4 6
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
-- ---        <- [x]
--       ---- <- [y]
-- ==========
--
-- Examples:
--
-- >>> x `before` y
-- True
-- >>> x `precedes` y
-- True
--
-- >>> x `after`y
-- False
-- >>> x `precededBy` y
-- False
--
-- >>> y `before` x
-- False
-- >>> y `precedes` x
-- False
--
-- >>> y `after` x
-- True
-- >>> y `precededBy` x
-- True
before,
  after,
  precedes,
  precededBy ::
    (Iv (Interval a), Intervallic i0, Intervallic i1) =>
    ComparativePredicateOf2 (i0 a) (i1 a)
before x y = ivBefore (getInterval x) (getInterval y)
after = flip before
precedes = before
precededBy = after

-- | Aliases for 'ivBefore' and 'ivAfter'.
ivPrecedes, ivPrecededBy :: (Iv iv) => iv -> iv -> Bool
ivPrecedes = ivBefore
ivPrecededBy = ivAfter

-- | Does x `overlaps` y? Is x `overlappedBy` y?
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 6 0
-- >>> y = bi 6 4
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
-- ------     <- [x]
--     ------ <- [y]
-- ==========
--
-- Examples:
--
-- >>> x `overlaps` y
-- True
--
-- >>> x `overlappedBy` y
-- False
--
-- >>> y `overlaps` x
-- False
--
-- >>> y `overlappedBy` x
-- True
overlaps,
  overlappedBy ::
    (Iv (Interval a), Intervallic i0, Intervallic i1) =>
    ComparativePredicateOf2 (i0 a) (i1 a)
overlaps x y = ivOverlaps (getInterval x) (getInterval y)
overlappedBy = flip overlaps

-- | Does x `starts` y? Is x `startedBy` y?
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 3 4
-- >>> y = bi 6 4
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
--     ---    <- [x]
--     ------ <- [y]
-- ==========
--
-- Examples:
--
-- >>> x `starts` y
-- True
--
-- >>> x `startedBy` y
-- False
--
-- >>> y `starts` x
-- False
--
-- >>> y `startedBy` x
-- True
starts,
  startedBy ::
    (Iv (Interval a), Intervallic i0, Intervallic i1) =>
    ComparativePredicateOf2 (i0 a) (i1 a)
starts x y = ivStarts (getInterval x) (getInterval y)
startedBy = flip starts

-- | Does x `finishes` y? Is x `finishedBy` y?
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 3 7
-- >>> y = bi 6 4
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
--        --- <- [x]
--     ------ <- [y]
-- ==========
--
-- Examples:
--
-- >>> x `finishes` y
-- True
--
-- >>> x `finishedBy` y
-- False
--
-- >>> y `finishes` x
-- False
--
-- >>> y `finishedBy` x
-- True
finishes,
  finishedBy ::
    (Iv (Interval a), Intervallic i0, Intervallic i1) =>
    ComparativePredicateOf2 (i0 a) (i1 a)
finishes x y = ivFinishes (getInterval x) (getInterval y)
finishedBy = flip finishes

-- | Is x `during` y? Does x `contains` y?
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 3 5
-- >>> y = bi 6 4
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
--      ---   <- [x]
--     ------ <- [y]
-- ==========
--
-- Examples:
--
-- >>> x `during` y
-- True
--
-- >>> x `contains` y
-- False
--
-- >>> y `during` x
-- False
--
-- >>> y `contains` x
-- True
during,
  contains ::
    (Iv (Interval a), Intervallic i0, Intervallic i1) =>
    ComparativePredicateOf2 (i0 a) (i1 a)
during x y = ivDuring (getInterval x) (getInterval y)
contains = flip during

-- | Does x `equals` y?
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 6 4
-- >>> y = bi 6 4
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
--     ------ <- [x]
--     ------ <- [y]
-- ==========
--
-- Examples:
--
-- >>> x `equals` y
-- True
--
-- >>> y `equals` x
-- True
equals ::
  (Iv (Interval a), Intervallic i0, Intervallic i1) =>
  ComparativePredicateOf2 (i0 a) (i1 a)
equals x y = ivEquals (getInterval x) (getInterval y)

{- Intervallic-specific relation utilities -}

-- | Operator for composing the union of two predicates on 'Intervallic' s.
(<|>) ::
  (Intervallic i0, Intervallic i1) =>
  ComparativePredicateOf2 (i0 a) (i1 a) ->
  ComparativePredicateOf2 (i0 a) (i1 a) ->
  ComparativePredicateOf2 (i0 a) (i1 a)
(<|>) f g = unionPredicates [f, g]

-- | The set of @IntervalRelation@ meaning two intervals are disjoint.
disjointRelations :: Data.Set.Set IntervalRelation
disjointRelations = toSet [Before, After, Meets, MetBy]

-- | The set of @IntervalRelation@ meaning one interval is within the other.
withinRelations :: Data.Set.Set IntervalRelation
withinRelations = toSet [Starts, During, Finishes, Equals]

-- | The set of @IntervalRelation@ meaning one interval is *strictly* within the other.
strictWithinRelations :: Data.Set.Set IntervalRelation
strictWithinRelations = Data.Set.difference withinRelations (toSet [Equals])

-- | Are x and y `disjoint` ('before', 'after', 'meets', or 'metBy')?
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 3 0
-- >>> y = bi 3 5
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
-- ---      <- [x]
--      --- <- [y]
-- ========
--
-- Examples:
--
-- >>> x `disjoint` y
-- True
--
-- >>> y `disjoint` x
-- True
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 3 0
-- >>> y = bi 3 3
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
-- ---    <- [x]
--    --- <- [y]
-- ======
--
-- Examples:
--
-- >>> x `disjoint` y
-- True
--
-- >>> y `disjoint` x
-- True
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 6 0
-- >>> y = bi 3 3
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
-- ------ <- [x]
--    --- <- [y]
-- ======
--
-- Examples:
--
-- >>> x `disjoint` y
-- False
--
-- >>> y `disjoint` x
-- False
disjoint ::
  (SizedIv (Interval a), Ord a, Intervallic i0, Intervallic i1) =>
  ComparativePredicateOf2 (i0 a) (i1 a)
disjoint = predicate disjointRelations

-- | Does x `concur` with y? Is x `notDisjoint` with y?); This is
-- the 'complement' of 'disjoint'.
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 3 0
-- >>> y = bi 3 4
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
-- ---     <- [x]
--     --- <- [y]
-- =======
--
-- Examples:
--
-- >>> x `notDisjoint` y
-- False
-- >>> y `concur` x
-- False
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 3 0
-- >>> y = bi 3 3
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
-- ---    <- [x]
--    --- <- [y]
-- ======
--
-- Examples:
--
-- >>> x `notDisjoint` y
-- False
-- >>> y `concur` x
-- False
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 6 0
-- >>> y = bi 3 3
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
-- ------ <- [x]
--    --- <- [y]
-- ======
--
-- Examples:
--
-- >>> x `notDisjoint` y
-- True
-- >>> y `concur` x
-- True
notDisjoint,
  concur ::
    (SizedIv (Interval a), Ord a, Intervallic i0, Intervallic i1) =>
    ComparativePredicateOf2 (i0 a) (i1 a)
notDisjoint = predicate (complement disjointRelations)
concur = notDisjoint

-- | Is x `within` (`enclosedBy`) y? That is, 'during', 'starts', 'finishes', or
-- 'equals'?
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 6 4
-- >>> y = bi 6 4
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
--     ------ <- [x]
--     ------ <- [y]
-- ==========
--
-- Examples:
--
-- >>> x `within` y
-- True
--
-- >>> y `enclosedBy` x
-- True
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 6 4
-- >>> y = bi 5 4
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
--     ------ <- [x]
--     -----  <- [y]
-- ==========
--
-- Examples:
--
-- >>> x `within` y
-- False
--
-- >>> y `enclosedBy` x
-- True
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 6 4
-- >>> y = bi 4 5
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
--     ------ <- [x]
--      ----  <- [y]
-- ==========
--
-- Examples:
--
-- >>> x `within` y
-- False
-- >>> y `enclosedBy` x
-- True
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 2 7
-- >>> y = bi 1 5
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
--        -- <- [x]
--      -    <- [y]
-- =========
--
-- Examples:
--
-- >>> x `within` y
-- False
--
-- >>> y `enclosedBy` x
-- False
within,
  enclosedBy ::
    (SizedIv (Interval a), Ord a, Intervallic i0, Intervallic i1) =>
    ComparativePredicateOf2 (i0 a) (i1 a)
within = predicate withinRelations
enclosedBy = within

-- | Does x `encloses` y? That is, is y 'within' x?
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 6 4
-- >>> y = bi 6 4
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
--     ------ <- [x]
--     ------ <- [y]
-- ==========
--
-- Examples:
--
-- >>> x `encloses` y
-- True
--
-- >>> y `encloses` x
-- True
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 6 4
-- >>> y = bi 5 4
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
--     ------ <- [x]
--     -----  <- [y]
-- ==========
--
-- Examples:
--
-- >>> x `encloses` y
-- True
--
-- >>> y `encloses` x
-- False
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 6 4
-- >>> y = bi 4 5
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
--     ------ <- [x]
--      ----  <- [y]
-- ==========
--
-- Examples:
--
-- >>> x `encloses` y
-- True
--
-- >>> y `encloses` x
-- False
--
-- Example data with corresponding diagram:
--
-- >>> x = bi 2 7
-- >>> y = bi 1 5
-- >>> pretty $ standardExampleDiagram [(x, "x"), (y, "y")] []
--        -- <- [x]
--      -    <- [y]
-- =========
--
-- Examples:
--
-- >>> x `encloses` y
-- False
--
-- >>> y `encloses` x
-- False
encloses ::
  (SizedIv (Interval a), Ord a, Intervallic i0, Intervallic i1) =>
  ComparativePredicateOf2 (i0 a) (i1 a)
encloses = flip enclosedBy

-- | The 'Data.Set.Set' of all 'IntervalRelation's.
intervalRelations :: Data.Set.Set IntervalRelation
intervalRelations =
  Data.Set.fromList (Prelude.map toEnum [0 .. 12] :: [IntervalRelation])

-- | Find the converse of a single 'IntervalRelation'
converseRelation :: IntervalRelation -> IntervalRelation
converseRelation x = toEnum (12 - fromEnum x)

-- | Shortcut to creating a 'Set IntervalRelation' from a list.
toSet :: [IntervalRelation] -> Data.Set.Set IntervalRelation
toSet = Data.Set.fromList

-- | Compose a list of interval relations with _or_ to create a new
-- @'ComparativePredicateOf1' i a@. For example,
-- @unionPredicates [before, meets]@ creates a predicate function determining
-- if one interval is either before or meets another interval.
unionPredicates :: [ComparativePredicateOf2 a b] -> ComparativePredicateOf2 a b
unionPredicates fs x y = any (\f -> f x y) fs

-- | Maps an 'IntervalRelation' to its corresponding predicate function.
toPredicate ::
  (SizedIv (Interval a), Ord a, Intervallic i0, Intervallic i1) =>
  IntervalRelation ->
  ComparativePredicateOf2 (i0 a) (i1 a)
toPredicate r = case r of
  Before       -> before
  Meets        -> meets
  Overlaps     -> overlaps
  FinishedBy   -> finishedBy
  Contains     -> contains
  Starts       -> starts
  Equals       -> equals
  StartedBy    -> startedBy
  During       -> during
  Finishes     -> finishes
  OverlappedBy -> overlappedBy
  MetBy        -> metBy
  After        -> after

-- | Given a set of 'IntervalRelation's return a list of 'predicate' functions
--   corresponding to each relation.
predicates ::
  (SizedIv (Interval a), Ord a, Intervallic i0, Intervallic i1) =>
  Data.Set.Set IntervalRelation ->
  [ComparativePredicateOf2 (i0 a) (i1 a)]
predicates x = Prelude.map toPredicate (Data.Set.toList x)

-- | Forms a predicate function from the union of a set of 'IntervalRelation's.
predicate ::
  (SizedIv (Interval a), Ord a, Intervallic i0, Intervallic i1) =>
  Data.Set.Set IntervalRelation ->
  ComparativePredicateOf2 (i0 a) (i1 a)
predicate = unionPredicates . predicates

-- | The lookup table for the compositions of interval relations.
composeRelationLookup :: [[[IntervalRelation]]]
composeRelationLookup =
  [ [p, p, p, p, p, p, p, p, pmosd, pmosd, pmosd, pmosd, full],
    [p, p, p, p, p, m, m, m, osd, osd, osd, fef, dsomp],
    [p, p, pmo, pmo, pmofd, o, o, ofd, osd, osd, cncr, dso, dsomp],
    [p, m, o, f', d', o, f', d', osd, fef, dso, dso, dsomp],
    [pmofd, ofd, ofd, d', d', ofd, d', d', cncr, dso, dso, dso, dsomp],
    [p, p, pmo, pmo, pmofd, s, s, ses, d, d, dfo, m', p'],
    [p, m, o, f', d', s, e, s', d, f, o', m', p'],
    [pmofd, ofd, ofd, d', d', ses, s', s', dfo, o', o', m', p'],
    [p, p, pmosd, pmosd, full, d, d, dfomp, d, d, dfomp, p', p'],
    [p, m, osd, fef, dsomp, d, f, omp, d, f, omp, p', p'],
    [pmofd, ofd, cncr, dso, dsomp, dfo, o', omp, dfo, o', omp, p', p'],
    [pmofd, ses, dfo, m', p', dfo, m', p', dfo, m', p', p', p'],
    [full, dfomp, dfomp, p', p', dfomp, p', p', dfomp, p', p', p', p']
  ]
  where
    p = [Before]
    m = [Meets]
    o = [Overlaps]
    f' = [FinishedBy]
    d' = [Contains]
    s = [Starts]
    e = [Equals]
    s' = [StartedBy]
    d = [During]
    f = [Finishes]
    o' = [OverlappedBy]
    m' = [MetBy]
    p' = [After]
    ses = s ++ e ++ s'
    fef = f' ++ e ++ f
    pmo = p ++ m ++ o
    pmofd = pmo ++ f' ++ d'
    osd = o ++ s ++ d
    ofd = o ++ f' ++ d'
    omp = o' ++ m' ++ p'
    dfo = d ++ f ++ o'
    dfomp = dfo ++ m' ++ p'
    dso = d' ++ s' ++ o'
    dsomp = dso ++ m' ++ p'
    pmosd = p ++ m ++ osd
    cncr = o ++ f' ++ d' ++ s ++ e ++ s' ++ d ++ f ++ o'
    full = p ++ m ++ cncr ++ m' ++ p'

-- | Compare two @i a@ to determine their 'IntervalRelation'.
--
-- >>> relate (Interval (0::Int, 1)) (Interval (1, 2))
-- Meets
--
-- >>> relate (Interval (1::Int, 2)) (Interval (0, 1))
-- MetBy
relate ::
  (Iv (Interval a), Intervallic i0, Intervallic i1) => i0 a -> i1 a -> IntervalRelation
relate x y = ivRelate (getInterval x) (getInterval y)

-- | Compose two interval relations according to the rules of the algebra.
-- The rules are enumerated according to
-- <https://thomasalspaugh.org/pub/fnd/allen.html#BasicCompositionsTable this table>.
compose ::
  IntervalRelation -> IntervalRelation -> Data.Set.Set IntervalRelation
compose x y = toSet (composeRelationLookup !! fromEnum x !! fromEnum y)

-- | Finds the complement of a @'Data.Set.Set' 'IntervalRelation'@.
complement :: Data.Set.Set IntervalRelation -> Data.Set.Set IntervalRelation
complement = Data.Set.difference intervalRelations

-- | Find the intersection of two 'Data.Set.Set's of 'IntervalRelation's.
intersection ::
  Data.Set.Set IntervalRelation ->
  Data.Set.Set IntervalRelation ->
  Data.Set.Set IntervalRelation
intersection = Data.Set.intersection

-- | Find the union of two 'Data.Set.Set's of 'IntervalRelation's.
union ::
  Data.Set.Set IntervalRelation ->
  Data.Set.Set IntervalRelation ->
  Data.Set.Set IntervalRelation
union = Data.Set.union

-- | Find the converse of a @'Data.Set.Set' 'IntervalRelation'@.
converse :: Data.Set.Set IntervalRelation -> Data.Set.Set IntervalRelation
converse = Data.Set.map converseRelation

{- Generic interval interfaces -}

-- | Generic interface for defining relations between abstract representations
-- of intervals, for the purpose of [Allen's interval algebra](https://en.wikipedia.org/wiki/Allen%27s_interval_algebra).
--
-- In general, these "intervals" need not be representable as temporal intervals with a fixed
-- beginning and ending. Specifically, the relations can be defined to provide temporal reasoning
-- in a qualitative setting, examples of which are in Allen 1983.
--
-- For intervals that can be cast in canonical form as 'Interval' s with begin and end points,
-- see 'PointedIv' and 'SizedIv'.
--
-- Instances of 'Iv' must ensure any pair of intervals satisfies exactly one
-- of the thirteen possible 'IntervalRelation' s.
--
-- When 'iv' is also an instance of 'PointedIv', with @Ord (Point iv)@,
-- the requirement implies
--
-- @
-- ivBegin i < ivEnd i
-- @
--
-- [Allen 1983](https://dl.acm.org/doi/10.1145/182.358434)
-- defines the 'IntervalRelation' s for such cases, which is provided in this module
-- for the canonical representation @'Interval' a@.
--
-- ==== __Examples__
--
-- The following example is modified from Allen 1983 to demonstrate the algebra used for temporal
-- reasoning in a qualitative setting, for a case where 'iv' does not have points.
--
-- It represents the temporal logic of the statement
--
-- > We found the letter during dinner, after we made the decision.
--
-- >>> :{
--data GoingsOn = Dinner | FoundLetter | MadeDecision
--  deriving (Show, Eq)
--instance Iv GoingsOn where
--  ivRelate MadeDecision Dinner = Before
--  ivRelate MadeDecision FoundLetter = Before
--  ivRelate FoundLetter Dinner = During
--  ivRelate x y
--    | x == y = Equals
--    | otherwise = converseRelation (ivRelate y x)
-- :}
class Iv iv where
  {-# MINIMAL ivRelate | ivBefore, ivMeets, ivOverlaps, ivStarts, ivFinishes, ivDuring, ivEquals #-}

  -- | The 'IntervalRelation' between two intervals.
  ivRelate :: iv -> iv -> IntervalRelation
  ivRelate x y
    | x `ivBefore` y = Before
    | x `ivAfter` y = After
    | x `ivMeets` y = Meets
    | x `ivMetBy` y = MetBy
    | x `ivOverlaps` y = Overlaps
    | x `ivOverlappedBy` y = OverlappedBy
    | x `ivStarts` y = Starts
    | x `ivStartedBy` y = StartedBy
    | x `ivFinishes` y = Finishes
    | x `ivFinishedBy` y = FinishedBy
    | x `ivDuring` y = During
    | x `ivContains` y = Contains
    | otherwise = Equals

  -- \| Is @'ivRelate' x y == Before@? @'ivAfter' = flip 'ivBefore'@.
  ivBefore,
    ivAfter ::
      -- | 'x'
      iv ->
      -- | 'y'
      iv ->
      Bool
  ivBefore x = (== Before) . ivRelate x
  ivAfter = flip ivBefore

  -- | Is @'ivRelate' x y == Meets@? @'ivMetBy' = flip 'ivMeets'@.
  ivMeets,
    ivMetBy ::
      -- | 'x'
      iv ->
      -- | 'y'
      iv ->
      Bool
  ivMeets x = (== Meets) . ivRelate x
  ivMetBy = flip ivMeets

  -- | Is @'ivRelate' x y == Overlaps@? @'ivOverlappedBy' = flip 'ivOverlaps'@.
  ivOverlaps,
    ivOverlappedBy ::
      -- | 'x'
      iv ->
      -- | 'y'
      iv ->
      Bool
  ivOverlaps x = (== Overlaps) . ivRelate x
  ivOverlappedBy = flip ivOverlaps

  -- | Is @'ivRelate' x y == Starts@? @'ivStartedBy' = flip 'ivStarts'@.
  ivStarts,
    ivStartedBy ::
      -- | 'x'
      iv ->
      -- | 'y'
      iv ->
      Bool
  ivStarts x = (== Starts) . ivRelate x
  ivStartedBy = flip ivStarts

  -- | Is @'ivRelate' x y == Finishes@? @'ivFinishedBy' = flip 'ivFinishes'@.
  ivFinishes,
    ivFinishedBy ::
      -- | 'x'
      iv ->
      -- | 'y'
      iv ->
      Bool
  ivFinishes x = (== Finishes) . ivRelate x
  ivFinishedBy = flip ivFinishes

  -- | Is @'ivRelate' x y == During@? @'ivContains' = flip 'ivDuring'@.
  ivDuring,
    ivContains ::
      -- | 'x'
      iv ->
      -- | 'y'
      iv ->
      Bool
  ivDuring x = (== During) . ivRelate x
  ivContains = flip ivDuring

  -- | Is @'ivRelate' x y == Equals@?
  ivEquals ::
    -- | 'x'
    iv ->
    -- | 'y'
    iv ->
    Bool
  ivEquals x = (== Equals) . ivRelate x

-- | Class representing intervals that can be cast to and from the canonical
-- representation @'Interval' a@.
--
-- When 'iv' is also an instance of 'PointedIv', with @Ord (Point iv)@, it should
-- adhere to Allen's construction of the interval algebra for intervals represented
-- by left and right endpoints. See [sections 3 and 4](https://cse.unl.edu/~choueiry/Documents/Allen-CACM1983.pdf)
-- of Allen 1983.
--
-- Specifically, the requirements for interval relations imply
--
-- @
-- ivBegin i < ivEnd i
-- @
--
-- This module provides default implementations for methods of 'Iv' in that case.
--
-- Note @iv@ should not be an instance of @Intervallic@ unless @iv ~ Interval
-- a@, since @Intervallic@ is a class for getting and setting intervals as
-- @Interval a@ in particular.
--
-- A @Vector@ whose elements are provided in strict ascending order is an example of
-- a type that could implement 'PointedIv' without being equivalent to 'Interval',
-- with @ivBegin = head@ and @ivEnd = last@.
class PointedIv iv where
  type Point iv

  -- | Access the left ("begin") and right ("end") endpoints of an interval.
  ivBegin, ivEnd :: iv -> Point iv

-- | The 'SizedIv' typeclass is a generic interface for constructing and
-- manipulating intervals. The class imposes strong requirements on its
-- methods, in large part to ensure the constructors 'ivExpandr' and 'ivExpandl'
-- return "valid" intervals, particularly in the typical case where 'iv' also
-- implements the interval algebra.
--
-- In all cases, 'ivExpandr' and 'ivExpandl' should preserve the value of the
-- point *not* shifted. That is,
--
-- @
-- ivBegin (ivExpandr d i) == ivBegin i
-- ivEnd (ivExpandl d i) == ivEnd i
-- @
--
-- In addition, using 'Interval' as example, the following must hold:
--
-- When @iv@ is @Ord@, for all @i == Interval (b, e)@,
-- @
-- ivExpandr d i >= i
-- ivExpandl d i <= i
-- @
--
-- When @Moment iv@ is @Ord@,
--
-- @
-- duration (ivExpandr d i) >= max moment (duration i)
-- duration (ivExpandl d i) >= max moment (duration i)
-- @
--
-- In particular, if the duration 'd' by which to expand is less than 'moment',
-- and @'duration' i >= moment@ then these constructors should return the input.
--
-- @
-- ivExpandr d i == i
-- ivExpandl d i == i
-- @
--
-- When @Moment iv@ also is @Num@, the default 'moment' value is @1@ and in all
-- cases should be positive.
--
-- @
-- moment @iv > 0
-- @
--
-- When in addition @Point iv ~ Moment iv@, the class provides a default 'duration' as
-- @duration i = ivEnd i - ivBegin i@.
--
-- This module enforces @'Point' (Interval a) = a@. However, it need not be
-- that @a ~ Moment iv@. For example @Moment (Interval UTCTime) ~
-- NominalDiffTime@.
--
-- ==== SizedIv and the interval algebra
--
-- When 'iv' is an instance of 'Iv', the methods of this class should ensure
-- the validity of the resulting interval with respect to the interval algebra.
-- For example, when @'Point' iv@ is 'Ord', they must always produce a valid
-- interval 'i' such that @'ivBegin' i < 'ivEnd' i@.
--
-- In addition, the requirements of 'SizedIv' implementations in the common case
-- where @'Moment' iv@ is 'Num' and 'Ord' require the constructors to produce intervals
-- with 'duration' of at least 'moment'.
--
-- In order to preserve the properties above, @ivExpandr, ivExpandl@ will not want to assume
-- validity of the input interval. In other words, @'ivExpandr' d i@ need not be the
-- identity when @d < 'moment'@ since it will need to ensure the result is a valid interval
-- even if 'i' is not.
--
-- These two methods can therefore be used as constructors for valid intervals.
class (PointedIv iv) => SizedIv iv where
  -- | Type of 'moment'.
  type Moment iv

  -- | The smallest duration for an 'iv'. When 'Moment iv' is an instance of
  -- 'Num', the default is 1. If @'Moment' iv@ is @Ord@ and @Num@, @'moment' > 0@
  -- is required.
  moment :: Moment iv

  -- | The duration of an 'iv'. When @Moment iv ~ Point iv@ and @Point iv@ is
  -- @Num@ this defaults to @ivEnd i - ivBegin i@.
  duration :: iv -> Moment iv

  -- | Resize @iv@ by expanding to the "left" or to the "right" by some
  -- duration. If @iv@ implements the interval algebra via @Iv@, these
  -- methods must produce valid intervals regardless of the validity of the input
  -- and thus serve as constructors for intervals. See also 'beginerval',
  -- 'endverval', 'safeInterval' and related.
  --
  -- See the class documentation for details requirements.
  --
  -- >>> ivExpandr 1 (safeInterval (0, 1) :: Interval Int) == safeInterval (0, 2)
  -- True
  -- >>> ivExpandr 0 (safeInterval (0, 1) :: Interval Int) == safeInterval (0, 1)
  -- True
  -- >>> ivExpandl 1 (safeInterval (0, 1) :: Interval Int) == safeInterval (-1, 1)
  -- True
  -- >>> ivExpandl 0 (safeInterval (0, 1) :: Interval Int) == safeInterval (0, 1)
  -- True
  ivExpandr, ivExpandl :: Moment iv -> iv -> iv

  default moment :: (Num (Moment iv)) => Moment iv
  moment = 1

  default duration :: (Point iv ~ Moment iv, Num (Point iv)) => iv -> Moment iv
  duration i = ivEnd i - ivBegin i

-- | Resize an @i a@ to by expanding to "left" by @l@ and to the "right" by @r@.
-- In the case that @l@ or @r@ are less than a 'moment' the respective endpoints
-- are unchanged.
--
-- >>> iv2to4 = safeInterval (2::Int, 4)
-- >>> iv2to4' = expand 0 0 iv2to4
-- >>> iv1to5 = expand 1 1 iv2to4
--
-- >>> iv2to4
-- (2, 4)
--
-- >>> iv2to4'
-- (2, 4)
--
-- >>> iv1to5
-- (1, 5)
--
-- >>> pretty $ standardExampleDiagram [(iv2to4, "iv2to4"), (iv1to5, "iv1to5")] []
--   --  <- [iv2to4]
--  ---- <- [iv1to5]
-- =====
expand ::
  (SizedIv (Interval a), Intervallic i) =>
  -- | duration to subtract from the 'begin'
  Moment (Interval a) ->
  -- | duration to add to the 'end'
  Moment (Interval a) ->
  i a ->
  i a
expand l r = expandl l . expandr r

-- | Expands an @i a@ to the "left".
--
-- >>> iv2to4 = (safeInterval (2::Int, 4::Int))
-- >>> iv0to4 = expandl 2 iv2to4
--
-- >>> iv2to4
-- (2, 4)
--
-- >>> iv0to4
-- (0, 4)
--
-- >>> pretty $ standardExampleDiagram [(iv2to4, "iv2to4"), (iv0to4, "iv0to4")] []
--   -- <- [iv2to4]
-- ---- <- [iv0to4]
-- ====
expandl :: (SizedIv (Interval a), Intervallic i) => Moment (Interval a) -> i a -> i a
expandl l i = setInterval i $ ivExpandl l $ getInterval i

-- | Expands an @i a@ to the "right".
--
-- >>> iv2to4 = (safeInterval (2::Int, 4::Int))
-- >>> iv2to6 = expandr 2 iv2to4
--
-- >>> iv2to4
-- (2, 4)
--
-- >>> iv2to6
-- (2, 6)
--
-- >>> pretty $ standardExampleDiagram [(iv2to4, "iv2to4"), (iv2to6, "iv2to6")] []
--   --   <- [iv2to4]
--   ---- <- [iv2to6]
-- ======
expandr :: (SizedIv (Interval a), Intervallic i) => Moment (Interval a) -> i a -> i a
expandr r i = setInterval i $ ivExpandr r $ getInterval i

-- | Safely creates an 'Interval a' using @x@ as the 'begin' and adding @max
-- 'moment' dur@ to @x@ as the 'end'. For the 'SizedIv' instances this
-- module exports, 'beginerval' is the same as 'interval'. However, it is defined
-- separately since 'beginerval' will /always/ have this behavior whereas
-- 'interval' behavior might differ by implementation.
--
-- >>> beginerval (0::Int) (0::Int)
-- (0, 1)
--
-- >>> beginerval (1::Int) (0::Int)
-- (0, 1)
--
-- >>> beginerval (2::Int) (0::Int)
-- (0, 2)
beginerval ::
  forall a.
  (SizedIv (Interval a)) =>
  -- | @dur@ation to add to the 'begin'
  Moment (Interval a) ->
  -- | the 'begin' point of the 'Interval'
  a ->
  Interval a
beginerval dur x = ivExpandr dur $ Interval (x, x)

-- | A synonym for `beginerval`
bi ::
  forall a.
  (SizedIv (Interval a)) =>
  -- | @dur@ation to add to the 'begin'
  Moment (Interval a) ->
  -- | the 'begin' point of the 'Interval'
  a ->
  Interval a
bi = beginerval

-- | Safely creates an 'Interval a' using @x@ as the 'end' and adding @negate max
-- 'moment' dur@ to @x@ as the 'begin'.
--
-- >>> enderval (0::Int) (0::Int)
-- (-1, 0)
--
-- >>> enderval (1::Int) (0::Int)
-- (-1, 0)
--
-- >>> enderval (2::Int) (0::Int)
-- (-2, 0)
enderval ::
  forall a.
  (SizedIv (Interval a)) =>
  -- | @dur@ation to subtract from the 'end'
  Moment (Interval a) ->
  -- | the 'end' point of the 'Interval'
  a ->
  Interval a
enderval dur x = ivExpandl dur $ Interval (x, x)

-- | A synonym for `enderval`
ei ::
  forall a.
  (SizedIv (Interval a)) =>
  -- | @dur@ation to subtract from the 'end'
  Moment (Interval a) ->
  -- | the 'end' point of the 'Interval'
  a ->
  Interval a
ei = enderval

-- | Safely creates an @'Interval'@ from a pair of endpoints,
-- expanding from the left endpoint if necessary to create a valid interval
-- according to the rules of 'SizedIv'. This function simply wraps
-- 'ivExpandr'.
--
-- >>> safeInterval (4, 5 ::Int)
-- (4, 5)
-- >>> safeInterval (4, 3 :: Int)
-- (4, 5)
safeInterval ::
  forall a.
  (SizedIv (Interval a), Ord (Moment (Interval a))) =>
  (a, a) ->
  Interval a
safeInterval (b, e)
  | duration i < m = ivExpandr m $ Interval (b, b)
  | otherwise = i
  where
    i = Interval (b, e)
    m = moment @(Interval a)

-- | A synonym for `safeInterval`
si ::
  (SizedIv (Interval a), Ord (Moment (Interval a))) =>
  (a, a) ->
  Interval a
si = safeInterval

-- | Creates a new 'Interval' from the 'end' of another.
beginervalFromEnd ::
  (SizedIv (Interval a), Intervallic i) =>
  -- | @dur@ation to add to the 'end'
  Moment (Interval a) ->
  -- | the @i a@ from which to get the 'end'
  i a ->
  Interval a
beginervalFromEnd d i = beginerval d (end i)

-- | Creates a new 'Interval' from the 'begin' of another.
endervalFromBegin ::
  (SizedIv (Interval a), Intervallic i) =>
  -- | @dur@ation to subtract from the 'begin'
  Moment (Interval a) ->
  -- | the @i a@ from which to get the 'begin'
  i a ->
  Interval a
endervalFromBegin d i = enderval d (begin i)

-- | Safely creates a new @Interval@ with 'moment' length with 'begin' at @x@
--
-- >>> beginervalMoment (10 :: Int)
-- (10, 11)
beginervalMoment :: forall a. (SizedIv (Interval a)) => a -> Interval a
beginervalMoment = beginerval (moment @(Interval a))

-- | Safely creates a new @Interval@ with 'moment' length with 'end' at @x@
--
-- >>> endervalMoment (10 :: Int)
-- (9, 10)
endervalMoment :: forall a. (SizedIv (Interval a)) => a -> Interval a
endervalMoment = enderval (moment @(Interval a))

-- | Creates a new @Interval@ spanning the extent x and y.
--
-- >>> extenterval (Interval (0, 1)) (Interval (9, 10))
-- (0, 10)
extenterval :: (SizedIv (Interval a), Ord a, Intervallic i) => i a -> i a -> Interval a
extenterval x y = Interval (s, e)
  where
    s = min (begin x) (begin y)
    e = max (end x) (end y)

-- | Modifies the endpoints of second argument's interval by taking the difference
-- from the first's input's 'begin'.
--
-- Example data with corresponding diagram:
--
-- >>> a = bi 3 2 :: Interval Int
-- >>> a
-- (2, 5)
-- >>> x = bi 3 7 :: Interval Int
-- >>> x
-- (7, 10)
-- >>> y = bi 4 9 :: Interval Int
-- >>> y
-- (9, 13)
-- >>> pretty $ standardExampleDiagram [(a, "a"), (x, "x"), (y, "y")] []
--   ---         <- [a]
--        ---    <- [x]
--          ---- <- [y]
-- =============
--
-- Examples:
--
-- >>> x' = shiftFromBegin a x
-- >>> x'
-- (5, 8)
-- >>> y' = shiftFromBegin a y
-- >>> y'
-- (7, 11)
-- >>> pretty $ standardExampleDiagram [(x', "x'"), (y', "y'")] []
--      ---    <- [x']
--        ---- <- [y']
-- ===========
shiftFromBegin ::
  (Num a, SizedIv (Interval a), Intervallic i1, Intervallic i0) =>
  i0 a ->
  i1 a ->
  i1 a
shiftFromBegin i = imapStrictMonotone (\x -> x - begin i)

-- | Modifies the endpoints of second argument's interval by taking the difference
-- from the first's input's 'end'.
--
-- Example data with corresponding diagram:
--
-- >>> a = bi 3 2 :: Interval Int
-- >>> a
-- (2, 5)
-- >>> x = bi 3 7 :: Interval Int
-- >>> x
-- (7, 10)
-- >>> y = bi 4 9 :: Interval Int
-- >>> y
-- (9, 13)
-- >>> pretty $ standardExampleDiagram [(a, "a"), (x, "x"), (y, "y")] []
--   ---         <- [a]
--        ---    <- [x]
--          ---- <- [y]
-- =============
--
-- Examples:
--
-- >>> x' = shiftFromEnd a x
-- >>> x'
-- (2, 5)
-- >>> y' = shiftFromEnd a y
-- >>> y'
-- (4, 8)
-- >>> pretty $ standardExampleDiagram [(x', "x'"), (y', "y'")] []
--   ---    <- [x']
--     ---- <- [y']
-- ========

shiftFromEnd ::
  (Num a, SizedIv (Interval a), Intervallic i1, Intervallic i0) =>
  i0 a ->
  i1 a ->
  i1 a
shiftFromEnd i = imapStrictMonotone (\x -> x - end i)

-- | Converts an @i a@ to an @i Int@ via @fromEnum@.  This assumes the provided
-- @fromEnum@ method is strictly monotone increasing: For @a@ types that are
-- @Ord@ with values @x, y@, then @x < y@ implies @fromEnum x < fromEnum y@, so
-- long as the latter is well-defined.
fromEnumInterval :: (Enum a, Intervallic i) => i a -> i Int
fromEnumInterval = imapStrictMonotone fromEnum

-- | Converts an @i Int@ to an @i a@ via @toEnum@.  This assumes the provided
-- @toEnum@ method is strictly monotone increasing: For @a@ types that are
-- @Ord@, then for @Int@ values @x, y@ it holds that @x < y@ implies @toEnum x
-- < toEnum y@.
toEnumInterval :: (Enum a, Intervallic i) => i Int -> i a
toEnumInterval = imapStrictMonotone toEnum

-- | Changes the duration of an 'Intervallic' value to a moment starting at the
-- 'begin' of the interval. Uses 'beginervalMoment'.
--
-- >>> momentize (Interval (6, 10))
-- (6, 7)
momentize ::
  forall i a. (SizedIv (Interval a), Intervallic i) => i a -> i a
momentize i = setInterval i $ beginervalMoment $ begin i

{-
Misc
-}

-- | Defines a predicate of two objects of type @a@.
type ComparativePredicateOf1 a = (a -> a -> Bool)

-- | Defines a predicate of two object of different types.
type ComparativePredicateOf2 a b = (a -> b -> Bool)

{- Common instance helpers -}

-- | Internal. Helper for SizedIv constructor implementations
-- defined in this module, so as to ensure the class properties.
ivExpandrI ::
  (Ord b) =>
  -- | 'moment' value to be passed here.
  b ->
  -- | 'duration'
  (a -> a -> b) ->
  -- | function for adding an amount of moments to a point.
  -- It must always satisfy addFun (dFun x y) y == x
  (b -> a -> a) ->
  -- | duration by which to expand.
  b ->
  Interval a ->
  Interval a
ivExpandrI mom dFun addFun d (Interval (b, e))
  | d < mom = Interval (b, addFun (max (dFun e b) mom) b)
  | otherwise = Interval (b, addFun d e)

ivExpandlI ::
  (Ord b) =>
  -- | 'moment' value to be passed here.
  b ->
  -- | 'duration'
  (a -> a -> b) ->
  -- | function for subtracting a amount of moments to a point.
  -- It must always satisfy subFun (dFun x y) x == y
  (b -> a -> a) ->
  -- | duration by which to expand.
  b ->
  Interval a ->
  Interval a
ivExpandlI mom dFun subFun d (Interval (b, e))
  | d < mom = Interval (subFun (max (dFun e b) mom) e, e)
  | otherwise = Interval (subFun d b, e)

{- Instances -}

-- | Imposes a total ordering on @'Interval' a@ based on first ordering the
--   'begin's then the 'end's.
instance (Ord a) => Ord (Interval a) where
  (Interval pts1) <= (Interval pts2) = pts1 <= pts2
  (Interval pts1) < (Interval pts2) = pts1 < pts2

instance Intervallic Interval where
  getInterval = id
  setInterval _ x = x

instance PointedIv (Interval a) where
  type Point (Interval a) = a

  ivBegin (Interval (b, _)) = b
  ivEnd (Interval (_, e)) = e

-- | Implements the interval algebra for intervals represented as left and right endpoints,
-- with points in a totally ordered set, as prescribed in
-- [Allen 1983](https://dl.acm.org/doi/10.1145/182.358434).
instance (Ord a) => Iv (Interval a) where
  ivBefore x y = ivEnd x < ivBegin y
  ivMeets x y = ivEnd x == ivBegin y
  ivOverlaps x y = ivBegin x < ivBegin y && ivEnd x < ivEnd y && ivEnd x > ivBegin y
  ivStarts x y = ivBegin x == ivBegin y && ivEnd x < ivEnd y
  ivFinishes x y = ivBegin x > ivBegin y && ivEnd x == ivEnd y
  ivDuring x y = ivBegin x > ivBegin y && ivEnd x < ivEnd y
  ivEquals x y = ivBegin x == ivBegin y && ivEnd x == ivEnd y

-- TODO: Consider whether blanket instance for
-- Num a => SizedIv (Interval a) is good.

instance SizedIv (Interval Int) where
  type Moment (Interval Int) = Int
  ivExpandr = ivExpandrI (moment @(Interval Int)) (-) (+)
  ivExpandl = ivExpandlI (moment @(Interval Int)) (-) (flip (-))

instance SizedIv (Interval Integer) where
  type Moment (Interval Integer) = Integer
  ivExpandr = ivExpandrI (moment @(Interval Integer)) (-) (+)
  ivExpandl = ivExpandlI (moment @(Interval Integer)) (-) (flip (-))

instance SizedIv (Interval Double) where
  type Moment (Interval Double) = Double
  ivExpandr = ivExpandrI (moment @(Interval Double)) (-) (+)
  ivExpandl = ivExpandlI (moment @(Interval Double)) (-) (flip (-))

instance SizedIv (Interval DT.Day) where
  type Moment (Interval DT.Day) = Integer
  duration (Interval (b, e)) = diffDays e b
  moment = 1
  ivExpandr = ivExpandrI (moment @(Interval DT.Day)) diffDays addDays
  ivExpandl = ivExpandlI (moment @(Interval DT.Day)) diffDays (\d -> addDays (-d))

-- | Note this instance changes the @moment@ to 1 'Pico' second, not 1 second
-- as would be the case if the default were used.
instance SizedIv (Interval DT.UTCTime) where
  type Moment (Interval DT.UTCTime) = NominalDiffTime
  moment = toEnum 1
  duration (Interval (b, e)) = diffUTCTime e b
  ivExpandr = ivExpandrI (moment @(Interval DT.UTCTime)) diffUTCTime addUTCTime
  ivExpandl = ivExpandlI (moment @(Interval DT.UTCTime)) diffUTCTime (\d -> addUTCTime (-d))
