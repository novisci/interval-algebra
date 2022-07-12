{-|
Module      : Interval Algebra
Description : Implementation of Allen's interval algebra
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

The @IntervalAlgebra@ module provides data types and related classes for the 
interval-based temporal logic described in [Allen (1983)](https://doi.org/10.1145/182.358434)
and axiomatized in [Allen and Hayes (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x). 
A good primer on Allen's algebra can be [found here](https://thomasalspaugh.org/pub/fnd/allen.html).

= Design

The module is built around three typeclasses designed to separate concerns of 
constructing, relating, and combining types that contain @'Interval'@s: 

1. @'Intervallic'@ provides an interface to the data structures which contain an
   @'Interval'@.
2. @'IntervalCombinable'@ provides an interface to methods of combining two
   @'Interval's@.
3. @'IntervalSizeable'@ provides methods for measuring and modifying the size of
    an interval.

-}

{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveGeneric #-}

module IntervalAlgebra.Core
  (

    -- * Intervals
    Interval
  , Intervallic(..)
  , ParseErrorInterval(..)
  , begin
  , end

    -- ** Create new intervals
  , parseInterval
  , prsi
  , beginerval
  , bi
  , enderval
  , ei
  , safeInterval
  , si

    -- ** Modify intervals
  , expand
  , expandl
  , expandr

    -- * Interval Algebra 

    -- ** Interval Relations and Predicates
  , IntervalRelation(..)
  , meets
  , metBy
  , before
  , after
  , overlaps
  , overlappedBy
  , finishedBy
  , finishes
  , contains
  , during
  , starts
  , startedBy
  , equals

    -- ** Additional predicates and utilities
  , precedes
  , precededBy
  , disjoint
  , notDisjoint
  , concur
  , within
  , enclose
  , enclosedBy
  , (<|>)
  , predicate
  , unionPredicates
  , disjointRelations
  , withinRelations
  , strictWithinRelations
  , ComparativePredicateOf1
  , ComparativePredicateOf2
  , beginervalFromEnd
  , endervalFromBegin
  , beginervalMoment
  , endervalMoment
  , shiftFromBegin
  , shiftFromEnd
  , momentize
  , toEnumInterval
  , fromEnumInterval

    -- ** Algebraic operations
  , intervalRelations
  , relate
  , compose
  , complement
  , union
  , intersection
  , converse

    -- * Combine two intervals
  , IntervalCombinable(..)
  , extenterval

    -- * Measure an interval
  , IntervalSizeable(..)
  ) where

import           Control.Applicative            ( Applicative(pure)
                                                , liftA2
                                                )
import           Control.DeepSeq                ( NFData )
import           Data.Binary                    ( Binary )
import           Data.Fixed                     ( Pico )
import           Data.Function                  ( ($)
                                                , (.)
                                                , flip
                                                , id
                                                )
import           Data.Ord                       ( Ord(..)
                                                , Ordering(..)
                                                , max
                                                , min
                                                )
import           Data.Semigroup                 ( Semigroup((<>)) )
import qualified Data.Set                       ( Set
                                                , difference
                                                , fromList
                                                , intersection
                                                , map
                                                , toList
                                                , union
                                                )
import           Data.Time                     as DT
                                                ( Day
                                                , DiffTime
                                                , NominalDiffTime
                                                , UTCTime
                                                , addDays
                                                , addUTCTime
                                                , diffDays
                                                , diffUTCTime
                                                , nominalDiffTimeToSeconds
                                                , secondsToNominalDiffTime
                                                )
import           Data.Tuple                     ( fst
                                                , snd
                                                )
import           GHC.Generics                   ( Generic )
import           Prelude                        ( (!!)
                                                , (&&)
                                                , (+)
                                                , (++)
                                                , (-)
                                                , (==)
                                                , Bool(..)
                                                , Bounded(..)
                                                , Either(..)
                                                , Enum(..)
                                                , Eq
                                                , Int
                                                , Integer
                                                , Maybe(..)
                                                , Num
                                                , Rational
                                                , Show
                                                , String
                                                , any
                                                , curry
                                                , fromInteger
                                                , fromRational
                                                , map
                                                , negate
                                                , not
                                                , otherwise
                                                , realToFrac
                                                , replicate
                                                , show
                                                , toInteger
                                                , toRational
                                                )
import           Test.QuickCheck                ( Arbitrary(..)
                                                , resize
                                                , sized
                                                , suchThat
                                                )

{- | An @'Interval' a@ is a pair \( (x, y) \text{ such that } x < y\). To create
intervals use the @'parseInterval'@, @'beginerval'@, or @'enderval'@ functions.
-}
newtype Interval a = Interval (a, a) deriving (Eq, Generic)

-- | A type identifying interval parsing errors.
newtype ParseErrorInterval = ParseErrorInterval String
    deriving (Eq, Show)

-- | Helper defining what a valid relation is between begin and end of an
-- Interval.
isValidBeginEnd :: (Ord a) => a -> a -> Bool
isValidBeginEnd b e = b < e

-- | Safely parse a pair of @a@s to create an @'Interval' a@.
--
-- >>> parseInterval 0 1
-- Right (0, 1)
-- 
-- >>> parseInterval 1 0
-- Left (ParseErrorInterval "0<=1")
-- 
parseInterval
  :: (Show a, Ord a) => a -> a -> Either ParseErrorInterval (Interval a)
parseInterval x y
  | isValidBeginEnd x y = Right $ Interval (x, y)
  | otherwise           = Left $ ParseErrorInterval $ show y ++ "<=" ++ show x
-- | A synonym for `parseInterval`
prsi :: (Show a, Ord a) => a -> a -> Either ParseErrorInterval (Interval a)
prsi = parseInterval

intervalBegin :: (Ord a) => Interval a -> a
intervalBegin (Interval x) = fst x

intervalEnd :: (Ord a) => Interval a -> a
intervalEnd (Interval x) = snd x

instance (Show a, Ord a) => Show (Interval a) where
  show x = "(" ++ show (begin x) ++ ", " ++ show (end x) ++ ")"

instance Binary a => Binary (Interval a)
instance NFData a => NFData (Interval a)

{- | 
The @'Intervallic'@ typeclass defines how to get and set the 'Interval' content
of a data structure. It also includes functions for getting the endpoints of the
'Interval' via @'begin'@ and @'end'@. 

>>> getInterval (Interval (0, 10))
(0, 10)

>>> begin (Interval (0, 10))
0

>>> end (Interval (0, 10))
10
-}
class Intervallic i where

    -- | Get the interval from an @i a@.
    getInterval :: i a -> Interval a

    -- | Set the interval in an @i a@.
    setInterval :: i a -> Interval b -> i b

-- | Access the endpoints of an @i a@ .
begin, end :: (Ord a, Intervallic i) => i a -> a
begin = intervalBegin . getInterval
end = intervalEnd . getInterval

-- | This *unexported* function is an internal convenience function for cases
-- in which @f@ is known to be strictly monotone.
imapStrictMonotone :: (Intervallic i) => (a -> b) -> i a -> i b
imapStrictMonotone f i = setInterval i (op f (getInterval i))
  where op f (Interval (b, e)) = Interval (f b, f e)

{- | 
The 'IntervalRelation' type and the associated predicate functions enumerate
the thirteen possible ways that two @'Interval'@ objects may 'relate' according
to Allen's interval algebra. Constructors are shown with their corresponding 
predicate function.
-}
data IntervalRelation =
      Before        -- ^ `before`
    | Meets         -- ^ `meets`
    | Overlaps      -- ^ `overlaps`
    | FinishedBy    -- ^ `finishedBy`
    | Contains      -- ^ `contains`
    | Starts        -- ^ `starts`
    | Equals        -- ^ `equals`
    | StartedBy     -- ^ `startedBy`
    | During        -- ^ `during`
    | Finishes      -- ^ `finishes`
    | OverlappedBy  -- ^ `overlappedBy`
    | MetBy         -- ^ `metBy`
    | After         -- ^ `after`
    deriving (Eq, Show, Enum)

instance Bounded IntervalRelation where
  minBound = Before
  maxBound = After

instance Ord IntervalRelation where
  compare x y = compare (fromEnum x) (fromEnum y)

{- not for Haddock
    code to generate interval for Haddock
    x = bi 5 0
    y = bi 5 5
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]   
-}

{- | Does x `meets` y? Is x `metBy` y?

Example data with corresponding diagram:

>>> x = bi 5 0

>>> y = bi 5 5

@
-----      <- [x]
     ----- <- [y]
==========
@

Examples: 

>>> x `meets` y
True

>>> x `metBy` y
False

>>> y `meets` x
False

>>> y `metBy` x
True

-}
meets, metBy
  :: (Ord a, Intervallic i0, Intervallic i1)
  => ComparativePredicateOf2 (i0 a) (i1 a)
meets x y = end x == begin y
metBy = flip meets

{- not for Haddock
    code to generate interval for Haddock
    x = bi 3 0
    y = bi 4 6
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]   
-}

{- | Is x `before` y? Does x `precedes` y? Is x `after` y? Is x `precededBy` y?

Example data with corresponding diagram:

>>> x = bi 3 0
>>> y = bi 4 6

@
---        <- [x]
      ---- <- [y]
========== 
@

Examples:

>>> x `before` y
True
>>> x `precedes` y
True

>>> x `after`y
False
>>> x `precededBy` y
False

>>> y `before` x
False
>>> y `precedes` x
False

>>> y `after` x
True
>>> y `precededBy` x
True


-}
before, after, precedes, precededBy
  :: (Ord a, Intervallic i0, Intervallic i1)
  => ComparativePredicateOf2 (i0 a) (i1 a)
before x y = end x < begin y
after = flip before
precedes = before
precededBy = after

{- not for Haddock
    code to generate interval for Haddock
    x = bi 6 0
    y = bi 6 4
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]   
-}


{- | Does x `overlaps` y? Is x `overlappedBy` y?

Example data with corresponding diagram:

>>> x = bi 6 0
>>> y = bi 6 4

@
------     <- [x]
    ------ <- [y]
==========
@

Examples:

>>> x `overlaps` y
True

>>> x `overlappedBy` y
False

>>> y `overlaps` x
False

>>> y `overlappedBy` x
True

-}
overlaps, overlappedBy
  :: (Ord a, Intervallic i0, Intervallic i1)
  => ComparativePredicateOf2 (i0 a) (i1 a)
overlaps x y = begin x < begin y && end x < end y && end x > begin y
overlappedBy = flip overlaps

{- not for Haddock
    code to generate interval for Haddock
    x = bi 3 4
    y = bi 6 4
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]   
-}

{-| Does x `starts` y? Is x `startedBy` y?

Example data with corresponding diagram:

>>> x = bi 3 4
>>> y = bi 6 4

@
    ---    <- [x]
    ------ <- [y]
==========
@

Examples:

>>> x `starts` y
True

>>> x `startedBy` y
False

>>> y `starts` x
False

>>> y `startedBy` x
True

-}
starts, startedBy
  :: (Ord a, Intervallic i0, Intervallic i1)
  => ComparativePredicateOf2 (i0 a) (i1 a)
starts x y = begin x == begin y && end x < end y
startedBy = flip starts

{- not for Haddock
    code to generate interval for Haddock
    x = bi 3 7
    y = bi 6 4
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]   
-}

{- | Does x `finishes` y? Is x `finishedBy` y?

Example data with corresponding diagram:

>>> x = bi 3 7
>>> y = bi 6 4

@
       --- <- [x]
    ------ <- [y]
==========
@

Examples:

>>> x `finishes` y
True

>>> x `finishedBy` y
False

>>> y `finishes` x
False

>>> y `finishedBy` x
True

-}
finishes, finishedBy
  :: (Ord a, Intervallic i0, Intervallic i1)
  => ComparativePredicateOf2 (i0 a) (i1 a)
finishes x y = begin x > begin y && end x == end y
finishedBy = flip finishes

{- not for Haddock
    code to generate interval for Haddock
    x = bi 3 5
    y = bi 6 4
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]   
-}

{-| Is x `during` y? Does x `contains` y?

Example data with corresponding diagram:

>>> x = bi 3 5
>>> y = bi 6 4

@
     ---   <- [x]
    ------ <- [y]
==========
@

Examples:

>>> x `during` y
True

>>> x `contains` y
False

>>> y `during` x
False

>>> y `contains` x
True

-}
during, contains
  :: (Ord a, Intervallic i0, Intervallic i1)
  => ComparativePredicateOf2 (i0 a) (i1 a)
during x y = begin x > begin y && end x < end y
contains = flip during


{- not for Haddock
    code to generate interval for Haddock
    x = bi 6 4
    y = bi 6 4
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]   
-}

{- | Does x `equals` y?

Example data with corresponding diagram:

>>> x = bi 6 4
>>> y = bi 6 4

@
    ------ <- [x]
    ------ <- [y]
==========
@

Examples:

>>> x `equals` y
True

>>> y `equals` x
True

-}
equals
  :: (Ord a, Intervallic i0, Intervallic i1)
  => ComparativePredicateOf2 (i0 a) (i1 a)
equals x y = begin x == begin y && end x == end y

-- | Operator for composing the union of two predicates
(<|>)
  :: (Intervallic i0, Intervallic i1)
  => ComparativePredicateOf2 (i0 a) (i1 a)
  -> ComparativePredicateOf2 (i0 a) (i1 a)
  -> ComparativePredicateOf2 (i0 a) (i1 a)
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


{- not for Haddock
    code to generate interval for Haddock
    x = bi 3 0
    y = bi 3 5
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]    

    x = bi 3 0
    y = bi 3 3
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]   

    x = bi 6 0
    y = bi 3 3 
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]   
-}

{- | Are x and y `disjoint` ('before', 'after', 'meets', or 'metBy')?

Example data with corresponding diagram:

>>> x = bi 3 0
>>> y = bi 3 5

@
---        <- [x]
     ---   <- [y]
==========
@

Examples:

>>> x `disjoint` y
True

>>> y `disjoint` x
True

Example data with corresponding diagram:

>>> x = bi 3 0
>>> y = bi 3 3

@
---        <- [x]
   ---     <- [y]
==========
@

Examples:

>>> x `disjoint` y
True

>>> y `disjoint` x
True

Example data with corresponding diagram:

>>> x = bi 6 0
>>> y = bi 3 3 

@
------     <- [x]
   ---     <- [y]
==========
@

Examples:

>>> x `disjoint` y
False

>>> y `disjoint` x
False

-}
disjoint
  :: (Ord a, Intervallic i0, Intervallic i1)
  => ComparativePredicateOf2 (i0 a) (i1 a)
disjoint = predicate disjointRelations

{- not for Haddock
    code to generate interval for Haddock
    x = bi 3 0
    y = bi 3 4
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]    

    x = bi 3 0
    y = bi 3 3
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]   

    x = bi 6 0
    y = bi 3 3 
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]    
-}

{-| Does x `concur` with y? Is x `notDisjoint` with y?); This is
the 'complement' of 'disjoint'.

Example data with corresponding diagram:

>>> x = bi 3 0
>>> y = bi 3 4

@
---        <- [x]
     ---   <- [y]
==========
@

Examples:

>>> x `notDisjoint` y
False
>>> y `concur` x
False

Example data with corresponding diagram:

>>> x = bi 3 0
>>> y = bi 3 3

@
---        <- [x]
   ---     <- [y]
==========
@

Examples:

>>> x `notDisjoint` y
False
>>> y `concur` x
False

Example data with corresponding diagram:

>>> x = bi 6 0
>>> y = bi 3 3 

@
------     <- [x]
   ---     <- [y]
==========
@

Examples:

>>> x `notDisjoint` y
True
>>> y `concur` x
True

-}
notDisjoint, concur
  :: (Ord a, Intervallic i0, Intervallic i1)
  => ComparativePredicateOf2 (i0 a) (i1 a)
notDisjoint = predicate (complement disjointRelations)
concur = notDisjoint

{- not for Haddock
    code to generate interval for Haddock
    x = bi 6 4
    y = bi 6 4
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]   

    x = bi 6 4
    y = bi 5 4
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]   

    x = bi 6 4
    y = bi 4 5
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]   

    x = bi 2 7
    y = bi 1 5
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]   
-}

{- | Is x `within` (`enclosedBy`) y? That is, 'during', 
     'starts', 'finishes', or 'equals'?

Example data with corresponding diagram:

>>> x = bi 6 4
>>> y = bi 6 4

@
    ------ <- [x]
    ------ <- [y]
==========
@

Examples:

>>> x `within` y
True

>>> y `enclosedBy` x
True

Example data with corresponding diagram:

>>> x = bi 6 4
>>> y = bi 5 4

@
    ------ <- [x]
    -----  <- [y]
==========
@

Examples:

>>> x `within` y
False

>>> y `enclosedBy` x
True

Example data with corresponding diagram:

>>> x = bi 6 4
>>> y = bi 4 5

@
    ------ <- [x]
     ----  <- [y]
==========
@

Examples:

>>> x `within` y
False
>>> y `enclosedBy` x
True

Example data with corresponding diagram:

>>> x = bi 2 7
>>> y = bi 1 5

@
       --  <- [x]
 -----     <- [y]
==========
@

Examples:

>>> x `within` y
False

>>> y `enclosedBy` x
False

-}
within, enclosedBy
  :: (Ord a, Intervallic i0, Intervallic i1)
  => ComparativePredicateOf2 (i0 a) (i1 a)
within = predicate withinRelations
enclosedBy = within

{- not for Haddock
    code to generate interval for Haddock
    x = bi 6 4
    y = bi 6 4
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]    

    x = bi 6 4
    y = bi 5 4
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]   

    x = bi 2 7
    y = bi 1 5
    pretty $ labeledIntervalDiagram [(x, "x"), (y, "y")]   
-}


{- | Does x `enclose` y? That is, is y 'within' x?

Example data with corresponding diagram:

>>> x = bi 6 4
>>> y = bi 6 4

@
    ------ <- [x]
    ------ <- [y]
==========
@

Examples:

>>> x `enclose` y
True

>>> y `enclose` x
True

Example data with corresponding diagram:

>>> x = bi 6 4
>>> y = bi 5 4

@
    ------ <- [x]
    -----  <- [y]
==========
@ 

Examples:

>>> x `enclose` y
True

>>> y `enclose` x
False

Example data with corresponding diagram:

>>> x = bi 6 4
>>> y = bi 4 5

@
    ------ <- [x]
     ----  <- [y]
==========
@

Examples:

>>> x `enclose` y
True

>>> y `enclose` x
False

Example data with corresponding diagram:

>>> x = bi 2 7
>>> y = bi 1 5

@
       --  <- [x]
 -----     <- [y]
==========
@

Examples:

>>> x `enclose` y
False

>>> y `enclose` x
False

-}
enclose
  :: (Ord a, Intervallic i0, Intervallic i1)
  => ComparativePredicateOf2 (i0 a) (i1 a)
enclose = flip enclosedBy

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
toPredicate
  :: (Ord a, Intervallic i0, Intervallic i1)
  => IntervalRelation
  -> ComparativePredicateOf2 (i0 a) (i1 a)
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
predicates
  :: (Ord a, Intervallic i0, Intervallic i1)
  => Data.Set.Set IntervalRelation
  -> [ComparativePredicateOf2 (i0 a) (i1 a)]
predicates x = Prelude.map toPredicate (Data.Set.toList x)

-- | Forms a predicate function from the union of a set of 'IntervalRelation's.
predicate
  :: (Ord a, Intervallic i0, Intervallic i1)
  => Data.Set.Set IntervalRelation
  -> ComparativePredicateOf2 (i0 a) (i1 a)
predicate = unionPredicates . predicates

-- | The lookup table for the compositions of interval relations.
composeRelationLookup :: [[[IntervalRelation]]]
composeRelationLookup =
  [ [p, p, p, p, p, p, p, p, pmosd, pmosd, pmosd, pmosd, full]
  , [p, p, p, p, p, m, m, m, osd, osd, osd, fef, dsomp]
  , [p, p, pmo, pmo, pmofd, o, o, ofd, osd, osd, cncr, dso, dsomp]
  , [p, m, o, f', d', o, f', d', osd, fef, dso, dso, dsomp]
  , [pmofd, ofd, ofd, d', d', ofd, d', d', cncr, dso, dso, dso, dsomp]
  , [p, p, pmo, pmo, pmofd, s, s, ses, d, d, dfo, m', p']
  , [p, m, o, f', d', s, e, s', d, f, o', m', p']
  , [pmofd, ofd, ofd, d', d', ses, s', s', dfo, o', o', m', p']
  , [p, p, pmosd, pmosd, full, d, d, dfomp, d, d, dfomp, p', p']
  , [p, m, osd, fef, dsomp, d, f, omp, d, f, omp, p', p']
  , [pmofd, ofd, cncr, dso, dsomp, dfo, o', omp, dfo, o', omp, p', p']
  , [pmofd, ses, dfo, m', p', dfo, m', p', dfo, m', p', p', p']
  , [full, dfomp, dfomp, p', p', dfomp, p', p', dfomp, p', p', p', p']
  ]
 where
  p     = [Before]
  m     = [Meets]
  o     = [Overlaps]
  f'    = [FinishedBy]
  d'    = [Contains]
  s     = [Starts]
  e     = [Equals]
  s'    = [StartedBy]
  d     = [During]
  f     = [Finishes]
  o'    = [OverlappedBy]
  m'    = [MetBy]
  p'    = [After]
  ses   = s ++ e ++ s'
  fef   = f' ++ e ++ f
  pmo   = p ++ m ++ o
  pmofd = pmo ++ f' ++ d'
  osd   = o ++ s ++ d
  ofd   = o ++ f' ++ d'
  omp   = o' ++ m' ++ p'
  dfo   = d ++ f ++ o'
  dfomp = dfo ++ m' ++ p'
  dso   = d' ++ s' ++ o'
  dsomp = dso ++ m' ++ p'
  pmosd = p ++ m ++ osd
  cncr  = o ++ f' ++ d' ++ s ++ e ++ s' ++ d ++ f ++ o'
  full  = p ++ m ++ cncr ++ m' ++ p'

-- | Compare two @i a@ to determine their 'IntervalRelation'.
--
-- >>> relate (Interval (0::Int, 1)) (Interval (1, 2))
-- Meets
--
-- >>> relate (Interval (1::Int, 2)) (Interval (0, 1))
-- MetBy
-- 
relate
  :: (Ord a, Intervallic i0, Intervallic i1) => i0 a -> i1 a -> IntervalRelation
relate x y | x `before` y       = Before
           | x `after` y        = After
           | x `meets` y        = Meets
           | x `metBy` y        = MetBy
           | x `overlaps` y     = Overlaps
           | x `overlappedBy` y = OverlappedBy
           | x `starts` y       = Starts
           | x `startedBy` y    = StartedBy
           | x `finishes` y     = Finishes
           | x `finishedBy` y   = FinishedBy
           | x `during` y       = During
           | x `contains` y     = Contains
           | otherwise          = Equals

-- | Compose two interval relations according to the rules of the algebra.
--   The rules are enumerated according to
-- <https://thomasalspaugh.org/pub/fnd/allen.html#BasicCompositionsTable this table>.
compose
  :: IntervalRelation -> IntervalRelation -> Data.Set.Set IntervalRelation
compose x y = toSet (composeRelationLookup !! fromEnum x !! fromEnum y)

-- | Finds the complement of a @'Data.Set.Set' 'IntervalRelation'@.
complement :: Data.Set.Set IntervalRelation -> Data.Set.Set IntervalRelation
complement = Data.Set.difference intervalRelations

-- | Find the intersection of two 'Data.Set.Set's of 'IntervalRelation's.
intersection
  :: Data.Set.Set IntervalRelation
  -> Data.Set.Set IntervalRelation
  -> Data.Set.Set IntervalRelation
intersection = Data.Set.intersection

-- | Find the union of two 'Data.Set.Set's of 'IntervalRelation's.
union
  :: Data.Set.Set IntervalRelation
  -> Data.Set.Set IntervalRelation
  -> Data.Set.Set IntervalRelation
union = Data.Set.union

-- | Find the converse of a @'Data.Set.Set' 'IntervalRelation'@. 
converse :: Data.Set.Set IntervalRelation -> Data.Set.Set IntervalRelation
converse = Data.Set.map converseRelation

{- |
The 'IntervalSizeable' typeclass provides functions to determine the size of an
'Intervallic' type and to resize an 'Interval a'.
-}
class (Ord a, Num b, Ord b) => IntervalSizeable a b | a -> b where

    -- | The smallest duration for an 'Interval a'.
    moment :: forall a . b
    moment = 1

    -- | Determine the duration of an @'i a'@.
    duration :: (Intervallic i) => i a -> b
    duration x = diff (end x) (begin x)

    -- | Shifts an @a@. Most often, the @b@ will be the same type as @a@. 
    --   But for example, if @a@ is 'Day' then @b@ could be 'Int'.
    add :: b -> a -> a

    -- | Takes the difference between two @a@ to return a @b@.
    diff :: a -> a -> b

-- | Resize an @i a@ to by expanding to "left" by @l@ and to the 
--   "right" by @r@. In the case that @l@ or @r@ are less than a 'moment'
--   the respective endpoints are unchanged. 
--
-- >>> expand 0 0 (Interval (0::Int, 2::Int))
-- (0, 2)
--
-- >>> expand 1 1 (Interval (0::Int, 2::Int))
-- (-1, 3)
--
expand
  :: forall i a b
   . (IntervalSizeable a b, Intervallic i)
  => b -- ^ duration to subtract from the 'begin'
  -> b -- ^ duration to add to the 'end'
  -> i a
  -> i a
expand l r p = setInterval p i
 where
  s = if l < moment @a then 0 else negate l
  e = if r < moment @a then 0 else r
  i = Interval (add s $ begin p, add e $ end p)

-- | Expands an @i a@ to "left".
--
-- >>> expandl 2 (Interval (0::Int, 2::Int))
-- (-2, 2)
--
expandl :: (IntervalSizeable a b, Intervallic i) => b -> i a -> i a
expandl i = expand i 0

-- | Expands an @i a@ to "right".
--
-- >>> expandr 2 (Interval (0::Int, 2::Int))
-- (0, 4)
--
expandr :: (IntervalSizeable a b, Intervallic i) => b -> i a -> i a
expandr = expand 0

-- | Safely creates an 'Interval a' using @x@ as the 'begin' and adding 
--   @max 'moment' dur@ to @x@ as the 'end'.
--
-- >>> beginerval (0::Int) (0::Int)
-- (0, 1)
--
-- >>> beginerval (1::Int) (0::Int)
-- (0, 1)
--
-- >>> beginerval (2::Int) (0::Int)
-- (0, 2)
--
beginerval
  :: forall a b
   . (IntervalSizeable a b)
  => b -- ^ @dur@ation to add to the 'begin' 
  -> a -- ^ the 'begin' point of the 'Interval'
  -> Interval a
beginerval dur x = Interval (x, y)
 where
  i = Interval (x, x)
  d = max (moment @a) dur
  y = add d x
{-# INLINABLE beginerval #-}

-- | A synonym for `beginerval`
bi
  :: (IntervalSizeable a b)
  => b -- ^ @dur@ation to add to the 'begin' 
  -> a -- ^ the 'begin' point of the 'Interval'
  -> Interval a
bi = beginerval


-- | Safely creates an 'Interval a' using @x@ as the 'end' and adding
--   @negate max 'moment' dur@ to @x@ as the 'begin'.
--
-- >>> enderval (0::Int) (0::Int)
-- (-1, 0)
--
-- >>> enderval (1::Int) (0::Int)
-- (-1, 0)
--
-- >>> enderval (2::Int) (0::Int)
-- (-2, 0)
--
enderval
  :: forall a b
   . (IntervalSizeable a b)
  => b -- ^ @dur@ation to subtract from the 'end' 
  -> a -- ^ the 'end' point of the 'Interval'
  -> Interval a
enderval dur x = Interval (add (negate $ max (moment @a) dur) x, x)
  where i = Interval (x, x)
{-# INLINABLE enderval #-}

-- | A synonym for `enderval`
ei
  :: (IntervalSizeable a b)
  => b -- ^ @dur@ation to subtract from the 'end' 
  -> a -- ^ the 'end' point of the 'Interval'
  -> Interval a
ei = enderval


-- | Safely creates an @'Interval'@ from a pair of endpoints.
-- IMPORTANT: This function uses 'beginerval', 
-- thus if the second element of the pair is `<=` the first element,
-- the duration will be an @"Interval"@ of 'moment' duration.
--
-- >>> safeInterval (4, 5 ::Int)
-- (4, 5)
-- >>> safeInterval (4, 3 :: Int)
-- (4, 5)
--
safeInterval :: IntervalSizeable a b => (a, a) -> Interval a
safeInterval (b, e) = beginerval (diff e b) b

-- | A synonym for `safeInterval`
si :: IntervalSizeable a b => (a, a) -> Interval a
si = safeInterval

-- | Creates a new Interval from the 'end' of an @i a@.
beginervalFromEnd
  :: (IntervalSizeable a b, Intervallic i)
  => b  -- ^ @dur@ation to add to the 'end' 
  -> i a -- ^ the @i a@ from which to get the 'end'
  -> Interval a
beginervalFromEnd d i = beginerval d (end i)

-- | Creates a new Interval from the 'begin' of an @i a@.
endervalFromBegin
  :: (IntervalSizeable a b, Intervallic i)
  => b -- ^ @dur@ation to subtract from the 'begin'  
  -> i a -- ^ the @i a@ from which to get the 'begin'
  -> Interval a
endervalFromBegin d i = enderval d (begin i)

-- | Safely creates a new @Interval@ with 'moment' length with 'begin' at @x@
--
-- >>> beginervalMoment (10 :: Int)
-- (10, 11)

-- 
beginervalMoment :: forall a b . (IntervalSizeable a b) => a -> Interval a
beginervalMoment x = beginerval (moment @a) x where i = Interval (x, x)

-- | Safely creates a new @Interval@ with 'moment' length with 'end' at @x@
--
-- >>> endervalMoment (10 :: Int)
-- (9, 10)
-- 
endervalMoment :: forall a b . (IntervalSizeable a b) => a -> Interval a
endervalMoment x = enderval (moment @a) x where i = Interval (x, x)

-- | Creates a new @Interval@ spanning the extent x and y.
--
-- >>> extenterval (Interval (0, 1)) (Interval (9, 10))
-- (0, 10)
--
extenterval :: (Ord a, Intervallic i) => i a -> i a -> Interval a
extenterval x y = Interval (s, e)
 where
  s = min (begin x) (begin y)
  e = max (end x) (end y)

-- | Modifies the endpoints of second argument's interval by taking the difference
--   from the first's input's 'begin'. 
-- >>> shiftFromBegin (Interval ((5::Int), 6)) (Interval (10, 15))
-- (5, 10)
--
-- >>> shiftFromBegin (Interval ((1::Int), 2)) (Interval (3, 15))
-- (2, 14)
--
shiftFromBegin
  :: (IntervalSizeable a b, Intervallic i1, Intervallic i0)
  => i0 a
  -> i1 a
  -> i1 b
shiftFromBegin i = imapStrictMonotone (`diff` begin i)

-- | Modifies the endpoints of second argument's interval by taking the difference
--   from the first's input's 'end'.
-- >>> shiftFromEnd (Interval ((5::Int), 6)) (Interval (10, 15))
-- (4, 9)
--
-- >>> shiftFromEnd (Interval ((1::Int), 2)) (Interval (3, 15))
-- (1, 13)
--
shiftFromEnd
  :: (IntervalSizeable a b, Intervallic i1, Intervallic i0)
  => i0 a
  -> i1 a
  -> i1 b
shiftFromEnd i = imapStrictMonotone (`diff` end i)

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
--   'begin' of the interval.
--
-- >>> momentize (Interval (6, 10))
-- (6, 7)
--
momentize
  :: forall i a b . (IntervalSizeable a b, Intervallic i) => i a -> i a
momentize i = setInterval i (beginerval (moment @a) (begin i))

{- |
The @'IntervalCombinable'@ typeclass provides methods for (possibly) combining
two @i a@s to form a @'Maybe' i a@, or in case of @><@, a possibly different 
@Intervallic@ type.
-}
class (Ord a, Intervallic i) => IntervalCombinable i a where

    -- | Maybe form a new @i a@ by the union of two @i a@s that 'meets'.
    (.+.) ::  i a -> i a -> Maybe (i a)
    (.+.) x y
      | x `meets` y = Just $ setInterval y $ Interval (b, e)
      | otherwise   = Nothing
      where b = begin x
            e = end y
    {-# INLINABLE (.+.) #-}

    -- | If @x@ is 'before' @y@, then form a new @Just Interval a@ from the 
    --   interval in the "gap" between @x@ and @y@ from the 'end' of @x@ to the
    --   'begin' of @y@. Otherwise, 'Nothing'.
    (><) :: i a -> i a -> Maybe (i a)

    -- | If @x@ is 'before' @y@, return @f x@ appended to @f y@. Otherwise, 
    --   return 'extenterval' of @x@ and @y@ (wrapped in @f@). This is useful for 
    --   (left) folding over an *ordered* container of @Interval@s and combining 
    --   intervals when @x@ is *not* 'before' @y@.
    (<+>):: ( Semigroup (f (i a)), Applicative f) =>
               i a
            -> i a
            -> f (i a)
{-# DEPRECATED (<+>) "A specialized function without clear use-cases." #-}

{-
Misc
-}

-- | Defines a predicate of two objects of type @a@.
type ComparativePredicateOf1 a = (a -> a -> Bool)

-- | Defines a predicate of two object of different types.
type ComparativePredicateOf2 a b = (a -> b -> Bool)

-- {-
-- Instances
-- -}

-- | Imposes a total ordering on @'Interval' a@ based on first ordering the 
--   'begin's then the 'end's.
instance (Ord a) => Ord (Interval a) where
  (<=) x y | begin x < begin y  = True
           | begin x == begin y = end x <= end y
           | otherwise          = False
  (<) x y | begin x < begin y  = True
          | begin x == begin y = end x < end y
          | otherwise          = False

instance Intervallic Interval where
  getInterval = id
  setInterval _ x = x

instance (Ord a) => IntervalCombinable Interval a where
  (><) x y | x `before` y = Just $ Interval (end x, begin y)
           | otherwise    = Nothing
  {-# INLINABLE (><) #-}

  (<+>) x y | x `before` y = pure x <> pure y
            | otherwise    = pure (extenterval x y)
  {-# INLINABLE (<+>) #-}

instance IntervalSizeable Int Int where
  moment = 1
  add    = (+)
  diff   = (-)

instance IntervalSizeable Integer Integer where
  moment = 1
  add    = (+)
  diff   = (-)

instance IntervalSizeable DT.Day Integer where
  moment = 1
  add    = addDays
  diff   = diffDays

-- | Note that the @moment@ of this instance is a @'Data.Fixed.Pico'@
instance IntervalSizeable DT.UTCTime NominalDiffTime where
  moment = toEnum 1 :: NominalDiffTime
  add    = addUTCTime
  diff   = diffUTCTime

-- Arbitrary instances
instance (Ord a, Arbitrary a) => Arbitrary (Interval a) where
  arbitrary =
    sized
        (\s -> liftA2 (curry Interval)
                      (s `resize` arbitrary)
                      (s `resize` arbitrary)
        )
      `suchThat` (\i -> isValidBeginEnd (intervalBegin i) (intervalEnd i))
