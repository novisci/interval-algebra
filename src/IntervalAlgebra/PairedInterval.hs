{-|
Module      : Paired interval 
Description : Extends the Interval Algebra to an interval paired with some data
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com
Stability   : experimental
-}

module IntervalAlgebra.PairedInterval () where

import IntervalAlgebra
    ( Interval, Intervallic(..), IntervalAlgebraic(..)
    , IntervalCombinable(..), IntervalSizeable(..)
    , IntervalRelation(..)
    , ComparativePredicateOf)
import Witherable ( Filterable(filter) )
newtype PairedInterval a b = PairedInterval (Interval a, b)

interval :: PairedInterval a b -> Interval a
interval (PairedInterval (x, _)) = x

begin' :: Intervallic a => PairedInterval a b -> a
begin' = begin . interval

end' :: Intervallic a => PairedInterval a b -> a
end' = end . interval

pairData :: PairedInterval a b -> b
pairData (PairedInterval (_, y)) = y

intervals :: [PairedInterval a b] -> [Interval a]
intervals = map interval

liftIntervalPredicate :: (IntervalAlgebraic a) =>
       ComparativePredicateOf (Interval a)
    -> ComparativePredicateOf (PairedInterval a b)
liftIntervalPredicate f x y = f (interval x) (interval y)

makePairPredicate :: (IntervalAlgebraic a) =>
       ComparativePredicateOf (Interval a)
    -> ComparativePredicateOf b
    -> ComparativePredicateOf (PairedInterval a b)
makePairPredicate intervalPredicate dataPredicate x y =
         liftIntervalPredicate intervalPredicate x y &&
         dataPredicate (pairData x) (pairData y)

compareToInterval :: (IntervalAlgebraic a) =>
    ComparativePredicateOf (Interval a)
    -> Interval a
    -> PairedInterval a b
    -> Bool
compareToInterval f x y = f x (interval y)

-- |Creates a function for filtering a 'Witherable.Filterable' of @Interval a@s based on a predicate
filterMaker' :: (Filterable f, IntervalAlgebraic a) =>
                 ComparativePredicateOf (Interval a)
                -> Interval a
                -> (f (PairedInterval a b) -> f (PairedInterval a b))
filterMaker' f p = Witherable.filter (compareToInterval f p)


-- filterBefore' :: 
    -- , filterMeets
    -- , filterOverlaps
    -- , filterFinishedBy
    -- , filterContains
    -- , filterStarts
    -- , filterEquals
    -- , filterStartedBy
    -- , filterDuring
    -- , filterFinishes
    -- , filterOverlappedBy
    -- , filterMetBy
    -- , filterAfter
    -- , filterDisjoint
    -- , filterNotDisjoint
    -- , filterConcur
    -- , filterWithin
    -- , filterEnclose
    -- , filterEnclosedBy