{- HLINT ignore -}
{-|
Module      : Interval Algebra Axioms
Description : Properties of Intervals
Copyright   : (c) NoviSci, Inc 2020
License     : BSD3
Maintainer  : bsaul@novisci.com

This module exports a single typeclass @IntervalAxioms@ which contains
property-based tests for the axioms in section 1 of [Allen and Hayes (1987)](https://doi.org/10.1111/j.1467-8640.1989.tb00329.x).
The notation below is that of the original paper.

This module is useful if creating a new instance of interval types that you want to test.

-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}


module IntervalAlgebra.Axioms
  ( IntervalAxioms(..)
  , M1set(..)
  , M2set(..)
  , M5set(..)
  ) where

import           Data.Either               (isRight)
import           Data.Maybe                (fromJust, isJust, isNothing)
import           Data.Set                  (Set, disjointUnion, fromList,
                                            member)
import           Data.Time                 as DT (Day (..), DiffTime,
                                                  NominalDiffTime, UTCTime (..))
import           IntervalAlgebra.Arbitrary
import           IntervalAlgebra.Core
import           Test.QuickCheck           (Arbitrary (arbitrary), Property,
                                            (===), (==>))


xor :: Bool -> Bool -> Bool
xor a b = a /= b

-- | Internal function for converting a number to a strictly positive value.
makePos :: (Ord b, Num b) => b -> b
makePos x | x == 0    = x + 1
          | x < 0     = negate x
          | otherwise = x

-- | A set used for testing M1 defined so that the M1 condition is true.
data M1set a = M1set
  { m11 :: Interval a
  , m12 :: Interval a
  , m13 :: Interval a
  , m14 :: Interval a
  }
  deriving Show

instance Arbitrary (M1set Int) where
  arbitrary = do
    x <- arbitrary
    a <- arbitrary
    b <- arbitrary
    m1set x a b <$> arbitrary

instance Arbitrary (M1set DT.Day) where
  arbitrary = do
    x <- arbitrary
    a <- arbitrary
    b <- arbitrary
    m1set x a b <$> arbitrary

instance Arbitrary (M1set DT.UTCTime) where
  arbitrary = do
    x <- arbitrary
    a <- arbitrary
    b <- arbitrary
    m1set x a b <$> arbitrary


-- | A set used for testing M2 defined so that the M2 condition is true.
data M2set a = M2set
  { m21 :: Interval a
  , m22 :: Interval a
  , m23 :: Interval a
  , m24 :: Interval a
  }
  deriving Show

instance Arbitrary (M2set Int) where
  arbitrary = do
    x <- arbitrary
    a <- arbitrary
    b <- arbitrary
    m2set x a b <$> arbitrary

instance Arbitrary (M2set DT.Day) where
  arbitrary = do
    x <- arbitrary
    a <- arbitrary
    b <- arbitrary
    m2set x a b <$> arbitrary

instance Arbitrary (M2set DT.UTCTime) where
  arbitrary = do
    x <- arbitrary
    a <- arbitrary
    b <- arbitrary
    m2set x a b <$> arbitrary

-- | A set used for testing M5.
data M5set a = M5set
  { m51 :: Interval a
  , m52 :: Interval a
  }
  deriving Show

instance Arbitrary (M5set Int) where
  arbitrary = do
    x <- arbitrary
    a <- arbitrary
    m5set x a <$> arbitrary

instance Arbitrary (M5set DT.Day) where
  arbitrary = do
    x <- arbitrary
    a <- arbitrary
    m5set x a <$> arbitrary

instance Arbitrary (M5set DT.UTCTime) where
  arbitrary = do
    x <- arbitrary
    a <- arbitrary
    m5set x a <$> arbitrary

-- | = "An Axiomatization of Interval Time".
class ( IntervalSizeable a b ) => IntervalAxioms a b where

    -- | Smart constructor of 'M1set'.
    m1set :: (IntervalSizeable a b) => Interval a -> b -> b -> b -> M1set a
    m1set x a b c = M1set p1 p2 p3 p4
      where p1 = x                          -- interval i in prop_IAaxiomM1
            p2 = beginerval a (end x)       -- interval j in prop_IAaxiomM1
            p3 = beginerval b (end x)       -- interval k in prop_IAaxiomM1
            p4 = enderval (makePos c) (begin p2)

    {- |

    == Axiom M1

    The first axiom of Allen and Hayes (1987) states that if "two periods both
    meet a third, thn any period met by one must also be met by the other."
    That is:

    \[
      \forall \text{ i,j,k,l } s.t. (i:j \text{ & } i:k \text{ & } l:j) \implies l:k
    \]
    -}
    prop_IAaxiomM1 :: (Ord a) => M1set a -> Property
    prop_IAaxiomM1 x =
      (i `meets` j && i `meets` k && l `meets` j) ==> (l `meets` k)
      where i = m11 x
            j = m12 x
            k = m13 x
            l = m14 x

    -- | Smart constructor of 'M2set'.
    m2set :: (IntervalSizeable a b)=> Interval a -> Interval a -> b -> b -> M2set a
    m2set x y a b = M2set p1 p2 p3 p4
      where p1 = x                          -- interval i in prop_IAaxiomM2
            p2 = beginerval a (end x)       -- interval j in prop_IAaxiomM2
            p3 = y                          -- interval k in prop_IAaxiomM2
            p4 = beginerval b (end y)       -- interval l in prop_IAaxiomM2

    {- |

    == Axiom M2

    If period i meets period j and period k meets l,
    then exactly one of the following holds:

      1) i meets l;
      2) there is an m such that i meets m and m meets l;
      3) there is an n such that k meets n and n meets j.

    That is,

    \[
      \forall i,j,k,l s.t. (i:j \text { & } k:l) \implies
        i:l \oplus
        (\exists m s.t. i:m:l) \oplus
        (\exists m s.t. k:m:j)
    \]

    -}

    prop_IAaxiomM2 :: (IntervalSizeable a b, Show a) =>
        M2set a -> Property
    prop_IAaxiomM2 x =
      (i `meets` j && k `meets` l) ==>
        (i `meets` l)  `xor`
        isRight m `xor`
        isRight n
        where i = m21 x
              j = m22 x
              k = m23 x
              l = m24 x
              m = parseInterval (end i) (begin l)
              n = parseInterval (end k) (begin j)

    {- |

    == Axiom ML1

    An interval cannot meet itself.

    \[
      \forall i \lnot i:i
    \]
    -}

    prop_IAaxiomML1 :: (Ord a) => Interval a -> Property
    prop_IAaxiomML1 x = not (x `meets` x) === True

    {- |

    == Axiom ML2

    If i meets j then j does not meet i.

    \[
    \forall i,j i:j \implies \lnot j:i
    \]
    -}

    prop_IAaxiomML2 :: (Ord a)=> M2set a -> Property
    prop_IAaxiomML2 x =
      (i `meets` j) ==> not (j `meets` i)
      where i = m21 x
            j = m22 x

    {- |

    == Axiom M3

    Time does not start or stop:

    \[
    \forall i \exists j,k s.t. j:i:k
    \]
    -}

    prop_IAaxiomM3 :: (IntervalSizeable a b)=>
          b -> Interval a -> Property
    prop_IAaxiomM3 b i =
      (j `meets` i && i `meets` k) === True
      where j = enderval   b (begin i)
            k = beginerval b (end i)

    {- |
      ML3 says that For all i, there does not exist m such that i meets m and
      m meet i. Not testing that this axiom holds, as I'm not sure how I would
      test the lack of existence easily.
    -}

    {- |

    == Axiom M4

    If two meets are separated by intervals, then this sequence is a longer interval.

    \[
    \forall i,j i:j \implies (\exists k,m,n s.t m:i:j:n \text { & } m:k:n)
    \]
    -}

    prop_IAaxiomM4 :: (IntervalSizeable a b)=>
        b -> M2set a -> Property
    prop_IAaxiomM4 b x =
      ((m `meets` i && i `meets` j && j `meets` n) &&
        (m `meets` k && k `meets` n)) === True
      where i = m21 x
            j = m22 x
            m = enderval   b (begin i)
            n = beginerval b (end j)
            k = beginerval g (end m)
            g = diff (begin n) (end m)


    -- | Smart constructor of 'M5set'.
    m5set :: (IntervalSizeable a b)=> Interval a -> b -> b -> M5set a
    m5set x a b = M5set p1 p2
      where p1 = x                     -- interval i in prop_IAaxiomM5
            p2 = beginerval a ps       -- interval l in prop_IAaxiomM5
            ps = end (expandr (makePos b) x) -- creating l by shifting and expanding i

    {- |

    == Axiom M5

    There is only one time period between any two meeting places.

    \[
    \forall i,j,k,l (i:j:l \text{ & } i:k:l) \equiv j = k
    \]
    -}
    prop_IAaxiomM5 :: (IntervalSizeable a b) =>
        M5set a -> Property
    prop_IAaxiomM5 x =
      ((i `meets` j && j `meets` l) &&
       (i `meets` k && k `meets` l)) === (j == k)
      where i = m51 x
            j = beginerval g (end i)
            k = beginerval g (end i)
            g = diff (begin l) (end i)
            l = m52 x

    {- |

    == Axiom M4.1

    Ordered unions:

    \[
    \forall i,j i:j \implies (\exists m,n s.t. m:i:j:n \text{ & } m:(i+j):n)
    \]
    -}
    prop_IAaxiomM4_1 :: (IntervalSizeable a b)=>
                    b -> M2set a -> Property
    prop_IAaxiomM4_1 b x =
      ((m `meets` i && i `meets` j && j `meets` n) &&
        (m `meets` ij && ij `meets` n)) === True
      where i = m21 x
            j = m22 x
            m = enderval   b (begin i)
            n = beginerval b (end j)
            ij = fromJust $ i .+. j

instance IntervalAxioms Int Int
instance IntervalAxioms Day Integer
instance IntervalAxioms UTCTime NominalDiffTime
