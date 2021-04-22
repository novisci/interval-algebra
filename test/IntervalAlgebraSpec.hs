{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

-- {-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# LANGUAGE FlexibleContexts #-}
module IntervalAlgebraSpec (spec) where

import Test.Hspec ( hspec, describe, it, Spec, shouldBe )
import Test.Hspec.QuickCheck ( modifyMaxSuccess, modifyMaxDiscardRatio )
import Test.QuickCheck
import IntervalAlgebra as IA
import Data.Maybe
import Control.Monad ()
import IntervalAlgebra.Arbitrary ()
import Data.Time as DT

xor :: Bool -> Bool -> Bool
xor a b = a /= b

-- | Internal function for converting a number to a strictly positive value.
makePos :: (Ord b, Num b) => b -> b
makePos x
  | x == 0    = x + 1
  | x <  0    = negate x
  | otherwise = x

-- | A function for creating intervals when you think you know what you're doing.
safeInterval :: (Intervallic a) => a -> a -> Interval a
safeInterval x y = unsafeInterval (min x y) (max x y)

-- | Create a 'Maybe Interval a' from two @a@s.
safeInterval'' :: (Intervallic a) => a -> a -> Maybe (Interval a)
safeInterval'' x y
    | y <= x    = Nothing
    | otherwise = Just $ safeInterval x y

-- | A set used for testing M1 defined so that the M1 condition is true.
data M1set a = M1set {
     m11 :: Interval a
   , m12 :: Interval a
   , m13 :: Interval a
   , m14 :: Interval a }
   deriving (Show)

-- TODO: remove duplication like this:
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

-- | Smart constructor of 'M1set'.
m1set :: (IntervalSizeable a b) => Interval a -> b -> b -> b -> M1set a
m1set x a b c = M1set p1 p2 p3 p4
  where p1 = x                          -- interval i in prop_IAaxiomM1
        p2 = beginerval a (end x)       -- interval j in prop_IAaxiomM1
        p3 = beginerval b (end x)       -- interval k in prop_IAaxiomM1
        p4 = safeInterval  (begin (expandl (makePos c) p2)) (begin p2)

{-

 ** Axiom M1

 The first axiom of Allen and Hayes (1987) states that if "two periods both
 meet a third, thn any period met by one must also be met by the other." 
 That is:

 \[
   \forall i,j,k,l s.t. (i:j & i:k & l:j) \implies l:k
 \] 
-}
prop_IAaxiomM1 :: (IntervalAlgebraic a) => M1set a -> Property
prop_IAaxiomM1 x =
  (i `meets` j && i `meets` k && l `meets` j) ==> (l `meets` k)
  where i = m11 x
        j = m12 x
        k = m13 x
        l = m14 x

prop_IAaxiomM1_Int :: M1set Int -> Property
prop_IAaxiomM1_Int = prop_IAaxiomM1

prop_IAaxiomM1_Day :: M1set DT.Day -> Property
prop_IAaxiomM1_Day = prop_IAaxiomM1

-- | A set used for testing M2 defined so that the M2 condition is true.
data M2set a = M2set {
    m21 :: Interval a
  , m22 :: Interval a
  , m23 :: Interval a
  , m24 :: Interval a }
  deriving (Show)

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

-- | Smart constructor of 'M2set'.
m2set :: (IntervalSizeable a b)=> Interval a -> Interval a -> b -> b -> M2set a
m2set x y a b = M2set p1 p2 p3 p4
  where p1 = x                          -- interval i in prop_IAaxiomM2
        p2 = beginerval a (end x)       -- interval j in prop_IAaxiomM2
        p3 = y                          -- interval k in prop_IAaxiomM2
        p4 = beginerval b (end y) -- interval l in prop_IAaxiomM2

{-

** Axiom M2

If period i meets period j and period k meets l, 
then exactly one of the following holds:
  1) i meets l; 
  2) there is an m such that i meets m and m meets l; 
  3) there is an n such that k meets n and n meets j.
   
That is,

 \[
   \forall i,j,k,l s.t. (i:j & k:l) \implies 
     i:l \oplus 
     (\exists m s.t. i:m:l) \oplus
     (\exists m s.t. k:m:j) 
 \] 
-}

prop_IAaxiomM2 :: (IntervalAlgebraic a) => M2set a -> Property
prop_IAaxiomM2 x =
  (i `meets` j && k `meets` l) ==>
    (i `meets` l)  `xor`
    isJust m `xor`
    isJust n
    where i = m21 x
          j = m22 x
          k = m23 x
          l = m24 x
          m = safeInterval'' (end i) (begin l)
          n = safeInterval'' (end k) (begin j)

prop_IAaxiomM2_Int :: M2set Int -> Property
prop_IAaxiomM2_Int = prop_IAaxiomM2

prop_IAaxiomM2_Day :: M2set DT.Day -> Property
prop_IAaxiomM2_Day = prop_IAaxiomM2

{-

 ** Axiom ML1

 An interval cannot meet itself.

 \[
   \forall i \lnot i:i
 \] 
-}

prop_IAaxiomML1 :: (IntervalAlgebraic a) => Interval a -> Property
prop_IAaxiomML1 x = not (x `meets` x) === True

prop_IAaxiomML1_Int :: Interval Int -> Property
prop_IAaxiomML1_Int = prop_IAaxiomML1

prop_IAaxiomML1_Day :: Interval DT.Day -> Property
prop_IAaxiomML1_Day = prop_IAaxiomML1

{-

** Axiom ML2

If i meets j then j does not meet i.

\[
 \forall i,j i:j \implies \lnot j:i
\] 
-}

prop_IAaxiomML2 :: (IntervalAlgebraic a)=> M2set a -> Property
prop_IAaxiomML2 x =
  (i `meets` j) ==> not (j `meets` i)
  where i = m21 x
        j = m22 x

prop_IAaxiomML2_Int :: M2set Int -> Property
prop_IAaxiomML2_Int = prop_IAaxiomML2

prop_IAaxiomML2_Day :: M2set DT.Day -> Property
prop_IAaxiomML2_Day = prop_IAaxiomML2

{-

** Axiom M3

Time does not start or stop:

\[
 \forall i \exists j,k s.t. j:i:k
\] 
-}

prop_IAaxiomM3 :: (IntervalAlgebraic a, IntervalSizeable a b)=>
      b -> Interval a -> Property
prop_IAaxiomM3 b i =
   (j `meets` i && i `meets` k) === True
   where j = safeInterval (begin (expandl b i)) (begin i)
         k = safeInterval (end i) (end (expandr b i))

prop_IAaxiomM3_Int :: Interval Int -> Property
prop_IAaxiomM3_Int = prop_IAaxiomM3 1

prop_IAaxiomM3_Day :: Interval Day -> Property
prop_IAaxiomM3_Day = prop_IAaxiomM3 1

{-

** Axiom M4

If two meets are separated by intervals, then this sequence is a longer interval.

\[
 \forall i,j i:j \implies (\exists k,m,n s.t m:i:j:n & m:k:n) 
\] 
-}

prop_IAaxiomM4 :: (IntervalAlgebraic a, IntervalSizeable a b)=>
     b -> M2set a -> Property
prop_IAaxiomM4 moment x =
   ((m `meets` i && i `meets` j && j `meets` n) &&
    (m `meets` k && k `meets` n)) === True
   where i = m21 x
         j = m22 x
         m = safeInterval (begin (expandl moment i)) (begin i)
         n = safeInterval (end j) (end (expandr moment j))
         k = safeInterval (end m) (begin n)

prop_IAaxiomM4_Int :: M2set Int -> Property
prop_IAaxiomM4_Int = prop_IAaxiomM4 1

prop_IAaxiomM4_Day :: M2set DT.Day -> Property
prop_IAaxiomM4_Day = prop_IAaxiomM4 1

{-

** Axiom M5

If two meets are separated by intervals, then this sequence is a longer interval.

\[
 \forall i,j,k,l (i:j:l & i:k:l) \seteq j = k
\] 
-}

-- | A set used for testing M5.
data M5set a = M5set {
     m51 :: Interval a
   , m52 :: Interval a }
   deriving (Show)

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

-- | Smart constructor of 'M5set'.
m5set :: (IntervalSizeable a b)=> Interval a -> b -> b -> M5set a
m5set x a b = M5set p1 p2
  where p1 = x                     -- interval i in prop_IAaxiomM5
        p2 = beginerval a ps       -- interval l in prop_IAaxiomM5
        ps = end (expandr (makePos b) x) -- creating l by shifting and expanding i


prop_IAaxiomM5 :: (IntervalAlgebraic a) => M5set a -> Property
prop_IAaxiomM5 x =
   ((i `meets` j && j `meets` l) &&
    (i `meets` k && k `meets` l))  === (j == k)
   where i = m51 x
         j = safeInterval (end i) (begin l)
         k = safeInterval (end i) (begin l)
         l = m52 x

prop_IAaxiomM5_Int :: M5set Int -> Property
prop_IAaxiomM5_Int = prop_IAaxiomM5

prop_IAaxiomM5_Day :: M5set DT.Day -> Property
prop_IAaxiomM5_Day = prop_IAaxiomM5

{-

** Axiom M4.1

Ordered unions:

\[
 \forall i,j i:j \implies (\exists m,n s.t. m:i:j:n & m:(i+j):n)
\] 
-}

prop_IAaxiomM4_1 :: (IntervalSizeable a b, IntervalCombinable a)=>
                    b -> M2set a -> Property
prop_IAaxiomM4_1 b x =
   ((m `meets` i && i `meets` j && j `meets` n) &&
    (m `meets` ij && ij `meets` n)) === True
   where i = m21 x
         j = m22 x
         m = safeInterval (begin (expandl b i)) (begin i)
         n = safeInterval (end j) (end (expandr b j))
         ij = fromJust $ i .+. j

prop_IAaxiomM4_1_Int :: M2set Int -> Property
prop_IAaxiomM4_1_Int = prop_IAaxiomM4_1 1

prop_IAaxiomM4_1_Day :: M2set DT.Day -> Property
prop_IAaxiomM4_1_Day = prop_IAaxiomM4_1 1

{-
* Interval Relation property testing 
-}

class (IntervalAlgebraic a, IntervalCombinable a)=> IntervalRelationProperties a where

    prop_IAbefore :: Interval a -> Interval a -> Property
    prop_IAbefore i j =
      IA.before i j ==> (i `meets` k) && (k `meets` j)
        where k = safeInterval (end i) (begin j)

    prop_IAstarts:: Interval a -> Interval a -> Property
    prop_IAstarts i j
      | IA.starts i j = (j == fromJust (i .+. k)) === True
      | otherwise     = IA.starts i j === False
        where k  = safeInterval (end i) (end j)

    prop_IAfinishes:: Interval a -> Interval a -> Property
    prop_IAfinishes i j
      | IA.finishes i j = (j == fromJust ( k .+. i)) === True
      | otherwise       = IA.finishes i j === False
        where k = safeInterval (begin j) (begin i)

    prop_IAoverlaps:: Interval a -> Interval a -> Property
    prop_IAoverlaps i j
      | IA.overlaps i j = ((i == fromJust ( k .+. l )) &&
                          (j == fromJust ( l .+. m ))) === True
      | otherwise       = IA.overlaps i j === False
        where k = safeInterval (begin i) (begin j)
              l = safeInterval (begin j) (end i)
              m = safeInterval (end i)   (end j)

    prop_IAduring:: Interval a -> Interval a-> Property
    prop_IAduring i j
      | IA.during i j = (j == fromJust ( fromJust (k .+. i) .+. l)) === True
      | otherwise     = IA.during i j === False
        where k = safeInterval (begin j) (begin i)
              l = safeInterval (end i) (end j)

    -- | For any two pair of intervals exactly one 'IntervalRelation' should hold
    prop_exclusiveRelations::  Interval a -> Interval a -> Property
    prop_exclusiveRelations x y =
      (  1 == length (filter id $ map (\r -> r x y) allIArelations)) === True

instance IntervalRelationProperties Int

allIArelations:: IntervalAlgebraic a => [ComparativePredicateOf (Interval a)]
allIArelations =   [  IA.equals
                    , IA.meets
                    , IA.metBy
                    , IA.before
                    , IA.after
                    , IA.starts
                    , IA.startedBy
                    , IA.finishes
                    , IA.finishedBy
                    , IA.overlaps
                    , IA.overlappedBy
                    , IA.during
                    , IA.contains ]

prop_expandl_end ::(IntervalAlgebraic a, IntervalSizeable a b)=>
       b
    -> Interval a
    -> Property
prop_expandl_end d i = end (expandl d i) === end i


prop_expandr_begin ::(IntervalAlgebraic a, IntervalSizeable a b)=>
       b
    -> Interval a
    -> Property
prop_expandr_begin d i = begin (expandr d i) === begin i

spec :: Spec
spec = do
  describe "IntervalSizeable tests" $
    do
      it "expandl doesn't change end"   $ property (prop_expandl_end @Int)  
      it "expandr doesn't change begin" $ property (prop_expandr_begin @Int)  
      it "expand 0 5 Interval (0, 1) should be Interval (0, 6)" $
        expand 0 5 (unsafeInterval (0::Int) (1::Int)) `shouldBe` unsafeInterval (0::Int) (6::Int)
      it "expand 5 0 Interval (0, 1) should be Interval (-5, 1)" $
        expand 5 0 (unsafeInterval (0::Int) (1::Int)) `shouldBe` unsafeInterval (-5::Int) (1::Int)
      it "expand 5 5 Interval (0, 1) should be Interval (-5, 6)" $
        expand 5 5 (unsafeInterval (0::Int) (1::Int)) `shouldBe` unsafeInterval (-5::Int) (6::Int)
      it "expand -1 5 Interval (0, 1) should be Interval (-5, 6)" $
        expand (-1) 5 (unsafeInterval (0::Int) (1::Int)) `shouldBe` unsafeInterval (0::Int) (6::Int)
      it "expand 5 -5 Interval (0, 1) should be Interval (-5, 1)" $
        expand 5 (-5) (unsafeInterval (0::Int) (1::Int)) `shouldBe` unsafeInterval (-5::Int) (1::Int)
      it "expand moment 0 Interval (0, 1) should be Interval (-1, 1)" $
        expand (moment @Int) 0 (unsafeInterval (0::Int) (1::Int)) `shouldBe`
         unsafeInterval (-1::Int) (1::Int)

      it "beginerval 2 10 should be Interval (10, 12)" $
        beginerval (2::Int) 10 `shouldBe` unsafeInterval (10::Int) (12::Int)
      it "beginerval 0 10 should be Interval (10, 11)" $
        beginerval (0::Int) 10 `shouldBe` unsafeInterval (10::Int) (11::Int)
      it "beginerval -2 10 should be Interval (10, 11)" $
        beginerval (-2::Int) 10 `shouldBe` unsafeInterval (10::Int) (11::Int)
      it "enderval 2 10 should be Interval (8, 10)" $
        enderval (2::Int) 10 `shouldBe` unsafeInterval (8::Int) (10::Int)
      it "enderval 0 10 should be Interval (9, 10)" $
        enderval (0::Int) 10 `shouldBe` unsafeInterval (9::Int) (10::Int)
      it "enderval -2 10 should be Interval (9, 10)" $
        enderval (-2::Int) 10 `shouldBe` unsafeInterval (9::Int) (10::Int)

  describe "Interval Algebra Axioms for meets properties" $
    modifyMaxSuccess (*10) $
    do
      it "M1 Int" $ property prop_IAaxiomM1_Int
      it "M1 Day" $ property prop_IAaxiomM1_Day
      it "M2_Int" $ property prop_IAaxiomM2_Int
      it "M2_Day" $ property prop_IAaxiomM2_Day
      it "ML1_Int" $ property prop_IAaxiomML1_Int
      it "ML1_Day" $ property prop_IAaxiomML1_Day
      it "ML2_Int" $ property prop_IAaxiomML2_Int
      it "ML2_Day" $ property prop_IAaxiomML2_Day
      {-
        ML3 says that For all i, there does not exist m such that i meets m and
        m meet i. Not testing that this axiom holds, as I'm not sure how I would
        test the lack of existence.
      -}
      --it "ML3" $ property prop_IAaxiomML3
      it "M3_Int" $ property prop_IAaxiomM3_Int
      it "M3_Day" $ property prop_IAaxiomM3_Day
      it "M4_Int" $ property prop_IAaxiomM4_Int
      it "M4_Day" $ property prop_IAaxiomM4_Day
      it "M5_Int" $ property prop_IAaxiomM5_Int
      it "M5_Day" $ property prop_IAaxiomM5_Day
      it "M4.1_Int" $ property prop_IAaxiomM4_1_Int
      it "M4.1_Day" $ property prop_IAaxiomM4_1_Day


  describe "Interval Algebra relation properties" $
      modifyMaxSuccess (*10) $
    do
      it "before"   $ property (prop_IAbefore @Int)
      it "starts"   $ property (prop_IAstarts @Int)
      it "finishes" $ property (prop_IAfinishes @Int)
      it "overlaps" $ property (prop_IAoverlaps @Int)
      it "during"   $ property (prop_IAduring @Int)

  describe "Interval Algebra relation uniqueness" $
      modifyMaxSuccess (*100) $
    do
      it "exactly one relation must be true" $ property (prop_exclusiveRelations @Int)

