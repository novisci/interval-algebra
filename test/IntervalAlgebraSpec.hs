{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module IntervalAlgebraSpec (spec) where

import Test.Hspec                 ( hspec, describe, it, Spec, shouldBe, pending )
import Test.Hspec.QuickCheck      ( modifyMaxSuccess, modifyMaxDiscardRatio )
import Test.QuickCheck            ( (===)
                                  , (==>)
                                  , Arbitrary(arbitrary)
                                  , Property
                                  ,  Testable(property) )
import Data.Maybe                 ( fromJust )
import Data.Either                ( isRight )
import IntervalAlgebra.Arbitrary  ()
import Data.Time as DT            ( Day )
import Data.Set                   ( member )
import IntervalAlgebra as IA      ( enderval
                                  , beginerval
                                  , expandr
                                  , expandl
                                  , expand
                                  , parseInterval
                                  , before
                                  , meets
                                  , overlaps
                                  , finishedBy
                                  , contains
                                  , starts
                                  , equals
                                  , startedBy
                                  , during
                                  , finishes
                                  , overlappedBy
                                  , metBy
                                  , after
                                  , relate
                                  , compose
                                  , disjoint
                                  , (<|>)
                                  , IntervalCombinable((.+.))
                                  , IntervalSizeable(moment, diff)
                                  , ComparativePredicateOf1
                                  , ComparativePredicateOf2
                                  , Intervallic(begin, end)
                                  , Interval )

mkIntrvl :: Int -> Int -> Interval Int
mkIntrvl = beginerval

xor :: Bool -> Bool -> Bool
xor a b = a /= b

-- | Internal function for converting a number to a strictly positive value.
makePos :: (Ord b, Num b) => b -> b
makePos x
  | x == 0    = x + 1
  | x <  0    = negate x
  | otherwise = x

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
        p4 = enderval (makePos c) (begin p2)

{-

 ** Axiom M1

 The first axiom of Allen and Hayes (1987) states that if "two periods both
 meet a third, thn any period met by one must also be met by the other." 
 That is:

 \[
   \forall i,j,k,l s.t. (i:j & i:k & l:j) \implies l:k
 \] 
-}
prop_IAaxiomM1 :: (Ord a) => M1set a -> Property
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

prop_IAaxiomML1 :: (Ord a) => Interval a -> Property
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

prop_IAaxiomML2 :: (Ord a)=> M2set a -> Property
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

prop_IAaxiomM3 :: (IntervalSizeable a b)=>
      b -> Interval a -> Property
prop_IAaxiomM3 b i =
   (j `meets` i && i `meets` k) === True
   where j = enderval   b (begin i) 
         k = beginerval b (end i)

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


prop_IAaxiomM5 :: (IntervalSizeable a b) => 
    M5set a -> Property
prop_IAaxiomM5 x =
   ((i `meets` j && j `meets` l) &&
    (i `meets` k && k `meets` l))  === (j == k)
   where i = m51 x
         j = beginerval g (end i)
         k = beginerval g (end i)
         g = diff (begin l) (end i)
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

prop_IAaxiomM4_1_Int :: M2set Int -> Property
prop_IAaxiomM4_1_Int = prop_IAaxiomM4_1 1

prop_IAaxiomM4_1_Day :: M2set DT.Day -> Property
prop_IAaxiomM4_1_Day = prop_IAaxiomM4_1 1

{-
* Interval Relation property testing 
-}

class ( IntervalSizeable a b ) => IntervalRelationProperties a b where

    prop_IAbefore :: Interval a -> Interval a -> Property
    prop_IAbefore i j =
      IA.before i j ==> (i `meets` k) && (k `meets` j)
        where k = beginerval (diff (begin j) (end i)) (end i)

    prop_IAstarts:: Interval a -> Interval a -> Property
    prop_IAstarts i j
      | IA.starts i j = (j == fromJust (i .+. k)) === True
      | otherwise     = IA.starts i j === False
        where k = beginerval (diff (end j) (end i)) (end i)

    prop_IAfinishes:: Interval a -> Interval a -> Property
    prop_IAfinishes i j
      | IA.finishes i j = (j == fromJust ( k .+. i)) === True
      | otherwise       = IA.finishes i j === False
        where k = beginerval (diff (begin i) (begin j)) (begin j)

    prop_IAoverlaps:: Interval a -> Interval a -> Property
    prop_IAoverlaps i j
      | IA.overlaps i j = ((i == fromJust ( k .+. l )) &&
                          (j == fromJust ( l .+. m ))) === True
      | otherwise       = IA.overlaps i j === False
        where k = beginerval (diff (begin j) (begin i)) (begin i)
              l = beginerval (diff (end i)   (begin j)) (begin j)
              m = beginerval (diff (end j)   (end i))   (end i)

    prop_IAduring:: Interval a -> Interval a-> Property
    prop_IAduring i j
      | IA.during i j = (j == fromJust ( fromJust (k .+. i) .+. l)) === True
      | otherwise     = IA.during i j === False
        where k = beginerval (diff (begin i) (begin j)) (begin j)
              l = beginerval (diff (end j)   (end i))   (end i)

    -- | For any two pair of intervals exactly one 'IntervalRelation' should hold
    prop_exclusiveRelations::  Interval a -> Interval a -> Property
    prop_exclusiveRelations x y =
      (  1 == length (filter id $ map (\r -> r x y) allIArelations)) === True

instance IntervalRelationProperties Int Int

allIArelations:: (Ord a) => [ComparativePredicateOf1 (Interval a)]
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

prop_expandl_end ::(IntervalSizeable a b, Show a)=>
       b
    -> Interval a
    -> Property
prop_expandl_end d i = end (expandl d i) === end i


prop_expandr_begin ::(IntervalSizeable a b, Show a)=>
       b
    -> Interval a
    -> Property
prop_expandr_begin d i = begin (expandr d i) === begin i

-- | The relation between x and z should be an element of the set of the
--   composed relations between x y and between y z.
prop_compose :: Ord a =>
       Interval a
    -> Interval a
    -> Interval a
    -> Property 
prop_compose x y z = member (relate x z) (compose (relate x y) (relate y z)) === True


spec :: Spec
spec = do
  describe "IntervalSizeable tests" $
    do
      it "expandl doesn't change end"   $ property (prop_expandl_end @Int)
      it "expandr doesn't change begin" $ property (prop_expandr_begin @Int)
      it "expand 0 5 Interval (0, 1) should be Interval (0, 6)" $
        expand 0 5 (beginerval (1::Int) (0::Int)) `shouldBe` beginerval (6::Int) (0::Int)
      it "expand 5 0 Interval (0, 1) should be Interval (-5, 1)" $
        expand 5 0 (beginerval (1::Int) (0::Int)) `shouldBe` beginerval (6::Int) (-5::Int)
      it "expand 5 5 Interval (0, 1) should be Interval (-5, 6)" $
        expand 5 5 (beginerval (1::Int) (0::Int)) `shouldBe` beginerval (11::Int) (-5::Int)
      it "expand -1 5 Interval (0, 1) should be Interval (-5, 6)" $
        expand (-1) 5 (beginerval (1::Int) (0::Int)) `shouldBe` beginerval (6::Int) (0::Int) 
      it "expand 5 -5 Interval (0, 1) should be Interval (-5, 1)" $
        expand 5 (-5) (beginerval (1::Int) (0::Int)) `shouldBe` beginerval (6::Int) (-5::Int)
      it "expand moment 0 Interval (0, 1) should be Interval (-1, 1)" $
        expand (moment @Int) 0 (beginerval (1::Int) (0::Int)) `shouldBe`
         beginerval (2::Int) (-1::Int) 

      it "beginerval 2 10 should be Interval (10, 12)" $
        Right (beginerval (2::Int) 10) `shouldBe` parseInterval (10::Int) (12::Int)
      it "beginerval 0 10 should be Interval (10, 11)" $
        Right (beginerval (0::Int) 10) `shouldBe` parseInterval (10::Int) (11::Int)
      it "beginerval -2 10 should be Interval (10, 11)" $
        Right (beginerval (-2::Int) 10) `shouldBe` parseInterval (10::Int) (11::Int)
      it "enderval 2 10 should be Interval (8, 10)" $
        Right (enderval (2::Int) 10) `shouldBe` parseInterval (8::Int) (10::Int)
      it "enderval 0 10 should be Interval (9, 10)" $
        Right (enderval (0::Int) 10) `shouldBe` parseInterval (9::Int) (10::Int)
      it "enderval -2 10 should be Interval (9, 10)" $
        Right (enderval (-2::Int) 10) `shouldBe` parseInterval (9::Int) (10::Int)

  describe "Intervallic tests" $
     modifyMaxSuccess (*10000) $
     do
      it "(startedBy <|> overlappedBy) Interval (0, 9) Interval (-1, 4) is True" $
        (startedBy <|> overlappedBy) (mkIntrvl 9 0) (mkIntrvl 5 (-1))
         `shouldBe` True
      it "(startedBy <|> overlappedBy) Interval (0, 9) Interval (0, 4) is True" $
        (startedBy <|> overlappedBy) (mkIntrvl 9 0) (mkIntrvl 4 0)
         `shouldBe` True
      it "(startedBy <|> overlappedBy) Interval (0, 9) Interval (-1, 9) is False" $
        (startedBy <|> overlappedBy) (mkIntrvl 9 0) (mkIntrvl 10 (-1))
         `shouldBe` False
      it "disjoint x y same as explicit union of predicates" $
         disjoint (mkIntrvl 2 0) (mkIntrvl 2 3) `shouldBe`
         (before <|> after <|> meets <|> metBy) (mkIntrvl 2 0) (mkIntrvl 2 3)
      it "prop_compose holds" $
         property (prop_compose @Int)

  describe "IntervalCombinable tests" $
      do
        it "" pending

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
