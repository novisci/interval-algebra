{-# LANGUAGE FlexibleInstances #-}

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import IntervalAlgebra as IA
import Data.Maybe
--import IntervalAlgebra.IntervalCombiner
import Control.Monad

xor :: Bool -> Bool -> Bool
xor a b = a /= b

instance Arbitrary IntrvlPairInt where
  arbitrary = liftM2 safeInterval arbitrary arbitrary

type IntrvlPairInt = Intrvl (Pair Int)

makePos :: Int -> Int
makePos x
  | x == 0    = x + 1
  | x <  0    = negate x
  | otherwise = x


-- | A function for creating intervals when you think you know what you're doing.
interval'' :: Int -> Int -> IntrvlPairInt
interval'' x y = Intrvl $ Pair (min x y, max x y)

-- | Safely create a valid 'IntrvlPairInt' from two Ints by adding 'makepos' @dur@
--   to @start@ to set the duration of the interval.
safeInterval :: Int -> Int -> IntrvlPairInt
safeInterval start dur = interval'' start (start + makePos dur)

-- | Create a 'Maybe IntrvlPairInt' from two Ints.
safeInterval' :: Int -> Int -> Maybe (Intrvl (Pair Int))
safeInterval' a b 
    | b <= a    = Nothing
    | otherwise = Just $ interval'' a b


-- | A set used for testing M1 defined so that the M1 condition is true.
data M1set = M1set { 
     m11 :: IntrvlPairInt
   , m12 :: IntrvlPairInt
   , m13 :: IntrvlPairInt
   , m14 :: IntrvlPairInt }
   deriving (Show)

instance Arbitrary M1set where
  arbitrary = do
    x <- arbitrary
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ m1set x a b c

-- | Smart constructor of 'M1set'.
m1set :: IntrvlPairInt -> Int -> Int -> Int -> M1set
m1set x a b c = M1set p1 p2 p3 p4
  where p1 = x                          -- interval i in prop_IAaxiomM1
        p2 = safeInterval (end x) a     -- interval j in prop_IAaxiomM1
        p3 = safeInterval (end x) b     -- interval k in prop_IAaxiomM1
        p4 = expandl (makePos c) pt     -- interval l in prop_IAaxiomM1
        pt = interval'' (begin p2 - 1) (begin p2)

{-

 ** Axiom M1

 The first axiom of Allen and Hayes (1987) states that if "two periods both
 meet a third, thn any period met by one must also be met by the other." 
 That is:

 \[
   \forall i,j,k,l s.t. (i:j & i:k & l:j) \implies l:k
 \] 
-}
prop_IAaxiomM1 :: M1set -> Property
prop_IAaxiomM1 x = 
  (i `meets` j && i `meets` k && l `meets` j) ==> (l `meets` k)
  where i = m11 x
        j = m12 x
        k = m13 x
        l = m14 x

-- | A set used for testing M2 defined so that the M2 condition is true.
data M2set = M2set {
    m21 :: IntrvlPairInt
  , m22 :: IntrvlPairInt
  , m23 :: IntrvlPairInt
  , m24 :: IntrvlPairInt }
  deriving (Show)

instance Arbitrary M2set where
  arbitrary = do
    x <- arbitrary
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ m2set x a b c

-- | Smart constructor of 'M2set'.
m2set :: IntrvlPairInt -> IntrvlPairInt -> Int -> Int -> M2set
m2set x y a b = M2set p1 p2 p3 p4
  where p1 = x                          -- interval i in prop_IAaxiomM2
        p2 = safeInterval (end x) a     -- interval j in prop_IAaxiomM2
        p3 = y                          -- interval k in prop_IAaxiomM2
        p4 = safeInterval (end y) b     -- interval l in prop_IAaxiomM2

{-

 ** Axiom M2

 The second interval axiom of Allen and Hayes (1987):

 \[
   \forall i,j,k,l s.t. (i:j & k:l) \implies 
     i:l \oplus 
     (\exists m s.t. i:m:l) \oplus
     (\exists m s.t. k:m:j) 
 \] 
-}

prop_IAaxiomM2 :: M2set -> Property
prop_IAaxiomM2 x =
  (i `meets` j && k `meets` l) ==> 
    (i `meets` l)  `xor`  
    (not $ isNothing m) `xor`
    (not $ isNothing n)
    where i = m21 x
          j = m22 x
          k = m23 x
          l = m24 x
          m = safeInterval' (end $ i) (begin $ l)
          n = safeInterval' (end $ k) (begin $ j)

{-

 ** Axiom ML1

 An interval cannot meet itself

 \[
   \forall i \lnot i:i
 \] 
-}

prop_IAaxiomML1 :: IntrvlPairInt -> Property
prop_IAaxiomML1 x = not (x `meets` x) === True

{-

 ** Axiom ML2

 If i meets j then j does not meet i.

 \[
   \forall i,j i:j \implies \lnot j:i
 \] 
-}

prop_IAaxiomML2 :: M2set -> Property
prop_IAaxiomML2 x =
  (i `meets` j) ==> not (j `meets` i)
  where i = m21 x
        j = m22 x


{-

 ** Axiom M3

 Time does not start or stop:

 \[
   \forall i \exists j,k s.t. j:i:k
 \] 
-}

prop_IAaxiomM3 :: IntrvlPairInt -> Property
prop_IAaxiomM3 i = 
   (j `meets` i && i `meets` k) === True
   where j = interval'' (begin i - 1) (begin i)
         k = interval'' (end i) (end i + 1)

{-

** Axiom M4

If two meets are separated by intervals, then this sequence is a longer interval.

 \[
   \forall i,j i:j \implies (\exists k,m,n s.t m:i:j:n & m:k:n) 
 \] 
-}

prop_IAaxiomM4 :: M2set -> Property
prop_IAaxiomM4 x = 
   ((m `meets` i && i `meets` j && j `meets` n) &&
    (m `meets` k && k `meets` n)) === True
   where i = m21 x
         j = m22 x
         m = interval'' (begin i - 1) (begin i)
         n = interval'' (end j) (end j + 1)
         k = interval'' (end m) (begin n)


{-

** Axiom M5

If two meets are separated by intervals, then this sequence is a longer interval.

 \[
   \forall i,j,k,l (i:j:l & i:k:l) \seteq j = k
 \] 
-}

-- | A set used for testing M5.
data M5set = M5set { 
     m51 :: IntrvlPairInt
   , m52 :: IntrvlPairInt }
   deriving (Show)

instance Arbitrary M5set where
  arbitrary = do
    x <- arbitrary
    a <- arbitrary
    b <- arbitrary
    return $ m5set x a b

-- | Smart constructor of 'M5set'.
m5set :: IntrvlPairInt -> Int -> Int -> M5set
m5set x a b = M5set p1 p2 
  where p1 = x                         -- interval i in prop_IAaxiomM5
        p2 = safeInterval ps a         -- interval l in prop_IAaxiomM5
        ps = add (makePos b) (end x) -- creating l by shifting and expanding i


prop_IAaxiomM5 :: M5set -> Property
prop_IAaxiomM5 x = 
   ((i `meets` j && j `meets` l) &&
    (i `meets` k && k `meets` l))  === (j == k)
   where i = m51 x
         j = interval'' (end i) (begin l)
         k = interval'' (end i) (begin l)
         l = m52 x

{-

** Axiom M4.1

Ordered unions

 \[
   \forall i,j i:j \implies (\exists m,n s.t. m:i:j:n & m:(i+j):n)
 \] 
-}

prop_IAaxiomM4_1 :: M2set -> Property
prop_IAaxiomM4_1 x = 
   ((m `meets` i && i `meets` j && j `meets` n) &&
    (m `meets` ij && ij `meets` n)) === True
   where i = m21 x
         j = m22 x
         m = interval'' (begin i - 1) (begin i)
         n = interval'' (end j) (end j + 1)
         ij = fromJust $ i .+. j


{-
* Interval Relation property testing 
-}

prop_IAbefore :: IntrvlPairInt -> IntrvlPairInt -> Property
prop_IAbefore i j = 
  IA.before i j ==> (i `meets` k) && (k `meets` j)
    where k = interval'' (end i) (begin j)


prop_IAstarts:: IntrvlPairInt -> IntrvlPairInt -> Property
prop_IAstarts i j
  | ((IA.starts i j) == True) =
    let k = interval'' (end i) (end j)
    in 
     (j == (fromJust $ i .+. k)) === True
  | otherwise = IA.starts i j === False


prop_IAfinishes:: IntrvlPairInt -> IntrvlPairInt -> Property
prop_IAfinishes i j
  | ((IA.finishes i j) == True) =
    let k = interval'' (begin j) (begin i)
    in 
     (j == (fromJust $ k .+. i)) === True
  | otherwise = IA.finishes i j === False

prop_IAoverlaps:: IntrvlPairInt -> IntrvlPairInt -> Property
prop_IAoverlaps i j
  | ((IA.overlaps i j) == True) = 
    let k = interval'' (begin i) (begin j)
        l = interval'' (begin j) (end i)
        m = interval'' (end i)   (end j)
    in 
     ((i == (fromJust $ k .+. l )) &&
      (j == (fromJust $ l .+. m ))) === True
  | otherwise  = IA.overlaps i j === False 

prop_IAduring:: IntrvlPairInt -> IntrvlPairInt -> Property
prop_IAduring i j
  | ((IA.during i j) == True) = 
    let k = interval'' (begin j) (begin i)
        l = interval'' (end i) (end j)
    in 
     (j == (fromJust $ (fromJust $ k .+. i) .+. l)) === True
  | otherwise  = IA.during i j === False 

{-
For any two pair of intervals exactly one 'IntervalRelation' should hold.
-}

allIArelations:: [(ComparativePredicateOf (IntrvlPairInt))]
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

prop_exclusiveRelations::  IntrvlPairInt -> IntrvlPairInt -> Property 
prop_exclusiveRelations x y =
  (foldl1 (xor) $ map (\r -> r x y) allIArelations) === True

main :: IO ()
main = hspec $ do
  describe "Interval Algebra Axioms for meets property" $ --modifyMaxDiscardRatio (* 10) $
    do 
      {- 
      if two periods both meet a third, 
      then any period met by one must also be met by the other.
      -}
      it "M1" $ property prop_IAaxiomM1

      {- 
        if period i meets period j and period k meets l, 
        then exactly one of the following holds:
          1) i meets l; 
          2) there is an m such that i meets m and m meets l; 
          3) there is an n such that k meets n and n meets j.
      -}       
      it "M2" $ property prop_IAaxiomM2

      {-
        a period cannot meet itself
      -}
      it "ML1" $ property prop_IAaxiomML1

      {-
        if i meets j then j does not meet i
      -}
      it "ML2" $ property prop_IAaxiomML2

      {-
        For all i, there does not exist m such that i meets m and m meet i
        Not testing that this axiom holds, as I'm not sure how I would
      -}
      --it "ML3" $ property prop_IAaxiomML3

      {-
        for all periods i there exist periods j and k such that j:i:k
      -}
      it "M3" $ property prop_IAaxiomM3


      {-
        if i meets j then there exists k, m, n such that m:i:j:n and m:k:n
      -}
      it "M4" $ property prop_IAaxiomM4

      
      {-
       i:j:l & i:k:l === j = k
      -}
      it "M5" $ property prop_IAaxiomM5 
      
      {-
        if i meets j then there exists k, m, n such that m:i:j:n and m:k:n
      -}
      it "M4.1" $ property prop_IAaxiomM4_1
 

  describe "Interval Algebra relation properties" $ 
      modifyMaxSuccess (*10) $
      --modifyMaxDiscardRatio (* 10) $
    do
      it "before"   $ property prop_IAbefore
      it "starts"   $ property prop_IAstarts
      it "finishes" $ property prop_IAfinishes
      it "overlaps" $ property prop_IAoverlaps
      it "during"   $ property prop_IAduring

  describe "Interval Algebra relation uniqueness" $ 
      modifyMaxSuccess (*100) $
      --modifyMaxDiscardRatio (* 10) $
    do
      it "exactly one relation must be true" $ property prop_exclusiveRelations
 {- 
  describe "Period expansions" $ 
    do
      it "expandl safely shrinks a period" $
        expandl (-10) (period 0 10) `shouldBe` period 0 10
      it "expandr safely shrinks a period" $
        expandr (-10) (period 0 10) `shouldBe` period 0 10
-}
