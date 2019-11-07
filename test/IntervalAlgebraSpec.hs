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

instance Arbitrary (Intrvl Int) where
  arbitrary = liftM2 safeInterval arbitrary arbitrary

makePos :: Int -> Int
makePos x
  | x == 0    = x + 1
  | x <  0    = negate x
  | otherwise = x


-- | Safely create a valid 'Intrvl Int' from two Ints. 
safeInterval :: Int -> Int -> Intrvl Int
safeInterval b dur = interval' b (makePos dur)

-- | Create a 'Maybe Intrvl Int' from two Ints.
safeInterval' :: Int -> Int -> Maybe (Intrvl Int)
safeInterval' a b 
    | b <= a    = Nothing
    | otherwise = Just $ validInterval a b


-- | A set used for testing M1 defined so that the M1 condition is true.
data M1set = M1set { 
     m11 :: Intrvl Int
   , m12 :: Intrvl Int
   , m13 :: Intrvl Int
   , m14 :: Intrvl Int }
   deriving (Show)

instance Arbitrary M1set where
  arbitrary = do
    x <- arbitrary
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ m1set x a b c

-- | Smart constructor of 'M1set'.
m1set :: Intrvl Int -> Int -> Int -> Int -> M1set
m1set x a b c = M1set p1 p2 p3 p4
  where p1 = x                          -- interval i in prop_IAaxiomM1
        p2 = safeInterval (end x) a     -- interval j in prop_IAaxiomM1
        p3 = safeInterval (end x) b     -- interval k in prop_IAaxiomM1
        p4 = expandl (makePos c) pt     -- interval l in prop_IAaxiomM1
        pt = validInterval (begin p2 - 1) (begin p2)

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
    m21 :: Intrvl Int
  , m22 :: Intrvl Int
  , m23 :: Intrvl Int
  , m24 :: Intrvl Int }
  deriving (Show)

instance Arbitrary M2set where
  arbitrary = do
    x <- arbitrary
    a <- arbitrary
    b <- arbitrary
    c <- arbitrary
    return $ m2set x a b c

-- | Smart constructor of 'M2set'.
m2set :: Intrvl Int -> Intrvl Int -> Int -> Int -> M2set
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

prop_IAaxiomML1 :: Intrvl Int -> Property
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

prop_IAaxiomM3 :: Intrvl Int -> Property
prop_IAaxiomM3 i = 
   (j `meets` i && i `meets` k) === True
   where j = validInterval (begin i - 1) (begin i)
         k = validInterval (end i) (end i + 1)

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
         m = validInterval (begin i - 1) (begin i)
         n = validInterval (end j) (end j + 1)
         k = validInterval (end m) (begin n)


{-

** Axiom M5

If two meets are separated by intervals, then this sequence is a longer interval.

 \[
   \forall i,j,k,l (i:j:l & i:k:l) \seteq j = l
 \] 
-}

-- TODO

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
         m = validInterval (begin i - 1) (begin i)
         n = validInterval (end j) (end j + 1)
         ij = fromJust $ i .+. j


{-
* Interval Relation property testing 
-}

prop_IAbefore :: Intrvl Int -> Intrvl Int -> Property
prop_IAbefore i j = 
  IA.before i j ==> (i `meets` k) && (k `meets` j)
    where k = validInterval (end i) (begin j)


prop_IAstarts:: Intrvl Int -> Intrvl Int -> Property
prop_IAstarts i j
  | ((IA.starts i j) == True) =
    let k = validInterval (end i) (end j)
    in 
     (j == (fromJust $ i .+. k)) === True
  | otherwise = IA.starts i j === False


prop_IAfinishes:: Intrvl Int -> Intrvl Int -> Property
prop_IAfinishes i j
  | ((IA.finishes i j) == True) =
    let k = validInterval (begin j) (begin i)
    in 
     (j == (fromJust $ k .+. i)) === True
  | otherwise = IA.finishes i j === False

prop_IAoverlaps:: Intrvl Int -> Intrvl Int -> Property
prop_IAoverlaps i j
  | ((IA.overlaps i j) == True) = 
    let k = validInterval (begin i) (begin j)
        l = validInterval (begin j) (end i)
        m = validInterval (end i)   (end j)
    in 
     ((i == (fromJust $ k .+. l )) &&
      (j == (fromJust $ l .+. m ))) === True
  | otherwise  = IA.overlaps i j === False 

prop_IAduring:: Intrvl Int -> Intrvl Int -> Property
prop_IAduring i j
  | ((IA.during i j) == True) = 
    let k = validInterval (begin j) (begin i)
        l = validInterval (end i) (end j)
    in 
     (j == (fromJust $ (fromJust $ k .+. i) .+. l)) === True
  | otherwise  = IA.during i j === False 


main :: IO ()
main = hspec $ do
  describe "Interval Algebra Axioms" $ --modifyMaxDiscardRatio (* 10) $
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
      -}
      it "M5" $ pending 
      
      {-
        if i meets j then there exists k, m, n such that m:i:j:n and m:k:n
      -}
      it "M4.1" $ property prop_IAaxiomM4_1
 

  describe "Interval Algebra relations" $ 
      modifyMaxSuccess (*10) $
      --modifyMaxDiscardRatio (* 10) $
  -- https://en.wikipedia.org/wiki/Allen%27s_interval_algebra#Relations 
  -- contains a visual of these relations
   do
      it "before"   $ property prop_IAbefore
      it "starts"   $ property prop_IAstarts
      it "finishes" $ property prop_IAfinishes
      it "overlaps" $ property prop_IAoverlaps
      it "during"   $ property prop_IAduring
 {- 
  describe "Period expansions" $ 
    do
      it "expandl safely shrinks a period" $
        expandl (-10) (period 0 10) `shouldBe` period 0 10
      it "expandr safely shrinks a period" $
        expandr (-10) (period 0 10) `shouldBe` period 0 10
-}
{- Examples of unit testing
  describe "after" $ do
    it "return False for a period before another" $
      IA.before (period 0 1) (period 2 3) `shouldBe` True
    it "return True for a period after another" $
      IA.before (period 2 3) (period 0 1) `shouldBe` False
    it "return False for a period meeting another" $
      IA.before (period 0 1) (period 1 2) `shouldBe` False}
-}