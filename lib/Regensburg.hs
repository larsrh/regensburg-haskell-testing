module Regensburg where

import Data.List (sort)
import Test.QuickCheck
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Gen

-- Eigenschaften für Typklassen

class Semigroup s where
  append :: s -> s -> s

prop_valid_semigroup xs ys zs =
  (xs `append` ys) `append` zs == xs `append` (ys `append` zs)

instance Semigroup [a] where
  append [] ys = ys
  append (x:xs) ys = x : append xs ys

-- Sortiertheit

sorted :: Ord a => [a] -> Bool
sorted [] = True
sorted [_] = True
sorted (x:y:xs) = x <= y && sorted (y:xs)

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x <= y = x:y:ys
                | otherwise = y : insert x ys

insertionSort :: Ord a => [a] -> [a]
insertionSort xs = foldr insert [] xs

-- eigene Generatoren

gen_ordered :: Gen [Int]
gen_ordered = do
  n <- choose (0 :: Int, 10)
  start <- choose (-10, 10)
  asc n start
  where asc 0 _ = return []
        asc n a = do
          a' <- choose (a, a + 10)
          tail <- asc (n-1) a'
          return (a' : tail)

-- eigene Instanzen von `Arbitrary`

newtype OrderedIntList = OrderedIntList [Int] deriving (Show, Eq)

instance Arbitrary OrderedIntList where
  arbitrary = do
    xs <- arbitrary
    return (OrderedIntList (sort xs))

-- mehr Eigenschaften

prop_insert_preserve_sorted a (OrderedIntList xs) = 
  sorted (insert (a::Int) xs)

prop_insert_does_insert a xs =
  a `elem` insert a xs

prop_insertionSort_sorted xs =
  sorted (insertionSort xs)

-- eigene Instanzen von `Arbitrary` für rekursive Datentypen

data BinTree a = Node (BinTree a) (BinTree a) | Leaf a deriving (Show, Eq)

instance Arbitrary a => Arbitrary (BinTree a) where
  arbitrary = sized $ \n ->
    if n <= 1 then
      leaf
    else
      resize (n `div` 2) (oneof [node, leaf])
    where node = do t1 <- arbitrary
                    t2 <- arbitrary
                    return (Node t1 t2)
          leaf = do x <- arbitrary
                    return (Leaf x)
