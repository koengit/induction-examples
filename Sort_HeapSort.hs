{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Sort_QuickSort where

import Test.QuickCheck
import Test.QuickCheck.Poly
import Test.QuickCheck.All
import Data.List( sort, delete )

--------------------------------------------------------------------------------

hsort :: Ord a => [a] -> [a]
hsort = toList . toHeap

data Heap a = Node (Heap a) a (Heap a) | Nil
 deriving ( Eq, Ord, Show )

merge :: Ord a => Heap a -> Heap a -> Heap a
Nil        `merge` q          = q
p          `merge` Nil        = p
Node p x q `merge` Node r y s
  | x <= y                    = Node (q `merge` Node r y s) x p
  | otherwise                 = Node (Node p x q `merge` s) y r

toHeap :: Ord a => [a] -> Heap a
toHeap xs = merging [ Node Nil x Nil | x <- xs ]

merging :: Ord a => [Heap a] -> Heap a
merging []  = Nil
merging [p] = p
merging ps  = merging (pairwise ps)

pairwise :: Ord a => [Heap a] -> [Heap a]
pairwise (p:q:qs) = (p `merge` q) : pairwise qs
pairwise ps       = ps

toList :: Ord a => Heap a -> [a]
toList Nil          = []
toList (Node p x q) = x : toList (p `merge` q)

--------------------------------------------------------------------------------

ordered :: Ord a => [a] -> Bool
ordered []       = True
ordered [x]      = True
ordered (x:y:xs) = x <= y && ordered (y:xs)

count :: Eq a => a -> [a] -> Integer
count x []                 = 0
count x (y:xs) | x == y    = 1 + count x xs
               | otherwise = count x xs

isPermutation :: Eq a => [a] -> [a] -> Bool
[]     `isPermutation` ys = null ys
(x:xs) `isPermutation` ys = x `elem` ys && xs `isPermutation` delete x ys

--------------------------------------------------------------------------------

prop_SortSorts (xs :: [OrdA]) =
  ordered (hsort xs)

prop_SortPermutes x (xs :: [OrdA]) =
  count x (hsort xs) == count x xs

prop_SortPermutes' (xs :: [OrdA]) =
  hsort xs `isPermutation` xs

prop_SortIsSort (xs :: [OrdA]) =
  hsort xs == sort xs

--------------------------------------------------------------------------------

return []
testAll = $(quickCheckAll)

--------------------------------------------------------------------------------

