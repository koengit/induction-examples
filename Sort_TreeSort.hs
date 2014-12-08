{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Sort_QuickSort where

import Test.QuickCheck
import Test.QuickCheck.Poly
import Test.QuickCheck.All
import Data.List( sort, delete )

--------------------------------------------------------------------------------

tsort :: Ord a => [a] -> [a]
tsort = ($[]) . flatten . toTree

data Tree a = Node (Tree a) a (Tree a) | Nil
 deriving ( Eq, Ord, Show )

toTree :: Ord a => [a] -> Tree a
toTree []     = Nil
toTree (x:xs) = add x (toTree xs)

add :: Ord a => a -> Tree a -> Tree a
add x Nil                      = Node Nil x Nil
add x (Node p y q) | x <= y    = Node (add x p) y q
                   | otherwise = Node p y (add x q)

flatten :: Tree a -> [a] -> [a]
flatten Nil          ys = ys
flatten (Node p x q) ys = flatten p (x : flatten q ys)

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
  ordered (tsort xs)

prop_SortPermutes x (xs :: [OrdA]) =
  count x (tsort xs) == count x xs

prop_SortPermutes' (xs :: [OrdA]) =
  tsort xs `isPermutation` xs

prop_SortIsSort (xs :: [OrdA]) =
  tsort xs == sort xs

--------------------------------------------------------------------------------

return []
testAll = $(quickCheckAll)

--------------------------------------------------------------------------------

