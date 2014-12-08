{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Sort_InsertionSort where

import Test.QuickCheck
import Test.QuickCheck.Poly
import Test.QuickCheck.All
import Data.List( sort, delete )

--------------------------------------------------------------------------------

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

insert :: Ord a => a -> [a] -> [a]
insert x []                 = [x]
insert x (y:xs) | x <= y    = x : y : xs
                | otherwise = y : insert x xs

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
  ordered (isort xs)

prop_SortPermutes x (xs :: [OrdA]) =
  count x (isort xs) == count x xs

prop_SortPermutes' (xs :: [OrdA]) =
  isort xs `isPermutation` xs

prop_SortIsSort (xs :: [OrdA]) =
  isort xs == sort xs

--------------------------------------------------------------------------------

return []
testAll = $(quickCheckAll)

--------------------------------------------------------------------------------

