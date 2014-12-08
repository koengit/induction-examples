{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Sort_QuickSort where

import Test.QuickCheck
import Test.QuickCheck.Poly
import Test.QuickCheck.All
import Data.List( sort, delete )

--------------------------------------------------------------------------------

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort (filter (<=x) xs) ++ [x] ++ qsort (filter (>x) xs)

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
  ordered (qsort xs)

prop_SortPermutes x (xs :: [OrdA]) =
  count x (qsort xs) == count x xs

prop_SortPermutes' (xs :: [OrdA]) =
  qsort xs `isPermutation` xs

prop_SortIsSort (xs :: [OrdA]) =
  qsort xs == sort xs

--------------------------------------------------------------------------------

return []
testAll = $(quickCheckAll)

--------------------------------------------------------------------------------

