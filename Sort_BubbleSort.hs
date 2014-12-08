{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Sort_BubbleSort where

import Test.QuickCheck
import Test.QuickCheck.Poly
import Test.QuickCheck.All
import Data.List( sort, delete )

--------------------------------------------------------------------------------

bsort :: Ord a => [a] -> [a]
bsort xs | b         = bsort ys
         | otherwise = xs
 where
  (b,ys) = bubble xs

bubble :: Ord a => [a] -> (Bool,[a])
bubble (x:y:xs) = (not c||b, x':ys)
 where
  c      = x <= y
  x'     = if c then x else y
  y'     = if c then y else x
  (b,ys) = bubble (y':xs)
bubble xs       = (False,xs)

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
  ordered (bsort xs)

prop_SortPermutes x (xs :: [OrdA]) =
  count x (bsort xs) == count x xs

prop_SortPermutes' (xs :: [OrdA]) =
  bsort xs `isPermutation` xs

prop_SortIsSort (xs :: [OrdA]) =
  bsort xs == sort xs

--------------------------------------------------------------------------------

return []
testAll = $(quickCheckAll)

--------------------------------------------------------------------------------

