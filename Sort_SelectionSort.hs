{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Sort_SelectionSort where

import Test.QuickCheck
import Test.QuickCheck.Poly
import Test.QuickCheck.All
import Data.List( sort, delete )

--------------------------------------------------------------------------------

ssort :: Ord a => [a] -> [a]
ssort [] = []
ssort xs = m : ssort (delete m xs)
 where
  m = minimum xs

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
  ordered (ssort xs)

prop_SortPermutes x (xs :: [OrdA]) =
  count x (ssort xs) == count x xs

prop_SortPermutes' (xs :: [OrdA]) =
  ssort xs `isPermutation` xs

prop_SortIsSort (xs :: [OrdA]) =
  ssort xs == sort xs

--------------------------------------------------------------------------------

return []
testAll = $(quickCheckAll)

--------------------------------------------------------------------------------

