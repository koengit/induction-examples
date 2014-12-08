{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Sort_MergeSortBottomUp2 where

import Test.QuickCheck
import Test.QuickCheck.Poly
import Test.QuickCheck.All
import Data.List( sort, delete )

--------------------------------------------------------------------------------

msort :: Ord a => [a] -> [a]
msort = merging . risers

{-
risers :: Ord a => [a] -> [[a]]
risers []     = []
risers [x]    = [[x]]
risers (x:xs) = case risers xs of
                  (y:ys):yss | x <= y -> (x:y:ys):yss
                  yss                 -> [x]:yss
-}

risers :: Ord a => [a] -> [[a]]
risers []       = []
risers [x]      = [[x]]
risers (x:y:xs)
  | x <= y      = case risers (y:xs) of
                    ys:yss -> (x:ys):yss
  | otherwise   = [x] : risers (y:xs)

merging :: Ord a => [[a]] -> [a]
merging []   = []
merging [xs] = xs
merging xss  = merging (pairwise xss)

pairwise (xs:ys:xss) = xs `merge` ys : pairwise xss
pairwise xss         = xss

merge :: Ord a => [a] -> [a] -> [a]
[]     `merge` ys = ys
xs     `merge` [] = xs
(x:xs) `merge` (y:ys)
  | x <= y        = x : xs `merge` (y:ys)
  | otherwise     = y : (x:xs) `merge` ys

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
  ordered (msort xs)

prop_SortPermutes x (xs :: [OrdA]) =
  count x (msort xs) == count x xs

prop_SortPermutes' (xs :: [OrdA]) =
  msort xs `isPermutation` xs

prop_SortIsSort (xs :: [OrdA]) =
  msort xs == sort xs

--------------------------------------------------------------------------------

return []
testAll = $(quickCheckAll)

--------------------------------------------------------------------------------

