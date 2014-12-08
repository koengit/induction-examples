{-# LANGUAGE ScopedTypeVariables, TemplateHaskell #-}
module Sort_OddEvenSort where

import Test.QuickCheck
import Test.QuickCheck.Poly
import Test.QuickCheck.All
import Data.List( sort, delete )

--------------------------------------------------------------------------------

bsort :: Ord a => [a] -> [a]
bsort []  = []
bsort [x] = [x]
bsort xs  = merge (bsort (evens xs)) (bsort (odds xs))

evens :: [a] -> [a]
evens (x:xs) = x : odds xs
evens []     = []

odds :: [a] -> [a]
odds (x:xs) = evens xs
odds []     = []

merge :: Ord a => [a] -> [a] -> [a]
merge []  bs  = []
merge as  []  = as
merge [a] [b] = sort2 a b
merge as  bs  = stitch cs0 cs1
 where
  as0 = evens as
  as1 = odds as
  bs0 = evens bs
  bs1 = odds bs
  cs0 = merge as0 bs0
  cs1 = merge as1 bs1

stitch :: Ord a => [a] -> [a] -> [a]
stitch []     ys = ys
stitch (x:xs) ys = x : pairs xs ys

pairs :: Ord a => [a] -> [a] -> [a]
pairs []     ys     = ys
pairs xs     []     = xs
pairs (x:xs) (y:ys) = sort2 x y ++ pairs xs ys

sort2 :: Ord a => a -> a -> [a]
sort2 x y | x <= y    = [x,y]
          | otherwise = [y,x]

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

