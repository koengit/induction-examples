{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module List where

import Test.QuickCheck
import Test.QuickCheck.Poly
import Test.QuickCheck.All
import Data.List ( delete )

--------------------------------------------------------------------------------

select :: [a] -> [(a,[a])]
select []     = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

pairs :: [a] -> [(a,a)]
pairs (x:y:xs) = (x,y) : pairs xs
pairs _        = []

unpair :: [(a,a)] -> [a]
unpair []          = []
unpair ((x,y):xys) = x : y : unpair xys

evens :: [a] -> [a]
evens (x:xs) = x : odds xs
evens []     = []

odds :: [a] -> [a]
odds (x:xs) = evens xs
odds []     = []

interleave :: [a] -> [a] -> [a]
interleave (x:xs) ys = x : interleave ys xs
interleave []     ys = ys

--------------------------------------------------------------------------------

count :: Eq a => a -> [a] -> Integer
count x []                 = 0
count x (y:xs) | x == y    = 1 + count x xs
               | otherwise = count x xs

isPermutation :: Eq a => [a] -> [a] -> Bool
[]     `isPermutation` ys = null ys
(x:xs) `isPermutation` ys = x `elem` ys && xs `isPermutation` delete x ys

--------------------------------------------------------------------------------

prop_Select (xs :: [A]) =
  map fst (select xs) == xs

prop_SelectPermutations (xs :: [A]) =
  all (`isPermutation` xs) [ y:ys | (y,ys) <- select xs ]

prop_SelectPermutations' (xs :: [A]) z =
  all ((n==) . count z) [ y:ys | (y,ys) <- select xs ]
 where
  n = count z xs

prop_PairUnpair (xs :: [A]) =
  even (length xs) ==>
    unpair (pairs xs) == xs

prop_PairEvens (xs :: [A]) =
  even (length xs) ==>
    map fst (pairs xs) == evens xs

prop_PairOdds (xs :: [A]) =
--  even (length xs) ==>
    map snd (pairs xs) == odds xs

prop_Interleave (xs :: [A]) =
  interleave (evens xs) (odds xs) == xs

--------------------------------------------------------------------------------

return []
testAll = $(quickCheckAll)

--------------------------------------------------------------------------------

