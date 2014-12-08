{-# LANGUAGE TemplateHaskell #-}
module Escape where

import Test.QuickCheck
import Test.QuickCheck.All

--------------------------------------------------------------------------------

data Token = A | B | C | D | ESC | P | Q | R
 deriving ( Eq, Ord, Show )

escape :: [Token] -> [Token]
escape []                   = []
escape (x:xs) | isSpecial x = ESC : code x : escape xs
              | otherwise   = x : escape xs

isSpecial :: Token -> Bool
isSpecial ESC = True
isSpecial P   = True
isSpecial Q   = True
isSpecial R   = True
isSpecial _   = False

code :: Token -> Token
code ESC = ESC
code P   = A
code Q   = B
code R   = C

--------------------------------------------------------------------------------

prop_Injective xs ys =
  escape xs == escape ys ==> xs == ys

prop_NoSpecial xs =
  all (\x -> not (isSpecial x) || x == ESC) (escape xs)

--------------------------------------------------------------------------------

instance Arbitrary Token where
  arbitrary = elements [A,B,C,D,ESC,P,Q,R]

return []
testAll = $(quickCheckAll)

--------------------------------------------------------------------------------

