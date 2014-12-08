{-# LANGUAGE TemplateHaskell #-}
module Propositional where

import Test.QuickCheck
import Test.QuickCheck.Poly
import Test.QuickCheck.All
import Control.Monad ( liftM, liftM2 )
import Data.List ( insert )

--------------------------------------------------------------------------------

type Name = Char -- can be whatever

data Form
  = Form :&: Form
  | Not Form
  | Var Name
 deriving ( Eq, Ord, Show )

type Val = [(Name,Bool)]

models :: Form -> Val -> [Val]
models (p :&: q) m =
  [ m2
  | m1 <- models p m
  , m2 <- models q m1
  ]

models (Not (p :&: q)) m =
  models (Not p) m ++ models (p :&: Not q) m

models (Not (Not p)) m =
  models p m

models (Var x) m =
  [ (x,True) `insert` m 
  | (x,False) `notElem` m
  ]

models (Not (Var x)) m =
  [ (x,False) `insert` m 
  | (x,True) `notElem` m
  ]

valid :: Form -> Bool
valid p = null (models (Not p) [])

--------------------------------------------------------------------------------

prop_AndCommutative p q =
  valid (p :&: q) == valid (q :&: p)

prop_AndIdempotent p =
  valid (p :&: p) == valid p

prop_AndImplication p q =
  valid (p :&: q) ==> valid q

--------------------------------------------------------------------------------

(|=) :: Val -> Form -> Bool
m |= Var x     = (x,True) `elem` m
m |= Not p     = not (m |= p)
m |= (p :&: q) = m |= p && m |= q

prop_Sound p =
  all (|= p) (models p [])

--------------------------------------------------------------------------------

instance Arbitrary Form where
  arbitrary = sized arbForm
   where
    arbForm n = frequency
                [ (n, liftM2 (:&:) (arbForm n2) (arbForm n2))
                , (n, liftM  Not   (arbForm n1))
                , (1, liftM  Var   (growingElements ['a'..'e']))
                ]
     where
      n1 = n-1
      n2 = n `div` 2

  shrink (p :&: q) = [p,q] ++ [p' :&: q | p' <- shrink p] ++ [p :&: q' | q' <- shrink q]
  shrink (Not p)   = [p]   ++ [p' | Not p' <- shrink p]   ++ [Not p' | p' <- shrink p]
  shrink (Var x)   = [Var x' | x' <- shrink x]

return []
testAll = $(quickCheckAll)

--------------------------------------------------------------------------------

