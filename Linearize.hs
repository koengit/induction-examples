{-# LANGUAGE TemplateHaskell #-}
module Linearize where

import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad ( liftM, liftM2 )

--------------------------------------------------------------------------------

data Expr = Expr :+: Expr | Expr :*: Expr | Var Char
 deriving ( Eq, Ord, Show )
 
--------------------------------------------------------------------------------

lin0 :: Expr -> String
lin0 (a :+: b) = "(" ++ lin0 a ++ "+" ++ lin0 b ++ ")"
lin0 (a :*: b) = "(" ++ lin0 a ++ "*" ++ lin0 b ++ ")"
lin0 (Var x)   = [x]

lin1 :: Expr -> String
lin1 (a :+: b) = "(" ++ lin1 a ++ "+" ++ lin1 b ++ ")"
lin1 (a :*: b) = lin1 a ++ "*" ++ lin1' b
lin1 (Var x)   = [x]

lin1' :: Expr -> String
lin1' a | isAtom a  = lin1 a
        | otherwise = "(" ++ lin1 a ++ ")"

isAtom :: Expr -> Bool
isAtom (a :*: b) = False
isAtom _         = True

--------------------------------------------------------------------------------

prop_InjectiveLin0 a b =
  lin0 a == lin0 b ==> a == b

prop_InjectiveLin1 a b =
  lin1 a == lin1 b ==> a == b

-- the below are helpful in proofs, but they really should be found automatically

prop_InjectiveAppend s s1 s2 =
  s1 ++ s == s2 ++ s ==> s1 == s2

prop_InjectiveLin0Append a b s1 s2 =
  lin0 a ++ s1 == lin0 b ++ s2 ==> a == b

prop_InjectiveLin1Append a b s1 s2 =
  lin1 a ++ s1 == lin1 b ++ s2 ==> a == b

--------------------------------------------------------------------------------

instance Arbitrary Expr where
  arbitrary = sized arbExpr
   where
    arbExpr n = frequency
                [ (n, liftM2 (:+:) (arbExpr n2) (arbExpr n2))
                , (1, liftM  Var   arbitrary)
                ]
     where
      n2 = n `div` 2

  shrink (a :+: b) = [a,b] ++ [a' :+: b | a' <- shrink a]
                           ++ [a :+: b' | b' <- shrink b]
  shrink (Var x)   = [Var x' | x' <- shrink x]

return []
testAll = $(quickCheckAll)

--------------------------------------------------------------------------------

