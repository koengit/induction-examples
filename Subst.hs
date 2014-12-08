{-# LANGUAGE TemplateHaskell #-}
module Subst where

import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad ( liftM, liftM2 )

--------------------------------------------------------------------------------

type Name = Integer

data Expr
  = Var Name
  | Lam Name Expr
  | App Expr Expr
 deriving ( Eq, Ord, Show )

free :: Expr -> [Name]
free (Var x)   = [x]
free (App a b) = free a ++ free b
free (Lam x a) = filter (x/=) (free a)

new :: [Name] -> Name
new xs = maximum (0:xs) + 1

subst :: Name -> Expr -> Expr -> Expr
subst x e (Var y)
  | x == y          = e
  | otherwise       = Var y
subst x e (App a b) = App (subst x e a) (subst x e b)
subst x e (Lam y a)
  | x == y          = Lam y a
  | y `elem` free e = subst x e (Lam z (subst y (Var z) a))
  | otherwise       = Lam y (subst x e a)
 where
  z = new (free e ++ free a)

--------------------------------------------------------------------------------

prop_SubstFreeNo x e a y =
  x `notElem` free a ==>
    (y `elem` free a) == (y `elem` free (subst x e a))

prop_SubstFreeYes x e a y =
  x `elem` free a ==>
    (y `elem` (filter (/=x) (free a) ++ free e)) == (y `elem` free (subst x e a))

--------------------------------------------------------------------------------

instance Arbitrary Expr where
  arbitrary = sized arbExpr
   where
    arbExpr n = frequency
                [ (n, liftM2 App (arbExpr n2) (arbExpr n2))
                , (n, liftM2 Lam arbVar (arbExpr n1))
                , (1, liftM  Var arbVar)
                ]
     where
      n1 = n-1
      n2 = n `div` 2

    arbVar = growingElements [1..10]

  shrink (Var x)   = [Var x' | x' <- shrink x]
  shrink (Lam x a) = [a] ++ [Lam x a' | a' <- shrink a] ++ [Lam x' a | x' <- shrink x] 
  shrink (App a b) = [a,b] ++ [App a' b | a' <- shrink a] ++ [App a b' | b' <- shrink b]

return []
testAll = $(quickCheckAll)

--------------------------------------------------------------------------------

