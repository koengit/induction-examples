{-# LANGUAGE TemplateHaskell #-}
module RegExp where

import Test.QuickCheck
import Test.QuickCheck.All
import Control.Monad ( liftM, liftM2 )

--------------------------------------------------------------------------------

data R a
  = Nil
  | Eps
  | Atom a
  | R a :+: R a
  | R a :>: R a
  | Star (R a)
 deriving ( Eq, Show )

(.+.), (.>.) :: R a -> R a -> R a
Nil .+. q   = q
p   .+. Nil = p
p   .+. q   = p :+: q

Nil .>. q   = Nil
p   .>. Nil = Nil
Eps .>. q   = q
p   .>. Eps = p
p   .>. q   = p :>: q

eps :: R a -> Bool
eps Eps       = True
eps (p :+: q) = eps p || eps q
eps (p :>: q) = eps p && eps q
eps (Star _)  = True
eps _         = False

epsR :: R a -> R a
epsR p | eps p     = Eps
       | otherwise = Nil

step :: Eq a => R a -> a -> R a
step (Atom a)  x | a == x = Eps
step (p :+: q) x          = step p x .+. step q x
step (p :>: q) x          = (step p x .>. q) .+. (epsR p .>. step q x)
step (Star p)  x          = step p x .>. Star p
step _         x          = Nil

rec :: Eq a => R a -> [a] -> Bool
rec p []     = eps p
rec p (x:xs) = rec (step p x) xs

--------------------------------------------------------------------------------

prop_PlusIdempotent p s =
  rec (p :+: p) s == rec p s

prop_PlusCommutative p q s =
  rec (p :+: q) s == rec (q :+: p) s

prop_PlusAssociative p q r s =
  rec (p :+: (q :+: r)) s == rec ((p :+: q) :+: r) s

prop_SeqAssociative p q r s =
  rec (p :>: (q :>: r)) s == rec ((p :>: q) :>: r) s

prop_SeqDistrPlus p q r s =
  rec (p :>: (q :+: r)) s == rec ((p :>: q) :+: (p :>: r)) s

prop_Star p s =
  rec (Star p) s == rec (Eps :+: (p :>: Star p)) s

--------------------------------------------------------------------------------

prop_RecAtom a s =
  rec (Atom a) s == (s == [a])

prop_RecEps s =
  rec Eps s == null s

prop_RecNil s =
  rec Nil s == False

prop_RecPlus p q s =
  rec (p :+: q) s == rec p s || rec q s

prop_RecSeq p q s =
  rec (p :>: q) s == or [ rec p s1 && rec q s2 | (s1,s2) <- split s ]

split []    = [([],[])]
split (x:s) = ([],x:s) : [ (x:s1,s2) | (s1,s2) <- split s ]

prop_RecStar p s =
  rec (Star p) s == null s || rec (p :>: Star p) s

--------------------------------------------------------------------------------

instance Arbitrary a => Arbitrary (R a) where
  arbitrary = sized arbR
   where
    arbR n = frequency
             [ (n, liftM2 (:+:) (arbR n2) (arbR n2))
             , (n, liftM2 (:>:) (arbR n2) (arbR n2))
             , (n, liftM  Star  (arbR n2))
             , (1, return Nil)
             , (1, return Eps)
             , (1, liftM  Atom arbitrary)
             ]
     where
      n2 = n `div` 2

  shrink (p :+: q) = [p,q] ++ [p' :+: q | p' <- shrink p] ++ [p :+: q' | q' <- shrink q]
  shrink (p :>: q) = [p,q] ++ [p' :>: q | p' <- shrink p] ++ [p :>: q' | q' <- shrink q]
  shrink (Star p)  = [Eps,p] ++ [Star p' | p' <- shrink p]
  shrink Nil       = []
  shrink Eps       = [Nil]
  shrink (Atom a)  = [Eps,Nil] ++ [Atom a' | a' <- shrink a]

return []
testAll = $(quickCheckAll)

--------------------------------------------------------------------------------

