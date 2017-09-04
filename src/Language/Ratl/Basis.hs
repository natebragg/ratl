module Language.Ratl.Basis (
    arity,
    basis,
) where

import Language.Ratl.Ty (
    Ty(..),
    FunTy(..),
    )
import Language.Ratl.Ast (
    Embeddable(..),
    Nat(..),
    List(..),
    Val(..),
    Var(..),
    Fun(..),
    )

import Prelude hiding (head, tail)

ifte :: [Val] -> Val
ifte [p, t, f] = undefined

plus :: [Val] -> Val
plus [Nat n, Nat m] = Nat $ embed (project n + project m)

head :: [Val] -> Val
head [List (Cons x _)] = x

tail :: [Val] -> Val
tail [List (Cons _ xs)] = List xs

arity :: Var -> Int
arity x = maybe 1 (\(Native _ a _) -> a) $ lookup x basis

basis :: [(Var, Fun ())]
basis = [
    (V "if",   Native (Arrow () [MysteryTy, MysteryTy, MysteryTy] MysteryTy) 3 ifte),
    (V "+",    Native (Arrow () [NatTy, NatTy] NatTy)                        2 plus),
    (V "head", Native (Arrow () [ListTy () MysteryTy] MysteryTy)             1 head),
    (V "tail", Native (Arrow () [ListTy () MysteryTy] (ListTy () MysteryTy)) 1 tail)
    ]
