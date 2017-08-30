module Language.Ratl.Basis (
    arity,
    apply,
) where

import Language.Ratl.Ast (
    Embeddable(..),
    Nat(..),
    List(..),
    Val(..),
    Var(..),
    )

import Control.Monad (guard)
import Prelude hiding (head, tail)

plus :: Nat -> Nat -> Nat
plus n m = embed (project n + project m)

head :: List -> Val
head (Cons x _) = x

tail :: List -> List
tail (Cons _ xs) = xs

arity :: Var -> Int
arity x = maybe 1 fst $ lookup x basis

apply :: Var -> [Val] -> Maybe Val
apply x vs = do
    (arity, f) <- lookup x basis
    guard $ arity == length vs
    return $ f vs

basis :: [(Var, (Int, [Val] -> Val))]
basis = [
    (V "if",   (3, \[p, t, f] -> undefined)),
    (V "+",    (2, \[Nat n, Nat m] -> Nat $ plus n m)),
    (V "head", (1, \[List xs] -> head xs)),
    (V "tail", (1, \[List xs] -> List $ tail xs))
    ]
