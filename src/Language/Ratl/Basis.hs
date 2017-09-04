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

ifte :: [Val] -> Val
ifte [p, t, f] = undefined

plus :: [Val] -> Val
plus [Nat n, Nat m] = Nat $ embed (project n + project m)

head :: [Val] -> Val
head [List (Cons x _)] = x

tail :: [Val] -> Val
tail [List (Cons _ xs)] = List xs

arity :: Var -> Int
arity x = maybe 1 fst $ lookup x basis

apply :: Var -> [Val] -> Maybe Val
apply x vs = do
    (arity, f) <- lookup x basis
    guard $ arity == length vs
    return $ f vs

basis :: [(Var, (Int, [Val] -> Val))]
basis = [
    (V "if",   (3, ifte)),
    (V "+",    (2, plus)),
    (V "head", (1, head)),
    (V "tail", (1, tail))
    ]
