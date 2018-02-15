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
    Prog,
    makeProg,
    lookupFun,
    )

import Prelude hiding (head, tail)

plus :: [Val] -> Val
plus [Nat n, Nat m] = Nat $ embed (project n + project m)

less :: [Val] -> Val
less [Nat n, Nat m] = Nat $ embed (if project n < project m then 1 else 0)

head :: [Val] -> Val
head [List (Cons x _)] = x

tail :: [Val] -> Val
tail [List (Cons _ xs)] = List xs

cons :: [Val] -> Val
cons [x, List xs] = List (Cons x xs)

arity :: Var -> Int
arity = maybe 1 (\(Native _ a _) -> a) . lookupFun basis

basis :: Prog ()
basis = makeProg [
    (V "+",    Native (Arrow ((), ()) [NatTy, NatTy] NatTy)                                       2 plus),
    (V "<",    Native (Arrow ((), ()) [NatTy, NatTy] NatTy)                                       2 less),
    (V "head", Native (Arrow ((), ()) [ListTy [] (Tyvar "a")] (Tyvar "a"))                        1 head),
    (V "tail", Native (Arrow ((), ()) [ListTy [] (Tyvar "a")] (ListTy [] (Tyvar "a")))            1 tail),
    (V "cons", Native (Arrow ((), ()) [Tyvar "a", ListTy [] (Tyvar "a")] (ListTy [] (Tyvar "a"))) 2 cons)
    ]
