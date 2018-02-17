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
less [Nat n, Nat m] = Boolean $ embed $ project n < project m

head :: [Val] -> Val
head [List (Cons x _)] = x

tail :: [Val] -> Val
tail [List (Cons _ xs)] = List xs

cons :: [Val] -> Val
cons [x, List xs] = List (Cons x xs)

arity :: Var -> Int
arity = maybe 1 (\(Native _ a _) -> a) . lookupFun basis

prims :: Prog ()
prims = makeProg [
    -- arithmetic operations
    (V "+",    Native (Arrow ((), ()) [NatTy, NatTy] NatTy)                                       2 plus),

    -- comparison operations
    (V "<",    Native (Arrow ((), ()) [NatTy, NatTy] BooleanTy)                                   2 less),

    -- list functions
    (V "head", Native (Arrow ((), ()) [ListTy [] (Tyvar "a")] (Tyvar "a"))                        1 head),
    (V "tail", Native (Arrow ((), ()) [ListTy [] (Tyvar "a")] (ListTy [] (Tyvar "a")))            1 tail),
    (V "cons", Native (Arrow ((), ()) [Tyvar "a", ListTy [] (Tyvar "a")] (ListTy [] (Tyvar "a"))) 2 cons)
    ]

basis :: Prog ()
basis = prims
