{-# LANGUAGE TemplateHaskell #-}

module Language.Ratl.Basis (
    arity,
    prims,
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

import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedFile)

plus :: [Val] -> Val
plus [Nat n, Nat m] = Nat $ embed (project n + project m)

less :: [Val] -> Val
less [Nat n, Nat m] = Boolean $ embed $ project n < project m

greater :: [Val] -> Val
greater [Nat n, Nat m] = Boolean $ embed $ project n > project m

equal :: [Val] -> Val
equal [a, b] = Boolean $ embed $ a == b

null' :: [Val] -> Val
null' [List xs] = Boolean $ embed $ xs == Nil

car :: [Val] -> Val
car [List (Cons x _)] = x

cdr :: [Val] -> Val
cdr [List (Cons _ xs)] = List xs

cons :: [Val] -> Val
cons [x, List xs] = List (Cons x xs)

arity :: Var -> Int
arity = maybe 1 (\(Native _ a _) -> a) . lookupFun prims

prims :: Prog ()
prims = makeProg [
    -- arithmetic operations
    (V "+",     Native (Arrow ((), ()) [NatTy, NatTy] NatTy)                                       2 plus),

    -- comparison operations
    (V "<",     Native (Arrow ((), ()) [NatTy, NatTy] BooleanTy)                                   2 less),
    (V ">",     Native (Arrow ((), ()) [NatTy, NatTy] BooleanTy)                                   2 greater),
    (V "=",     Native (Arrow ((), ()) [Tyvar "a", Tyvar "a"] BooleanTy)                           2 equal),

    -- list functions
    (V "null?", Native (Arrow ((), ()) [ListTy [] (Tyvar "a")] BooleanTy)                          1 null'),
    (V "cons",  Native (Arrow ((), ()) [Tyvar "a", ListTy [] (Tyvar "a")] (ListTy [] (Tyvar "a"))) 2 cons),
    (V "car",   Native (Arrow ((), ()) [ListTy [] (Tyvar "a")] (Tyvar "a"))                        1 car),
    (V "cdr",   Native (Arrow ((), ()) [ListTy [] (Tyvar "a")] (ListTy [] (Tyvar "a")))            1 cdr)
    ]

basis :: String
basis = unpack $(embedFile "lib/basis.ratl")
