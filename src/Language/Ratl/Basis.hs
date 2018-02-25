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

arith :: (Int -> Int -> Int) -> [Val] -> Val
arith op [n, m] = embed $ project n `op` project m

cmp :: (Int -> Int -> Bool) -> [Val] -> Val
cmp op [n, m] = embed $ project n `op` project m

equal :: [Val] -> Val
equal [a, b] = embed $ a == b

null' :: [Val] -> Val
null' [List xs] = embed $ xs == Nil

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
    (V "+",     Native (Arrow ((), ()) [NatTy, NatTy] NatTy)                                       2 (arith (+))),
    (V "*",     Native (Arrow ((), ()) [NatTy, NatTy] NatTy)                                       2 (arith (*))),

    -- comparison operations
    (V "<",     Native (Arrow ((), ()) [NatTy, NatTy] BooleanTy)                                   2 (cmp (<))),
    (V ">",     Native (Arrow ((), ()) [NatTy, NatTy] BooleanTy)                                   2 (cmp (>))),
    (V "=",     Native (Arrow ((), ()) [Tyvar "a", Tyvar "a"] BooleanTy)                           2 equal),

    -- list functions
    (V "null?", Native (Arrow ((), ()) [ListTy [] (Tyvar "a")] BooleanTy)                          1 null'),
    (V "cons",  Native (Arrow ((), ()) [Tyvar "a", ListTy [] (Tyvar "a")] (ListTy [] (Tyvar "a"))) 2 cons),
    (V "car",   Native (Arrow ((), ()) [ListTy [] (Tyvar "a")] (Tyvar "a"))                        1 car),
    (V "cdr",   Native (Arrow ((), ()) [ListTy [] (Tyvar "a")] (ListTy [] (Tyvar "a")))            1 cdr)
    ]

basis :: String
basis = unpack $(embedFile "lib/basis.ratl")
