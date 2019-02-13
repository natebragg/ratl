{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Ratl.Basis (
    prims,
    basis,
) where

import Language.Ratl.Ty (
    Ty(..),
    FunTy(..),
    )
import Language.Ratl.Val (
    Embeddable(..),
    List(..),
    Val(..),
    )
import Language.Ratl.Ast (
    NativeError(..),
    Var(..),
    Fun(..),
    Prog,
    makeProg,
    lookupFun,
    )

import Control.Monad.Except (Except, throwError)
import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedFile)

arith :: (Int -> Int -> Int) -> [Val] -> Except NativeError Val
arith op [n, m] = return $ embed $ project n `op` project m

divide :: [Val] -> Except NativeError Val
divide [_, m] | m == embed (0 :: Int) = throwError DivideByZeroError
divide vs = arith div vs

cmp :: (Int -> Int -> Bool) -> [Val] -> Except NativeError Val
cmp op [n, m] = return $ embed $ project n `op` project m

equal :: [Val] -> Except NativeError Val
equal [a, b] = return $ embed $ a == b

null' :: [Val] -> Except NativeError Val
null' [List xs] = return $ embed $ xs == Nil

car :: [Val] -> Except NativeError Val
car [List (Cons x _)] = return $ x
car [List Nil] = throwError EmptyError

cdr :: [Val] -> Except NativeError Val
cdr [List (Cons _ xs)] = return $ List xs
cdr [List Nil] = throwError EmptyError

cons :: [Val] -> Except NativeError Val
cons [x, List xs] = return $ List (Cons x xs)

prims :: Prog
prims = makeProg [
    -- arithmetic operations
    (V "+",     Native (Arrow [NatTy, NatTy] NatTy)                                 (arith (+))),
    (V "-",     Native (Arrow [NatTy, NatTy] NatTy)                                 (arith (-))),
    (V "*",     Native (Arrow [NatTy, NatTy] NatTy)                                 (arith (*))),
    (V "/",     Native (Arrow [NatTy, NatTy] NatTy)                                 divide),

    -- comparison operations
    (V "<",     Native (Arrow [NatTy, NatTy] BooleanTy)                             (cmp (<))),
    (V ">",     Native (Arrow [NatTy, NatTy] BooleanTy)                             (cmp (>))),
    (V "=",     Native (Arrow [Tyvar "a", Tyvar "a"] BooleanTy)                     equal),

    -- list functions
    (V "null?", Native (Arrow [ListTy (Tyvar "a")] BooleanTy)                       null'),
    (V "cons",  Native (Arrow [Tyvar "a", ListTy (Tyvar "a")] (ListTy (Tyvar "a"))) cons),
    (V "car",   Native (Arrow [ListTy (Tyvar "a")] (Tyvar "a"))                     car),
    (V "cdr",   Native (Arrow [ListTy (Tyvar "a")] (ListTy (Tyvar "a")))            cdr)
    ]

basis :: String
basis = unpack $(embedFile "lib/basis.ratl")
