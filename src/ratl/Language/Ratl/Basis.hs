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
    Val(..),
    )
import Language.Ratl.Ast (
    RuntimeError(..),
    Var(..),
    Fun(..),
    Prog,
    makeProg,
    lookupFun,
    )

import Control.Monad.Except (MonadError, throwError)
import Data.ByteString.Char8 (unpack)
import Data.FileEmbed (embedFile)

arith :: Monad m => (Int -> Int -> Int) -> [Val] -> m Val
arith op [n, m] = return $ embed $ project n `op` project m

divide :: MonadError RuntimeError m => [Val] -> m Val
divide [_, m] | m == embed (0 :: Int) = throwError DivideByZeroError
divide vs = arith div vs

cmp :: Monad m => (Int -> Int -> Bool) -> [Val] -> m Val
cmp op [n, m] = return $ embed $ project n `op` project m

equal :: Monad m => [Val] -> m Val
equal [a, b] = return $ embed $ a == b

null' :: Monad m => [Val] -> m Val
null' [Cons _ _] = return $ embed False
null' [Nil]      = return $ embed True

cons :: Monad m => [Val] -> m Val
cons [x, xs] = return $ Cons x xs

car :: MonadError RuntimeError m => [Val] -> m Val
car [Cons x _] = return x
car [Nil] = throwError EmptyError

cdr :: MonadError RuntimeError m => [Val] -> m Val
cdr [Cons _ xs] = return xs
cdr [Nil] = throwError EmptyError

error' :: MonadError RuntimeError m => [Val] -> m Val
error' [v] = throwError $ UserError v

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
    (V "cdr",   Native (Arrow [ListTy (Tyvar "a")] (ListTy (Tyvar "a")))            cdr),

    -- pair functions
    (V "pair",  Native (Arrow [Tyvar "a", Tyvar "b"] (PairTy (Tyvar "a") (Tyvar "b"))) cons),
    (V "fst",   Native (Arrow [PairTy (Tyvar "a") (Tyvar "b")] (Tyvar "a"))         car),
    (V "snd",   Native (Arrow [PairTy (Tyvar "a") (Tyvar "b")] (Tyvar "b"))         cdr),

    -- io functions
    (V "error", Native (Arrow [Tyvar "a"] (Tyvar "b"))                              error')
    ]

basis :: String
basis = unpack $(embedFile "lib/basis.ratl")
