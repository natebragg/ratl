{-# LANGUAGE TypeFamilies #-}

module Language.Ratl.Ast (
    Embeddable(..),
    Nat(..),
    List(..),
    Var(..),
    Val(..),
    Fun(..),
    Ex(..),
    Prog
) where

import Language.Ratl.Ty (FunTy)

class Embeddable a where
    type HostType a :: *
    embed :: HostType a -> a
    project :: a -> HostType a

data Nat = Z | S Nat
    deriving (Eq)

instance Embeddable Nat where
    type HostType Nat = Int
    embed n | n <= 0 = Z
    embed n = S $ embed $ n - 1

    project Z = 0
    project (S n) = 1 + project n

instance Show Nat where
    show n = show $ project n

data List = Nil | Cons Val List
    deriving (Eq)

instance Embeddable List where
    type HostType List = [Val]
    embed [] = Nil
    embed (v:vs) = Cons v (embed vs)

    project Nil = []
    project (Cons v vs) = v:project vs

instance Show List where
    show l = show $ project l

data Var = V String
    deriving (Show, Eq)

data Val = List List | Nat Nat
    deriving (Show, Eq)

data Fun = Fun FunTy Var Ex
    deriving (Show)

data Ex = Plus Ex Ex | Head Ex | Tail Ex | Var Var | Val Val | App Var Ex | If Ex Ex Ex
    deriving (Show, Eq)

type Prog = [(Var, Fun)]
