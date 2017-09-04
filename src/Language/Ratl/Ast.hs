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

data Nat = N Int
    deriving (Eq)

instance Embeddable Nat where
    type HostType Nat = Int
    embed = N . max 0

    project (N n) = n

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
    deriving (Eq)

instance Show Var where
    show (V x) = x

data Val = List List | Nat Nat
    deriving (Eq)

instance Show Val where
    show (List xs) = show xs
    show (Nat n) = show n

data Fun a = Fun (FunTy a) Var Ex
           | Native (FunTy a) Int ([Val] -> Val)

instance Show (Fun a) where
    show _ = "(fn ...)"

data Ex = Var Var | Val Val | App Var [Ex]
    deriving (Show, Eq)

type Prog a = [(Var, Fun a)]
