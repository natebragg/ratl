module Language.Ratl.Ast (
    Nat(..),
    fromNat,
    toNat,
    List(..),
    Var(..),
    Val(..),
    Fun(..),
    Ex(..),
    Prog
) where

import Language.Ratl.Ty (FunTy)

data Nat = Z | S Nat
    deriving (Eq)

fromNat :: Nat -> Int
fromNat Z = 0
fromNat (S n) = 1 + fromNat n

toNat :: Int -> Nat
toNat x | x <= 0 = Z
toNat x = S $ toNat $ x - 1

instance Show Nat where
    show n = show $ fromNat n

data List = Nil | Cons Val List
    deriving (Show, Eq)

data Var = V String
    deriving (Show, Eq)

data Val = List List | Nat Nat
    deriving (Show, Eq)

data Fun = Fun FunTy Var Ex
    deriving (Show)

data Ex = Plus Ex Ex | Head Ex | Tail Ex | Var Var | Val Val | App Var Ex | If Ex Ex Ex
    deriving (Show, Eq)

type Prog = [(Var, Fun)]
