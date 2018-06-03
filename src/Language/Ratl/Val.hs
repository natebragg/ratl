{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Ratl.Val (
    Embeddable(..),
    List(..),
    Sym(..),
    Val(..),
) where

projectionBug v = error $ "Tried to project to wrong type, which should be impossible: " ++ show v

class Embeddable a where
    embed :: a -> Val
    project :: Val -> a

newtype Nat = N Int
    deriving (Eq)

instance Embeddable Int where
    embed = Nat . N . max 0
    project (Nat (N n)) = n
    project v = projectionBug v

instance Show Nat where
    show (N n) = show n

data List = Nil | Cons Val List
    deriving (Eq)

instance Embeddable a => Embeddable [a] where
    embed = List . go
        where go [] = Nil
              go (v:vs) = Cons (embed v) $ go vs

    project (List a) = go a
        where go Nil = []
              go (Cons v vs) = project v:go vs
    project v = projectionBug v

instance Show List where
    show l = beg ++ go "" l ++ end
        where (beg, sep, end) = ("(", " ", ")")
              go _ Nil = ""
              go c (Cons v vs) = c ++ show v ++ go sep vs

newtype Boolean = B Bool
    deriving (Eq)

instance Embeddable Bool where
    embed = Boolean . B
    project (Boolean (B b)) = b
    project v = projectionBug v

instance Show Boolean where
    show (B True) = "#t"
    show (B False) = "#f"

instance Embeddable () where
    embed () = Unit
    project Unit = ()
    project v = projectionBug v

newtype Sym = S String
    deriving (Eq)

instance Show Sym where
    show (S x) = x

instance Embeddable Sym where
    embed = Sym
    project (Sym x) = x
    project v = projectionBug v

data Val = List List
         | Nat Nat
         | Boolean Boolean
         | Unit
         | Sym Sym
    deriving (Eq)

instance Embeddable Val where
    embed = id
    project = id

instance Show Val where
    show (List xs) = show xs
    show (Nat n) = show n
    show (Boolean b) = show b
    show (Sym s) = show s
    show Unit = show ()
