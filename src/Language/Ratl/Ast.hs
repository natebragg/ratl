{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Ratl.Ast (
    Embeddable(..),
    Nat(..),
    List(..),
    Var(..),
    Val(..),
    Fun(..),
    Ex(..),
    Prog(..)
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

instance Functor Fun where
    fmap f (Fun ty x e) = Fun (fmap f ty) x e
    fmap f (Native ty a g) = Native (fmap f ty) a g

instance Foldable Fun where
    foldMap f (Fun ty _ _) = foldMap f ty
    foldMap f (Native ty _ _) = foldMap f ty

instance Traversable Fun where
    traverse f (Fun ty x e) = Fun <$> traverse f ty <*> pure x <*> pure e
    traverse f (Native ty a g) = Native <$> traverse f ty <*> pure a <*> pure g

data Ex = Var Var | Val Val | App Var [Ex]
    deriving (Show, Eq)

newtype Prog a = Prog {getProg :: [(Var, Fun a)]}
    deriving (Monoid)

instance Functor Prog where
    fmap f (Prog p) = Prog $ fmap (fmap (fmap f)) p

instance Foldable Prog where
    foldMap f (Prog p) = foldMap (foldMap (foldMap f)) p

instance Traversable Prog where
    traverse f (Prog p) = Prog <$> traverse (traverse (traverse f)) p
