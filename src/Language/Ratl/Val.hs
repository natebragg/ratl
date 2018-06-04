{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Language.Ratl.Val (
    Embeddable(..),
    List(Nil, Cons),
    Sym(..),
    Val(List, Nat, Boolean, Unit, Sym),
) where

projectionBug v = error $ "Tried to project to wrong type, which should be impossible: " ++ show v

class Embeddable a where
    embed :: a -> Val
    project :: Val -> a

newtype Fix f = Fix { unfix :: f (Fix f) }

deriving instance Eq (f (Fix f)) => Eq (Fix f)

instance Embeddable (f (Fix f)) => Embeddable (Fix f) where
    embed = embed . unfix
    project = Fix . project

instance Show (f (Fix f)) => Show (Fix f) where
    show = show . unfix

newtype Nat = N Int
    deriving (Eq)

instance Embeddable Int where
    embed = Nat . N . max 0
    project (Nat (N n)) = n
    project v = projectionBug v

instance Show Nat where
    show (N n) = show n

data ListRep v = NilRep | ConsRep v (ListRep v)
    deriving (Eq, Functor)

instance Embeddable a => Embeddable [a] where
    embed = List . go
        where go [] = Nil
              go (v:vs) = Cons (embed v) $ go vs

    project (List a) = go a
        where go Nil = []
              go (Cons v vs) = project v:go vs
    project v = projectionBug v

instance Show v => Show (ListRep v) where
    show l = beg ++ go "" l ++ end
        where (beg, sep, end) = ("(", " ", ")")
              go _ NilRep = ""
              go c (ConsRep v vs) = c ++ show v ++ go sep vs

newtype List = ListT { unlist :: ListRep (Fix ValRep) }
    deriving Eq

{-# COMPLETE Nil, Cons #-}

pattern Nil = ListT NilRep
pattern Cons v xs <- ((\case
            l@(ListT (ConsRep v xs)) -> (l, Val v, ListT xs)
            l -> (l, undefined, undefined)) -> (ListT (ConsRep _ _), v, xs))
    where Cons v xs = ListT (ConsRep (unval v) (unlist xs))

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

data ValRep v = ListRep (ListRep v)
              | NatRep Nat
              | BooleanRep Boolean
              | UnitRep
              | SymRep Sym
    deriving (Eq, Functor)

instance Embeddable v => Embeddable (ValRep v) where
    embed = Val . Fix . fmap (unval . embed)
    project = fmap (project . Val) . unfix . unval

instance Show v => Show (ValRep v) where
    show (ListRep xs) = show xs
    show (NatRep n) = show n
    show (BooleanRep b) = show b
    show UnitRep = show ()
    show (SymRep s) = show s

newtype Val = Val { unval :: (Fix ValRep) }
    deriving Eq

{-# COMPLETE List, Nat, Boolean, Unit, Sym #-}

pattern List xs <- ((\case
            v@(Val (Fix (ListRep xs))) -> (v, ListT xs)
            v -> (v, undefined)) -> (Val (Fix (ListRep _)), xs))
    where List xs = Val (Fix (ListRep (unlist xs)))
pattern Nat n = Val (Fix (NatRep n))
pattern Boolean b = Val (Fix (BooleanRep b))
pattern Unit = Val (Fix UnitRep)
pattern Sym s = Val (Fix (SymRep s))

instance Embeddable Val where
    embed = id
    project = id

instance Show Val where
    show = show . unval
