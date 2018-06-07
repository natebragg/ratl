{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Language.Ratl.Val (
    Span(..),
    Embeddable(..),
    List(Nil, Cons),
    Val(List, Nat, Boolean, Unit, Sym),
    LitList(LitNil, LitCons),
    litList,
    Lit(LitList, LitNat, LitBoolean, LitUnit, LitSym),
    litSpan,
) where

import Text.Parsec.Pos (SourcePos)

data Span = Span SourcePos SourcePos
          | Unknown

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

newtype LitList = LitListT { unsexp :: ListRep (Fix LitRep) }
    deriving Eq

{-# COMPLETE LitNil, LitCons #-}

pattern LitNil = LitListT NilRep
pattern LitCons v xs <- ((\case
            l@(LitListT (ConsRep v xs)) -> (l, Lit v, LitListT xs)
            l -> (l, undefined, undefined)) -> (LitListT (ConsRep _ _), v, xs))
    where LitCons v xs = LitListT (ConsRep (unlit v) (unsexp xs))

litList :: [Lit] -> LitList
litList = LitListT . foldr (ConsRep . unlit) NilRep

newtype Nat = N Int
    deriving (Eq)

instance Embeddable Int where
    embed = Nat . N . max 0
    project (Nat (N n)) = n
    project v = projectionBug v

instance Show Nat where
    show (N n) = show n

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

newtype LitRep a = LitRep { unlitrep :: (Span, ValRep a) }

instance Eq a => Eq (LitRep a) where
    (LitRep (_, a)) == (LitRep (_, b)) = a == b

instance Embeddable a => Embeddable (LitRep a) where
    embed = embed . snd . unlitrep
    project = LitRep . (,) Unknown . project

instance Show a => Show (LitRep a) where
    show = show . snd . unlitrep

newtype Lit = Lit { unlit :: (Fix LitRep) }
    deriving Eq

{-# COMPLETE LitList, LitNat, LitBoolean, LitUnit, LitSym #-}

pattern LitList span xs <- ((\case
            v@(Lit (Fix (LitRep (_, ListRep xs)))) -> (v, LitListT xs)
            v -> (v, undefined)) -> (Lit (Fix (LitRep (span, ListRep _))), xs))
    where LitList span xs = Lit (Fix (LitRep (span, ListRep (unsexp xs))))
pattern LitNat span n = Lit (Fix (LitRep (span, NatRep (N n))))
pattern LitBoolean span b = Lit (Fix (LitRep (span, BooleanRep (B b))))
pattern LitUnit span = Lit (Fix (LitRep (span, UnitRep)))
pattern LitSym span s = Lit (Fix (LitRep (span, SymRep (S s))))

litSpan :: Lit -> Span
litSpan = fst . unlitrep . unfix . unlit

instance Embeddable Lit where
    embed = embed . unlit
    project = Lit . project

instance Show Lit where
    show = show . unlit
