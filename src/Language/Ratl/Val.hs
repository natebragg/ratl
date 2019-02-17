{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ratl.Val (
    Span(..),
    Embeddable(..),
    Val(   Nat,    Boolean,    Sym,    Nil,    Cons),
    Lit(LitNat, LitBoolean, LitSym, LitNil, LitCons),
    litCons,
    litList,
    litSpan,
    withSpan,
) where

import Text.Parsec.Pos (SourcePos)
import Data.Fix (Fix(..))

data Span = Span SourcePos SourcePos
          | Unknown

projectionBug v = error $ "Tried to project to wrong type, which should be impossible: " ++ show v

class Embeddable a where
    embed :: a -> Val
    project :: Val -> a

instance Embeddable (f (Fix f)) => Embeddable (Fix f) where
    embed = embed . unfix
    project = Fix . project

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

newtype Sym = S String
    deriving (Eq)

instance Show Sym where
    show (S x) = x

instance Embeddable Sym where
    embed = Sym
    project (Sym x) = x
    project v = projectionBug v

instance Embeddable () where
    embed () = Nil
    project Nil = ()
    project v = projectionBug v

instance (Embeddable a, Embeddable b) => Embeddable (a, b) where
    embed (a, b) = Cons (embed a) (embed b)
    project (Cons f s) = (project f, project s)
    project v = projectionBug v

instance Embeddable a => Embeddable [a] where
    embed = go
        where go [] = Nil
              go (v:vs) = Cons (embed v) $ go vs

    project = go
        where go Nil = []
              go (Cons v vs) = project v:go vs
              go v = projectionBug v

data ValRep v = NatRep Nat
              | BooleanRep Boolean
              | SymRep Sym
              | NilRep
              | ConsRep v v
    deriving Eq

instance Embeddable v => Embeddable (ValRep v) where
    embed (NatRep n)     = Val (Fix (NatRep n))
    embed (BooleanRep b) = Val (Fix (BooleanRep b))
    embed (SymRep s)     = Val (Fix (SymRep s))
    embed  NilRep        = Val (Fix  NilRep)
    embed (ConsRep f s)  = Val (Fix (ConsRep (unval $ embed f) (unval $ embed s)))

    project (Val (Fix (NatRep n)))     = NatRep n
    project (Val (Fix (BooleanRep b))) = BooleanRep b
    project (Val (Fix (SymRep s)))     = SymRep s
    project (Val (Fix  NilRep))        = NilRep
    project (Val (Fix (ConsRep f s)))  = ConsRep (project $ Val f) (project $ Val s)

instance (Show v, Embeddable v) => Show (ValRep v) where
    show (NatRep n) = show n
    show (BooleanRep b) = show b
    show (SymRep s) = show s
    show NilRep = show ()
    show (ConsRep f s) = "(" ++ show f ++ go (unfix $ unval $ embed s) ++ ")"
        where go NilRep = ""
              go (ConsRep f s) = " " ++ show f ++ go (unfix $ unval $ embed s)
              go v = " . " ++ show v

newtype Val = Val { unval :: (Fix ValRep) }
    deriving Eq

{-# COMPLETE Nat, Boolean, Sym, Nil, Cons #-}

pattern Nat n = Val (Fix (NatRep n))
pattern Boolean b = Val (Fix (BooleanRep b))
pattern Sym s = Val (Fix (SymRep s))
pattern Nil = Val (Fix NilRep)
pattern Cons f s <- ((\case
            v@(Val (Fix (ConsRep f s))) -> (v, (Val f, Val s))
            v -> (v, undefined)) -> (Val (Fix (ConsRep _ _)), (f, s)))
    where Cons f s = Val (Fix (ConsRep (unval f) (unval s)))

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

instance (Show a, Embeddable a) => Show (LitRep a) where
    show = show . snd . unlitrep

newtype Lit = Lit { unlit :: (Fix LitRep) }
    deriving Eq

{-# COMPLETE LitNat, LitBoolean, LitSym, LitNil, LitCons #-}

pattern LitNat span n = Lit (Fix (LitRep (span, NatRep (N n))))
pattern LitBoolean span b = Lit (Fix (LitRep (span, BooleanRep (B b))))
pattern LitSym span s = Lit (Fix (LitRep (span, SymRep (S s))))
pattern LitNil span = Lit (Fix (LitRep (span, NilRep)))
pattern LitCons span f s <- ((\case
            v@(Lit (Fix (LitRep (_, ConsRep f s)))) -> (v, (Lit f, Lit s))
            v -> (v, undefined)) -> (Lit (Fix (LitRep (span, ConsRep _ _))), (f, s)))
    where LitCons span f s = Lit (Fix (LitRep (span, ConsRep (unlit f) (unlit s))))

litCons :: Lit -> [Lit] -> Lit
litCons = foldr (LitCons Unknown)

litList :: [Lit] -> Lit
litList = litCons (LitNil Unknown)

litSpan :: Lit -> Span
litSpan = fst . unlitrep . unfix . unlit

withSpan :: Span -> Lit -> Lit
withSpan s = Lit . Fix . LitRep . (,) s . snd . unlitrep . unfix . unlit

instance Embeddable Lit where
    embed = embed . unlit
    project = Lit . project

instance Show Lit where
    show = show . unlit
