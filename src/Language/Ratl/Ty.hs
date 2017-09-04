module Language.Ratl.Ty (
    Ty(..),
    isListTy,
    isNatTy,
    eqTy,
    FunTy(..),
) where

data Ty a = NatTy | ListTy a (Ty a) | MysteryTy

instance Show (Ty a) where
    show NatTy = "Nat"
    show (ListTy _ t) = "[" ++ show t ++ "]"
    show MysteryTy = "forall a. a"

isListTy :: Ty a -> Bool
isListTy NatTy = False
isListTy     _ = True

isNatTy :: Ty a -> Bool
isNatTy (ListTy _ _) = False
isNatTy            _ = True

eqTy :: Ty a -> Ty a -> Bool
eqTy MysteryTy _ = True
eqTy _ MysteryTy = True
eqTy NatTy NatTy = True
eqTy (ListTy _ t) (ListTy _ t') = eqTy t t'
eqTy     _     _ = False

data FunTy a = Arrow a [Ty a] (Ty a)

instance Show (FunTy a) where
    show (Arrow _ ts t') = concatMap (\t -> show t ++ " -> ") ts ++ show t'
