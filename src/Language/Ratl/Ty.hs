module Language.Ratl.Ty (
    Anno,
    Ty(..),
    isListTy,
    isNatTy,
    eqTy,
    FunTy(..),
    eqFun,
) where

type Anno = Int

data Ty = NatTy | ListTy Anno Ty | MysteryTy

instance Show Ty where
    show NatTy = "Nat"
    show (ListTy _ t) = "[" ++ show t ++ "]"
    show MysteryTy = "forall a. a"

isListTy :: Ty -> Bool
isListTy NatTy = False
isListTy     _ = True

isNatTy :: Ty -> Bool
isNatTy (ListTy _ _) = False
isNatTy            _ = True

eqTy :: Ty -> Ty -> Bool
eqTy MysteryTy _ = True
eqTy _ MysteryTy = True
eqTy NatTy NatTy = True
eqTy (ListTy _ t) (ListTy _ t') = eqTy t t'
eqTy     _     _ = False

data FunTy = Arrow Anno Ty Ty

eqFun :: FunTy -> FunTy -> Bool
eqFun (Arrow _ t t'') (Arrow _ t' t''') = eqTy t t' && eqTy t'' t'''

instance Show FunTy where
    show (Arrow _ t t') = show t ++ " -> " ++ show t'
