module Language.Ratl.Ty (
    Ty(..),
    eqTy,
    varname,
    varnum,
    FunTy(..),
) where

import Data.Char (chr, ord)

data Ty a = NatTy
          | ListTy a (Ty a)
          | Tyvar String

instance Show (Ty a) where
    show NatTy = "Nat"
    show (ListTy _ t) = "[" ++ show t ++ "]"
    show (Tyvar x) = "'" ++ x

eqTy :: Ty a -> Ty a -> Bool
eqTy        NatTy         NatTy = True
eqTy (ListTy _ t) (ListTy _ t') = eqTy t t'
eqTy    (Tyvar x)     (Tyvar y) = x == y
eqTy            _             _ = False

varname :: Int -> String
varname n = if m > 0 then 'z':show m else [chr (ord 'a' + n)]
    where m = n - (ord 'z' - ord 'a')

varnum :: String -> Int
varnum (v:ms) = (ord v - ord 'a') + read ms

data FunTy a = Arrow a [Ty a] (Ty a)

instance Show (FunTy a) where
    show (Arrow _ ts t') = concatMap (\t -> show t ++ " -> ") ts ++ show t'
