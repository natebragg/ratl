module Language.Ratl.Ty (
    Ty(..),
    varname,
    varnum,
    FunTy(..),
) where

import Data.Char (chr, ord)

data Ty   = NatTy
          | ListTy Ty
          | PairTy (Ty, Ty)
          | BooleanTy
          | UnitTy
          | SymTy
          | Tyvar String
    deriving (Eq, Ord)

instance Show Ty where
    show NatTy = "int"
    show (ListTy t) = "(list " ++ show t ++ ")"
    show (PairTy (t1, t2)) = "(pair " ++ show t1 ++ " " ++ show t2 ++ ")"
    show BooleanTy = "bool"
    show UnitTy = "unit"
    show SymTy = "sym"
    show (Tyvar x) = "'" ++ x

varname :: Int -> String
varname n = if m > 0 then 'a':show m else [chr (ord 'a' + n)]
    where m = n - (ord 'z' - ord 'a')

varnum :: String -> Int
varnum (v:ms) = (ord v - ord 'a') +
                if not $ null ms then (ord 'z' - ord 'a') + read ms else 0

data FunTy = Arrow Ty Ty

instance Show FunTy where
    show (Arrow t t') = "(" ++ unpair t ++ " -> " ++ show t' ++ ")"
        where unpair (PairTy (t1, t2)) = show t1 ++ " " ++ unpair t2
              unpair t = show t
