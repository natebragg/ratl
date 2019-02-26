module Language.Ratl.Ty (
    Tyvar,
    Ty(..),
    FreshTyvar,
    evalFresh,
    freshTyvar,
    FunTy(..),
) where

import Control.Monad.State (StateT, evalStateT, get, put)
import Data.Char (chr, ord)
import Data.Function (on)
import Data.List (intercalate, unionBy)
import Data.Semigroup (Semigroup(..))

type Tyvar = String

data Ty   = NatTy
          | ListTy Ty
          | PairTy Ty Ty
          | BooleanTy
          | UnitTy
          | SymTy
          | Tyvar Tyvar
          | ForAll [Tyvar] Ty
    deriving (Eq, Ord)

instance Show Ty where
    show NatTy = "int"
    show (ListTy t) = "(list " ++ show t ++ ")"
    show (PairTy t1 t2) = "(pair " ++ show t1 ++ " " ++ show t2 ++ ")"
    show BooleanTy = "bool"
    show UnitTy = "unit"
    show SymTy = "sym"
    show (Tyvar x) = "'" ++ x
    show (ForAll as t) = "(forall (" ++ unwords as ++ ") " ++ show t ++ ")"

varname :: Int -> Tyvar
varname n = if m > 0 then 'a':show m else [chr (ord 'a' + n)]
    where m = n - (ord 'z' - ord 'a')

varnum :: Tyvar -> Int
varnum (v:ms) = (ord v - ord 'a') +
                if not $ null ms then (ord 'z' - ord 'a') + read ms else 0

type FreshTyvar = StateT Tyvar

evalFresh :: Monad m => FreshTyvar m a -> [Tyvar] -> m a
evalFresh m = evalStateT m . varname . (1 +) . maximum . ((-1):) .  map varnum

freshTyvar :: Monad m => FreshTyvar m Ty
freshTyvar = do
    x <- get
    put $ varname $ 1 + varnum x
    return $ Tyvar x

data FunTy = Arrow [Ty] Ty

instance Show FunTy where
    show (Arrow t t') = "(" ++ intercalate " " (map show t ++ ["->", show t']) ++ ")"
