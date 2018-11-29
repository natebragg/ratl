module Language.Ratl.Ty (
    Ty(..),
    varname,
    varnum,
    TyvarEnv,
    freevars,
    subst,
    FunTy(..),
) where

import Data.Char (chr, ord)
import Data.Function (on)
import Data.List (intercalate, unionBy)
import Data.Semigroup (Semigroup(..))

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

freevars :: Ty -> [String]
freevars       (ListTy ty) = freevars ty
freevars (PairTy (t1, t2)) = freevars t1 ++ freevars t2
freevars         (Tyvar y) = [y]
freevars                 _ = []

freein :: String -> Ty -> Bool
freein x t = x `elem` freevars t

alpha :: String -> Ty -> Ty
alpha x t = rename t
    where x' = varname $ 1 + (maximum $ map varnum $ freevars t)
          rename (Tyvar y) | x == y = Tyvar x'
          rename (ListTy t) = ListTy $ rename t
          rename (PairTy (t1, t2)) = PairTy (rename t1, rename t2)
          rename t = t

type TyvarEnv = [(String, Ty)]

subst :: TyvarEnv -> Ty -> Ty
subst theta = go
    where go       (ListTy ty) = ListTy $ go ty
          go (PairTy (t1, t2)) = PairTy (go t1, go t2)
          go         (Tyvar x) = maybe (Tyvar x) id $ lookup x theta
          go                 t = t

(~~) :: Ty -> Ty -> TyvarEnv
t'              ~~ t | t == t'      = []
Tyvar x         ~~ t | x `freein` t = [(x, alpha x t)]
Tyvar x         ~~ t                = [(x, t)]
t               ~~ Tyvar x          = Tyvar x ~~ t
ListTy t        ~~ ListTy t'        = t ~~ t'
PairTy (t1, t2) ~~ PairTy (t3, t4)  =
    let theta1  = t1 ~~ t3
        theta2 = subst theta1 t2 ~~ subst theta1 t4
    in  unionBy ((==) `on` fst) (fmap (fmap (subst theta2)) theta1) theta2
t' ~~ t = error ("Failed to unify " ++ show t' ++ " and " ++ show t)

instance Semigroup Ty where
    t <> t' = subst (t ~~ t') t

instance Monoid Ty where
    mempty = Tyvar "a"
    mappend = (<>)

data FunTy = Arrow [Ty] Ty

instance Show FunTy where
    show (Arrow t t') = "(" ++ intercalate " " (map show t ++ ["->", show t']) ++ ")"
