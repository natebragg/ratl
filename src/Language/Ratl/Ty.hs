module Language.Ratl.Ty (
    Ty(..),
    eqTy,
    varname,
    varnum,
    FunTy(..),
) where

import Data.Char (chr, ord)

data Ty a = NatTy
          | ListTy [a] (Ty a)
          | BooleanTy
          | UnitTy
          | Tyvar String
    deriving (Eq, Ord)

instance Show (Ty a) where
    show NatTy = "Nat"
    show (ListTy _ t) = "[" ++ show t ++ "]"
    show BooleanTy = "Boolean"
    show UnitTy = "Unit"
    show (Tyvar x) = "'" ++ x

instance Functor Ty where
    fmap _        NatTy  = NatTy
    fmap f (ListTy qs t) = ListTy (fmap f qs) (fmap f t)
    fmap _    BooleanTy  = BooleanTy
    fmap _       UnitTy  = UnitTy
    fmap _     (Tyvar x) = Tyvar x

instance Foldable Ty where
    foldMap _        NatTy  = mempty
    foldMap f (ListTy qs t) = foldMap f qs `mappend` foldMap f t
    foldMap _    BooleanTy  = mempty
    foldMap _       UnitTy  = mempty
    foldMap _     (Tyvar x) = mempty

instance Traversable Ty where
    traverse _        NatTy  = pure NatTy
    traverse f (ListTy qs t) = ListTy <$> traverse f qs <*> traverse f t
    traverse _    BooleanTy  = pure BooleanTy
    traverse _       UnitTy  = pure UnitTy
    traverse _     (Tyvar x) = pure $ Tyvar x

eqTy :: Ty a -> Ty a -> Bool
eqTy        NatTy         NatTy = True
eqTy (ListTy _ t) (ListTy _ t') = eqTy t t'
eqTy    BooleanTy     BooleanTy = True
eqTy       UnitTy        UnitTy = True
eqTy    (Tyvar x)     (Tyvar y) = x == y
eqTy            _             _ = False

varname :: Int -> String
varname n = if m > 0 then 'a':show m else [chr (ord 'a' + n)]
    where m = n - (ord 'z' - ord 'a')

varnum :: String -> Int
varnum (v:ms) = (ord v - ord 'a') +
                if not $ null ms then (ord 'z' - ord 'a') + read ms else 0

data FunTy a = Arrow (a, a) [Ty a] (Ty a)

instance Show (FunTy a) where
    show (Arrow _ ts t') = concatMap (\t -> show t ++ " -> ") ts ++ show t'

instance Functor FunTy where
    fmap f (Arrow (q, q') ts t') = Arrow (f q, f q') (fmap (fmap f) ts) (fmap f t')

instance Foldable FunTy where
    foldMap f (Arrow (q, q') ts t') = f q `mappend` f q' `mappend` foldMap (foldMap f) ts `mappend` foldMap f t'

instance Traversable FunTy where
    traverse f (Arrow (q, q') ts t') = Arrow <$> ((,) <$> f q <*> f q') <*> traverse (traverse f) ts <*> traverse f t'
