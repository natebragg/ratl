{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Ratl.Elab (
    Env(..),
    elaborate,
    Unifiable(uid),
    subst,
    unify,
    solve,
) where

import Data.Foldable (foldrM)
import Data.Function (on)
import Data.List (intersect, intercalate, union, unionBy, nub, (\\), foldl')
import Data.Mapping (deleteAll)
import Control.Arrow (second)
import Control.Monad (when, void, (<=<))
import Control.Monad.Except (MonadError(..))
import Control.Monad.Except.Extra (unlessJustM)
import Control.Monad.Reader (MonadReader, runReaderT, local, asks)

import Language.Ratl.Ty (
    Tyvar,
    Ty(..),
    FreshTyvar,
    evalFresh,
    freshTyvar,
    FunTy(..),
    )
import Language.Ratl.Val (
    Val(..),
    )
import Language.Ratl.Ast (
    Var(..),
    Fun(..),
    TypedFun(..),
    Ex(..),
    TypedEx(..),
    Prog,
    TypedProg,
    tyOf,
    tyGet,
    mapFun,
    mapProg,
    travFun,
    )

type TyEnv = [(Var, Ty)]
type FunTyEnv = [(Var, FunTy)]
type TyvarEnv = [(Tyvar, Ty)]

data Env = Env {
        gamma :: TyEnv,
        phi :: FunTyEnv
    }

data TypeError = TypeError [(Ty, Ty)]
               | ArityError Int Int
               | NameError Var

instance Show TypeError where
    show (TypeError ts) = "Cannot unify unlike types: " ++ intercalate ", " (map (\(t, t') -> show t ++ " and " ++ show t') ts)
    show (ArityError f a) = "Expected " ++ show f ++ " arguments, but got " ++ show a ++ "."
    show (NameError x) = "Name " ++ show x ++ " is not defined."

class Quantifiable t where
    freeVars :: t -> [Tyvar]
    subst :: TyvarEnv -> t -> t

alpha :: (Quantifiable t, Monad m) => Tyvar -> t -> FreshTyvar m t
alpha x t = (\t' -> subst [(x, t')] t) <$> freshTyvar

compose :: TyvarEnv -> TyvarEnv -> TyvarEnv
compose theta2 theta1 = unionBy ((==) `on` fst) (subst theta2 theta1) theta2

generalize :: [Tyvar] -> Ty -> Ty
generalize bs t = case freeVars t \\ bs of
    [] -> t
    as -> ForAll as t

class Quantifiable t => Unifiable t where
    uid :: t
    (~~) :: MonadError TypeError m => t -> t -> FreshTyvar m TyvarEnv
    freshTy :: Monad m => t -> FreshTyvar m t

infix 4 ~~

solve :: (MonadError TypeError m, Unifiable t) => t -> t -> m TyvarEnv
solve t t' =
    let fv's  = freeVars t'
    in  evalFresh (foldrM alpha t fv's >>= (t' ~~)) (freeVars t ++ fv's)

unify :: Unifiable t => t -> t -> t
unify t t' = either (error . show) (flip subst t') $ solve t t'

instance Quantifiable Ty where
    freeVars    (ListTy ty) = freeVars ty
    freeVars (PairTy t1 t2) = nub $ freeVars t1 ++ freeVars t2
    freeVars      (Tyvar y) = [y]
    freeVars (ForAll as ty) = freeVars ty \\ as
    freeVars              _ = []

    subst theta = go
        where go    (ListTy ty) = ListTy $ go ty
              go (PairTy t1 t2) = PairTy (go t1) (go t2)
              go      (Tyvar x) = maybe (Tyvar x) id $ lookup x theta
              go (ForAll as ty) = ForAll as $ subst (deleteAll as theta) ty
              go              t = t

instance Unifiable Ty where
    uid = Tyvar "a"

    t'           ~~ t | t == t'             = return []
    t'@(Tyvar x) ~~ t | x `elem` freeVars t = throwError $ TypeError [(t', t)]
    Tyvar x      ~~ t                       = return [(x, t)]
    t            ~~ t'@(Tyvar _)            = t' ~~ t
    ForAll as ty ~~ t                       = error "This should not be possible."
    t            ~~ t'@(ForAll _ _)         = t' ~~ t
    ListTy t     ~~ ListTy t'               = t ~~ t'
    PairTy t1 t2 ~~ PairTy t3 t4            = do
        theta1 <- t1 ~~ t3
        theta2 <- subst theta1 t2 ~~ subst theta1 t4
        return $ theta2 `compose` theta1
    t' ~~ t = throwError $ TypeError $ [(t', t)]

    freshTy (ListTy ty   ) = ListTy <$> freshTy ty
    freshTy (PairTy t1 t2) = PairTy <$> freshTy t1 <*> freshTy t2
    freshTy (ForAll as ty) = freshTy =<< foldrM alpha ty as
    freshTy ty             = return ty

instance (Functor f, Foldable f, Quantifiable a) => Quantifiable (f a) where
    freeVars = concatMap freeVars

    subst = fmap . subst

instance Unifiable [Ty] where
    uid = []

    []     ~~ []       = return []
    []     ~~ ts       = freshTyvar >>= \t' -> [t'] ~~ ts
    ts     ~~ []       = [] ~~ ts
    (t:ts) ~~ (t':t's) = do
        theta1 <- t ~~ t'
        theta2 <- subst theta1 ts ~~ subst theta1 t's
        return $ theta2 `compose` theta1

    -- [Ty] is implicitly a universally quantified closure
    freshTy ty = traverse freshTy =<< foldrM alpha ty (freeVars ty)

instance Quantifiable FunTy where
    freeVars (Arrow t t') = freeVars t ++ freeVars t'

    subst theta (Arrow t t') = Arrow (subst theta t) (subst theta t')

instance Unifiable FunTy where
    uid = Arrow uid uid

    Arrow t1 t2 ~~ Arrow t3 t4 = do
        theta1 <- t1 ~~ t3
        theta2 <- subst theta1 t2 ~~ subst theta1 t4
        return $ theta2 `compose` theta1

    -- FunTy is implicitly a universally quantified closure
    freshTy ty = do
        Arrow t1 t2 <- foldrM alpha ty $ freeVars ty
        Arrow <$> traverse freshTy t1 <*> freshTy t2

instance Quantifiable Fun where
    freeVars = freeVars . tyOf

    subst theta (Fun ty xs e) = Fun (subst theta ty) xs e
    subst theta (Native ty f) = Native (subst theta ty) f

instance Quantifiable TypedFun where
    freeVars = freeVars . tyOf

    subst theta (TypedFun ty xs e) = TypedFun (subst theta ty) xs (subst theta e)
    subst theta (TypedNative ty f) = TypedNative (subst theta ty) f

instance Quantifiable Ex where
    freeVars _ = []

    subst _ e = e

instance Quantifiable TypedEx where
    freeVars = freeVars . tyGet

    subst theta (TypedVar ty x) = TypedVar (subst theta ty) x
    subst theta (TypedVal ty v) = TypedVal (subst theta ty) v
    subst theta (TypedIf ty ep et ef) = TypedIf (subst theta ty) (subst theta ep) (subst theta et) (subst theta ef)
    subst theta (TypedApp ty f es) = TypedApp (subst theta ty) f (subst theta es)
    subst theta (TypedLet ty bs e) = TypedLet (subst theta ty) (subst theta bs) (subst theta e)

instance Quantifiable Prog where
    freeVars = concat . mapFun (freeVars . snd)

    subst = mapProg . fmap . subst

class Elab a where
    type Type a :: *
    elab :: (MonadReader Env m, MonadError TypeError m) => a -> FreshTyvar m (Type a)

instance Elab Prog where
    type Type Prog = TypedProg
    elab = travFun (traverse elab)

instance Elab Fun where
    type Type Fun = TypedFun
    elab (Fun fty xs e) = do
        Arrow pty rty <- freshTy fty
        when (length xs /= length pty) $
            throwError $ ArityError (length xs) (length pty)
        ety <- local (\ce -> ce {gamma = zip xs pty}) $ elab e
        theta <- rty ~~ tyGet ety
        let ety' = subst theta ety
        return $ TypedFun fty xs ety'
    elab (Native fty f) =
        return $ TypedNative fty f

instance Elab Ex where
    type Type Ex = TypedEx
    elab (Var x) = do
        ty <- freshTy <=< unlessJustM (asks $ lookup x . gamma) $
            throwError $ NameError x
        return $ TypedVar ty x
    elab (Val v) = do
        ty <- elab v
        return $ TypedVal ty v
    elab (If ep et ef) = do
        etys <- traverse elab [ep, et, ef]
        ifty <- freshTy [BooleanTy, uid, uid]
        theta <- ifty ~~ map tyGet etys
        let [etyp', etyt', etyf'] = subst theta etys
        return $ TypedIf (tyGet etyf') etyp' etyt' etyf'
    elab (App f es) = do
        etys <- traverse elab es
        Arrow ty ty' <- freshTy <=< unlessJustM (asks $ lookup f . phi) $
            throwError $ NameError f
        when (length ty /= length es) $
            throwError $ ArityError (length ty) (length es)
        theta <- ty ++ [ty'] ~~ map tyGet etys
        let ty''  = subst theta ty'
            etys' = subst theta etys
        return $ TypedApp ty'' f etys'
    elab (Let bs e) = do
        etyds <- traverse (traverse elab) bs
        free <- asks $ freeVars . gamma
        let tyds = map (fmap (generalize free . tyGet)) etyds
        ety <- local (\ce -> ce {gamma = reverse tyds ++ gamma ce}) $ elab e
        let ty = tyGet ety
        return $ TypedLet ty etyds ety

-- Incomplete: exclusion = UnitTy, PairTy 'a (ListTy 'b | t in exclusion)
-- Fixing requires '() to type as {ListTy 'a, UnitTy}, and '(_ . ()) to
-- type as {PairTy 'a UnitTy, PairTy 'a (ListTy 'b), ListTy 'a}
-- unification is intersection over set member equivalence.
instance Elab Val where
    type Type Val = Ty
    elab (Nat _)     = return NatTy
    elab (Boolean _) = return BooleanTy
    elab (Sym _)     = return SymTy
    elab Nil         = ListTy <$> freshTyvar
    elab (Cons f s)  = do
        vty <- elab f
        lty <- elab s
        case lty of
            ListTy lty' -> do
                theta <- vty ~~ lty'
                return $ ListTy $ subst theta lty'
            t -> return $ PairTy vty t

elaborate :: (Elab a, Quantifiable a, MonadError TypeError m) => Prog -> a -> m (Type a)
elaborate p a = runReaderT (evalFresh (elab a) (freeVars a)) (Env {phi = mapFun (second tyOf) p, gamma = []})
