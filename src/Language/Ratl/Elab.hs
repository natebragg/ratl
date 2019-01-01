{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Ratl.Elab (
    Env(..),
    elaborate,
    Unifiable(uid),
    unify,
    instantiate,
    solve,
) where

import Data.Foldable (foldrM)
import Data.Function (on)
import Data.List (intersect, intercalate, union, unionBy, nub, foldl')
import Control.Arrow (second)
import Control.Monad (when, void)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Except.Extra (unlessJustM)
import Control.Monad.Reader (runReaderT, withReaderT, ReaderT, asks)

import Language.Ratl.Ty (
    Tyvar,
    Ty(..),
    FreshTyvar,
    evalFresh,
    freshTyvar,
    FunTy(..),
    )
import Language.Ratl.Val (
    List(..),
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
    travFun,
    )
import Language.Ratl.Basis (arity)

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

alpha :: (Unifiable t, Monad m) => Tyvar -> t -> FreshTyvar m t
alpha x t = (\t' -> subst [(x, t')] t) <$> freshTyvar

compose :: TyvarEnv -> TyvarEnv -> TyvarEnv
compose theta2 theta1 = unionBy ((==) `on` fst) (fmap (fmap (subst theta2)) theta1) theta2

class Unifiable t where
    uid :: t
    freeVars :: t -> [Tyvar]
    subst :: TyvarEnv -> t -> t
    (~~) :: MonadError TypeError m => t -> t -> FreshTyvar m TyvarEnv

solve :: (MonadError TypeError m, Unifiable t) => t -> t -> m TyvarEnv
solve t t' =
    let fv's  = freeVars t'
    in  evalFresh (foldrM alpha t fv's >>= (t' ~~)) (freeVars t ++ fv's)

unify :: Unifiable t => t -> t -> t
unify t t' = either (error . show) (flip subst t') $ solve t t'

instance Unifiable Ty where
    uid = Tyvar "a"

    freeVars       (ListTy ty) = freeVars ty
    freeVars (PairTy (t1, t2)) = freeVars t1 ++ freeVars t2
    freeVars         (Tyvar y) = [y]
    freeVars                 _ = []

    subst theta = go
        where go       (ListTy ty) = ListTy $ go ty
              go (PairTy (t1, t2)) = PairTy (go t1, go t2)
              go         (Tyvar x) = maybe (Tyvar x) id $ lookup x theta
              go                 t = t

    t'              ~~ t | t == t'             = return []
    Tyvar x         ~~ t | x `elem` freeVars t = (\t' -> [(x, t')]) <$> alpha x t
    Tyvar x         ~~ t                       = return [(x, t)]
    t               ~~ Tyvar x                 = Tyvar x ~~ t
    ListTy t        ~~ ListTy t'               = t ~~ t'
    PairTy (t1, t2) ~~ PairTy (t3, t4)         = do
        theta1 <- t1 ~~ t3
        theta2 <- subst theta1 t2 ~~ subst theta1 t4
        return $ theta2 `compose` theta1
    t' ~~ t = throwError $ TypeError $ [(t', t)]

instance Unifiable [Ty] where
    uid = []

    freeVars = concatMap freeVars

    subst = map . subst

    []     ~~ []       = return []
    []     ~~ ts       = freshTyvar >>= \t' -> [t'] ~~ ts
    ts     ~~ []       = [] ~~ ts
    (t:ts) ~~ (t':t's) = do
        theta1 <- t ~~ t'
        theta2 <- subst theta1 ts ~~ subst theta1 t's
        return $ theta2 `compose` theta1

instance Unifiable FunTy where
    uid = Arrow uid uid

    freeVars (Arrow t t') = freeVars t ++ freeVars t'

    subst theta (Arrow t t') = Arrow (subst theta t) (subst theta t')

    Arrow t1 t2 ~~ Arrow t3 t4 = do
        theta1 <- t1 ~~ t3
        theta2 <- subst theta1 t2 ~~ subst theta1 t4
        return $ theta2 `compose` theta1

class Instantiable t where
    instantiate :: TyvarEnv -> t -> t

instance (Functor f, Instantiable a) => Instantiable (f a) where
    instantiate theta = fmap (instantiate theta)

instance Instantiable Ty where
    instantiate = subst

instance Instantiable FunTy where
    instantiate = subst

instance Instantiable Fun where
    instantiate theta (Fun ty x e) = Fun (instantiate theta ty) x e
    instantiate theta (Native ty a f) = Native (instantiate theta ty) a f

instance Instantiable TypedFun where
    instantiate theta (TypedFun ty x e) = TypedFun (instantiate theta ty) x (instantiate theta e)
    instantiate theta (TypedNative ty a f) = TypedNative (instantiate theta ty) a f

instance Instantiable TypedEx where
    instantiate theta (TypedVar ty x) = TypedVar (instantiate theta ty) x
    instantiate theta (TypedVal ty v) = TypedVal (instantiate theta ty) v
    instantiate theta (TypedIf ty ep et ef) = TypedIf (instantiate theta ty) (instantiate theta ep) (instantiate theta et) (instantiate theta ef)
    instantiate theta (TypedApp ty f es) = TypedApp (instantiate theta ty) f (instantiate theta es)
    instantiate theta (TypedLet ty bs e) = TypedLet (instantiate theta ty) (instantiate theta bs) (instantiate theta e)

class Elab a where
    type Type a :: *
    elab :: MonadError TypeError m => a -> ReaderT Env m (Type a)

instance Elab Prog where
    type Type Prog = TypedProg
    elab = travFun (traverse elab)

instance Elab Fun where
    type Type Fun = TypedFun
    elab (Fun fty@(Arrow pty rty) x e) = do
        ety <- withReaderT (\ce -> ce {gamma = zip [x] pty}) $ elab e
        theta <- solve rty (tyGet ety)
        let ety' = instantiate theta ety
            ty'' = tyGet ety'
        when (rty /= ty'') $
            throwError $ TypeError $ [(rty, ty'')]
        return $ TypedFun fty x ety'
    elab (Native fty a f) =
        return $ TypedNative fty a f

instance Elab Ex where
    type Type Ex = TypedEx
    elab (Var x) = do
        ty <- unlessJustM (asks $ lookup x . gamma) $
            throwError $ NameError x
        return $ TypedVar ty x
    elab (Val v) = do
        ty <- elab v
        return $ TypedVal ty v
    elab (If ep et ef) = do
        etys <- traverse elab [ep, et, ef]
        let ifty = [BooleanTy, uid, uid]
        theta <- solve ifty (map tyGet etys)
        let [etyp', etyt', etyf'] = instantiate theta etys
        return $ TypedIf (tyGet etyf') etyp' etyt' etyf'
    elab (App f es) = do
        etys <- traverse elab es
        when (arity f /= length es) $
            throwError $ ArityError (arity f) (length es)
        Arrow ty ty' <- unlessJustM (asks $ lookup f . phi) $
            throwError $ NameError f
        theta1 <- solve (map tyGet etys) (ty ++ [ty'])
        let tys    = instantiate theta1 ty
            ty''   = instantiate theta1 ty'
        theta2 <- solve (ty ++ [ty']) (map tyGet etys)
        let etys'  = instantiate theta2 etys
            tys'   = map tyGet etys'
            ineqs  = filter (uncurry (/=)) (zip tys' tys)
        when (not $ null ineqs) $
            throwError $ TypeError $ ineqs
        return $ TypedApp ty'' f etys'
    elab (Let bs e) = do
        etyds <- traverse (traverse elab) bs
        let tyds = map (fmap tyGet) etyds
        ety <- withReaderT (\ce -> ce {gamma = reverse tyds ++ gamma ce}) $ elab e
        let ty = tyGet ety
        return $ TypedLet ty etyds ety

instance Elab Val where
    type Type Val = Ty
    elab (Nat _)     = return NatTy
    elab (Boolean _) = return BooleanTy
    elab Unit        = return UnitTy
    elab (Sym _)     = return SymTy
    elab (List l)    = elab l

instance Elab List where
    type Type List = Ty
    elab Nil = return $ ListTy uid
    elab (Cons v vs) = do
        vty <- elab v
        lty <- elab vs
        ty <- case lty of
            ListTy lty' -> do
                theta <- solve vty lty'
                return $ ListTy $ instantiate theta lty'
            t -> throwError $ TypeError [(ListTy vty, t)]
        return ty

elaborate :: (Elab a, MonadError TypeError m) => Prog -> a -> m (Type a)
elaborate p a = runReaderT (elab a) (Env {phi = mapFun (second tyOf) p, gamma = []})
