{-# LANGUAGE FlexibleContexts #-}

module Language.Ratl.Anno (
    Anno,
    annotate,
    reannotate,
    freshAnno,
    freshListTy,
    freshFunTy,
) where

import Control.Monad (replicateM)
import Control.Monad.State (MonadState, get, put)
import Control.Arrow (second)

import Language.Ratl.Ty (
    Ty(..),
    FunTy(..),
    )
import Language.Ratl.Ast (
    Nat(..),
    List(..),
    Var(..),
    Val(..),
    Fun(..),
    Ex(..),
    Prog(..),
    )

type Anno = Int

class Annotatory a where
    annotate :: MonadState Anno m => Int -> a b -> m (a Anno)
    reannotate :: MonadState Anno m => a Anno -> m (a Anno)

instance Annotatory Prog where
    annotate deg_max (Prog p) = Prog <$> mapM (mapM (annotate deg_max)) p
    reannotate (Prog p) = Prog <$> mapM (mapM reannotate) p

instance Annotatory Fun where
    annotate deg_max (Fun ty x e) = do
                ty' <- annotate deg_max ty
                return $ Fun ty' x e
    annotate deg_max (Native ty a f) = do
                ty' <- annotate deg_max ty
                return $ Native ty' a f
    reannotate (Fun ty x e) = do
                ty' <- reannotate ty
                return $ Fun ty' x e
    reannotate (Native ty a f) = do
                ty' <- reannotate ty
                return $ Native ty' a f

instance Annotatory FunTy where
    annotate deg_max (Arrow _ ts1 t2) = do
        ts1' <- mapM (annotate deg_max) ts1
        t2' <- annotate deg_max t2
        freshFunTy ts1' t2'
    reannotate (Arrow _ ts1 t2) = do
        ts1' <- mapM reannotate ts1
        t2' <- reannotate t2
        freshFunTy ts1' t2'

instance Annotatory Ty where
    annotate deg_max (ListTy _ ty) = do
                ty' <- annotate deg_max ty
                freshListTy deg_max ty'
    annotate deg_max NatTy = return NatTy
    annotate deg_max (Tyvar x) = return (Tyvar x)
    reannotate (ListTy ps ty) = do
                ty' <- reannotate ty
                freshListTy (length ps) ty'
    reannotate NatTy = return NatTy
    reannotate (Tyvar x) = return (Tyvar x)

freshAnno :: MonadState Anno m => m Anno
freshAnno = do
    q <- get
    put (q + 1)
    return q

freshListTy :: MonadState Anno m => Int -> Ty Anno -> m (Ty Anno)
freshListTy deg_max tau = do
    ps <- replicateM deg_max freshAnno
    return $ ListTy ps tau

freshFunTy :: MonadState Anno m => [Ty Anno] -> Ty Anno -> m (FunTy Anno)
freshFunTy taus tau' = do
    q  <- freshAnno
    q' <- freshAnno
    return $ Arrow (q, q') taus tau'
