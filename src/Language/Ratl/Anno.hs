module Language.Ratl.Anno (
    Anno,
    annotate,
    freshAnno,
    freshListTy,
    freshFunTy,
) where

import Control.Monad (replicateM)
import Control.Monad.State (StateT, get, put)
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
    Prog,
    )

type Anno = Int

annotate :: Monad m => Prog () -> StateT Anno m (Prog Anno)
annotate = mapM (mapM annoF)
    where annoF (Fun ty x e) = do
                ty' <- annoFTy ty
                return $ Fun ty' x e
          annoF (Native ty a f) = do
                ty' <- annoFTy ty
                return $ Native ty' a f
          annoFTy (Arrow _ ts1 t2) = do
                ts1' <- mapM annoTy ts1
                t2' <- annoTy t2
                freshFunTy ts1' t2'
          annoTy (ListTy _ ty) = do
                ty' <- annoTy ty
                freshListTy ty'
          annoTy NatTy = return NatTy
          annoTy (Tyvar x) = return (Tyvar x)

freshAnno :: Monad m => StateT Anno m Anno
freshAnno = do
    q <- get
    put (q + 1)
    return q

freshListTy :: Monad m => Ty Anno -> StateT Anno m (Ty Anno)
freshListTy tau = do
    ps <- replicateM 1 freshAnno
    return $ ListTy ps tau

freshFunTy :: Monad m => [Ty Anno] -> Ty Anno -> StateT Anno m (FunTy Anno)
freshFunTy taus tau' = do
    q <- freshAnno
    return $ Arrow q taus tau'
