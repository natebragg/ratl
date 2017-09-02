module Language.Ratl.Anno (
    Anno,
    annotate,
    freshAnno,
    freshListTy,
    freshFunTy,
) where

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
          annoFTy (Arrow () t1 t2) = do
                t1' <- annoTy t1
                t2' <- annoTy t2
                freshFunTy t1' t2'
          annoTy (ListTy () ty) = do
                ty' <- annoTy ty
                freshListTy ty'
          annoTy NatTy = return NatTy
          annoTy MysteryTy = return MysteryTy

freshAnno :: Monad m => StateT Anno m Anno
freshAnno = do
    q <- get
    put (q + 1)
    return q

freshListTy :: Monad m => Ty Anno -> StateT Anno m (Ty Anno)
freshListTy tau = do
    p <- freshAnno
    return $ ListTy p tau

freshFunTy :: Monad m => Ty Anno -> Ty Anno -> StateT Anno m (FunTy Anno)
freshFunTy tau tau' = do
    q <- freshAnno
    return $ Arrow q tau tau'
