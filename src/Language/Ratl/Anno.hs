{-# LANGUAGE FlexibleContexts #-}

module Language.Ratl.Anno (
    Anno,
    Annotatory(..),
    reannotate,
    freshAnno,
) where

import Control.Monad (replicateM)
import Control.Monad.State (MonadState, get, put)
import Control.Arrow (second)

import Language.Ratl.Ty (
    Ty(..),
    FunTy(..),
    )
import Language.Ratl.Ast (
    Fun(..),
    Prog,
    travProg,
    )

type Anno = Int

reannotate :: (Traversable t, MonadState Anno m) => t Anno -> m (t Anno)
reannotate = traverse ((const <$> freshAnno <*>) . pure)

class Annotatory a where
    annotate :: MonadState Anno m => Int -> a b -> m (a Anno)

instance Annotatory Prog where
    annotate deg_max p = travProg (traverse (annotate deg_max)) p

instance Annotatory Fun where
    annotate deg_max (Fun ty x e) = do
                ty' <- annotate deg_max ty
                return $ Fun ty' x e
    annotate deg_max (Native ty a f) = do
                ty' <- annotate deg_max ty
                return $ Native ty' a f

instance Annotatory FunTy where
    annotate deg_max (Arrow _ ts1 t2) = do
        ts1' <- mapM (annotate deg_max) ts1
        t2' <- annotate deg_max t2
        q  <- freshAnno
        q' <- freshAnno
        return $ Arrow (q, q') ts1' t2'

instance Annotatory Ty where
    annotate deg_max (ListTy _ ty) = do
                ty' <- annotate deg_max ty
                ps <- replicateM deg_max freshAnno
                return $ ListTy ps ty'
    annotate deg_max NatTy = return NatTy
    annotate deg_max BooleanTy = return BooleanTy
    annotate deg_max (Tyvar x) = return (Tyvar x)

freshAnno :: MonadState Anno m => m Anno
freshAnno = do
    q <- get
    put (q + 1)
    return q
