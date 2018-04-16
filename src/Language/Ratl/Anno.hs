{-# LANGUAGE FlexibleContexts #-}

module Language.Ratl.Anno (
    Anno,
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

reannotate :: (Traversable t, MonadState Anno m) => t a -> m (t Anno)
reannotate = traverse $ const freshAnno

freshAnno :: MonadState Anno m => m Anno
freshAnno = do
    q <- get
    put (q + 1)
    return q
