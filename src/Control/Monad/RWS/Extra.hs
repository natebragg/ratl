{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}

module Control.Monad.RWS.Extra (
    MonadRS,
    MonadWS,
    execRWT,
) where

import Control.Monad.RWS (RWST, execRWST)
import Control.Monad.State (MonadState, get, put)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Writer (MonadWriter)

type MonadRS r s m = (MonadReader r m, MonadState s m)

type MonadWS w s m = (MonadWriter w m, MonadState s m)

execRWT :: (MonadState s m) => RWST r w s m a -> r -> m w
execRWT m r = do
    s <- get
    (s', w) <- execRWST m r s
    put s'
    return w
