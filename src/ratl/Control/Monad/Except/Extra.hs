{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Control.Monad.Except.Extra (
    unlessJust,
    unlessJustM,
    toError,
) where

import Control.Monad.Except (Except, runExcept, MonadError(..))

unlessJustM :: Monad m => m (Maybe a) -> m a -> m a
unlessJustM p f = p >>= flip unlessJust f

unlessJust :: Applicative f => Maybe a -> f a -> f a
unlessJust p f = maybe f pure p

class MonadError e m => ConvertError e m | m -> e where
    toError :: MonadError e n => m a -> n a

instance ConvertError e (Either e) where
    toError = either throwError return

instance ConvertError e (Except e) where
    toError = toError . runExcept
