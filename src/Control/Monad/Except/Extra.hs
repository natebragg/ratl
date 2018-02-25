module Control.Monad.Except.Extra (
    unlessJust,
    unlessJustM,
) where

unlessJustM :: Monad m => m (Maybe a) -> m a -> m a
unlessJustM p f = p >>= flip unlessJust f

unlessJust :: Applicative f => Maybe a -> f a -> f a
unlessJust p f = maybe f pure p
