{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Fix (
    Fix(..),
) where

newtype Fix f = Fix { unfix :: f (Fix f) }

deriving instance Eq (f (Fix f)) => Eq (Fix f)

instance Show (f (Fix f)) => Show (Fix f) where
    show = show . unfix
