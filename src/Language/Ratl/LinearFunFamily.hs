{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.Ratl.LinearFunFamily (
    LinearFunFamily,
) where

import Numeric.Optimization.Bankroll.LinearFunction (LinearFunction)
import Data.List (partition)
import Data.Mapping (Mapping(..))
import Numeric.Algebra (
    Natural,
    Additive(..),
    Abelian,
    Multiplicative(..),
    Semiring,
    Group(..),
    Monoidal(..),
    LeftModule(..),
    RightModule(..),
    )
import Prelude hiding (negate, (+))
import qualified Prelude as P (negate, (+))

newtype LinearFunFamily b = LinearFunFamily { unfamily :: [(b, LinearFunction)] }
    deriving (Show, Eq, Ord)

instance Mapping (LinearFunFamily b) b LinearFunction where
    lookupBy f   = lookupBy f . unfamily
    updateBy f k v = LinearFunFamily . updateBy f k v . unfamily
    deleteBy f   = LinearFunFamily . deleteBy f . unfamily

    fromList = LinearFunFamily
    elements = elements . unfamily

instance Eq b => Additive (LinearFunFamily b) where
    fs + f's = LinearFunFamily $ merge $ unfamily fs ++ unfamily f's
        where merge [] = []
              merge fs@((b, _):_) =
                case partition ((==b) . fst) fs of
                    (bs, unbs) -> (b, foldr1 (+) $ map snd bs):merge unbs

instance (Eq b, Semiring r, RightModule r LinearFunction) => RightModule r (LinearFunFamily b) where
    fs *. n = LinearFunFamily $ fmap (fmap (*. n)) $ unfamily fs

instance (Eq b, Semiring r, LeftModule r LinearFunction) => LeftModule r (LinearFunFamily b) where
    n .* fs = LinearFunFamily $ fmap (fmap (n .*)) $ unfamily fs

instance Eq b => Monoidal (LinearFunFamily b) where
    zero = LinearFunFamily []

instance Eq b => Group (LinearFunFamily b) where
    negate = LinearFunFamily . fmap (fmap negate) . unfamily
