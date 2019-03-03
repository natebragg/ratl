{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Clp.LinearFunction (
    LinearFunction,
    LinearFunFamily,
    dense,
    sparse,
    unpack,
    coefficients,
) where

import Control.Arrow (second)
import Data.Foldable (toList)
import Data.List (sortOn, partition)
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
    sum)
import Prelude hiding (sum, negate, (+), (*))
import qualified Prelude as P (negate, (+), (*))

-- These instances are not lawful; they are inexact.
instance Additive Double where
    (+) = (P.+)

instance Abelian Double

instance Multiplicative Double where
    (*) = (P.*)

instance Semiring Double

instance RightModule Natural Double where
    m *. n = m * fromIntegral n

instance LeftModule Natural Double where
    (.*) = (*) . fromIntegral

instance Monoidal Double where
    zero = 0.0

instance RightModule Integer Double where
    m *. n = m * fromIntegral n

instance LeftModule Integer Double where
    (.*) = (*) . fromIntegral

instance Group Double where
    negate = P.negate

type LinearFunction = LinFunc Int Double

-- For efficiency, it's a critical invariant that LinFuncs are normalized.
-- Never construct with LinFunc, only sparse and dense.
data LinFunc i a where
    LinFunc :: (Ord i, Enum i, Eq a, Monoidal a) => [(i, a)] -> LinFunc i a

deriving instance (Show i, Show a) => Show (LinFunc i a)
deriving instance (Ord i, Ord a) => Ord (LinFunc i a)
deriving instance (Eq i, Eq a) => Eq (LinFunc i a)

instance Foldable (LinFunc i) where
    foldMap f (LinFunc cs) = go (toEnum 0) $ sortOn fst cs
        where go _ [] = mempty
              go i cs = f (sum $ map snd eqc) `mappend` go (succ i) gtc
                    where (eqc, gtc) = span ((i ==) . fst) cs

    null (LinFunc cs) = null cs

    length (LinFunc []) = 0
    length (LinFunc cs) = 1 + fromEnum (maximum (map fst cs))

    elem a (LinFunc cs) = any ((a ==) . snd) cs

instance Additive a => Additive (LinFunc i a) where
    (LinFunc f) + (LinFunc f') = sparse $ merge (sortOn fst f) (sortOn fst f')
        where merge  f [] = f
              merge [] f' = f'
              merge f@(ia@(i, a):ias) f'@(ia'@(i', a'):ia's) =
                case compare i i' of
                    LT -> ia:merge ias f'
                    EQ -> (i, a + a'):merge ias ia's
                    GT -> ia':merge f ia's

instance Semiring r => RightModule r (LinFunc i r) where
    (LinFunc f) *. n = sparse $ fmap (fmap (* n)) f

instance Semiring r => LeftModule r (LinFunc i r) where
    (.*) = flip (*.)

instance Additive r => RightModule Natural (LinFunc i r) where
    m *. n = m *. fromIntegral n

instance Additive r => LeftModule Natural (LinFunc i r) where
    (.*) = (.*) . fromIntegral

instance (Ord i, Enum i, Eq a, Monoidal a, Additive a) => Monoidal (LinFunc i a) where
    zero = LinFunc []

instance Additive r => RightModule Integer (LinFunc i r) where
    m *. n = m *. fromIntegral n

instance Additive r => LeftModule Integer (LinFunc i r) where
    (.*) = (.*) . fromIntegral

instance (Ord i, Enum i, Eq a, Monoidal a, Group a) => Group (LinFunc i a) where
    negate (LinFunc f) = LinFunc $ fmap (fmap negate) f

dense :: (Ord i, Enum i, Eq a, Monoidal a) => [a] -> LinFunc i a
dense = sparse . zip (enumFrom $ toEnum 0)

sparse :: (Ord i, Enum i, Eq a, Monoidal a) => [(i, a)] -> LinFunc i a
sparse = LinFunc . filter ((/= zero) . snd)

unpack :: (Ord i, Enum i, Eq a, Monoidal a) => [(i, a)] -> [a]
unpack = toList . sparse

coefficients :: LinFunc i a -> ([i], [a])
coefficients (LinFunc cs) = unzip cs

type LinearFunFamily b = FunFamily b Int Double

newtype FunFamily b i a = FunFamily { unfamily :: [(b, LinFunc i a)] }
    deriving (Show, Eq, Ord)

instance Mapping (FunFamily b i a) b (LinFunc i a) where
    lookupBy f   = lookupBy f . unfamily
    updateBy f k v = FunFamily . updateBy f k v . unfamily
    deleteBy f   = FunFamily . deleteBy f . unfamily

    fromList = FunFamily
    elements = elements . unfamily

instance (Eq b, Additive a) => Additive (FunFamily b i a) where
    fs + f's = FunFamily $ merge $ unfamily fs ++ unfamily f's
        where merge [] = []
              merge fs@((b, _):_) =
                case partition ((==b) . fst) fs of
                    (bs, unbs) -> (b, foldr1 (+) $ map snd bs):merge unbs

instance (Eq b, Semiring r) => RightModule r (FunFamily b i r) where
    fs *.    n = FunFamily $ fmap (fmap (*. n)) $ unfamily fs

instance (Eq b, Semiring r) => LeftModule r (FunFamily b i r) where
    (.*) = flip (*.)

instance (Eq b, Additive r) => RightModule Natural (FunFamily b i r) where
    m *. n = m *. fromIntegral n

instance (Eq b, Additive r) => LeftModule Natural (FunFamily b i r) where
    (.*) = (.*) . fromIntegral

instance (Eq b, Additive a) => Monoidal (FunFamily b i a) where
    zero = FunFamily []

instance (Eq b, Additive r) => RightModule Integer (FunFamily b i r) where
    m *. n = m *. fromIntegral n

instance (Eq b, Additive r) => LeftModule Integer (FunFamily b i r) where
    (.*) = (.*) . fromIntegral

instance (Eq b, Ord i, Enum i, Eq a, Group a) => Group (FunFamily b i a) where
    negate = FunFamily . fmap (fmap negate) . unfamily
