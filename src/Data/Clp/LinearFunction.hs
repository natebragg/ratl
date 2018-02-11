{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Clp.LinearFunction (
    LinearFunction,
    dense,
    sparse,
    unpack,
    coefficients,
) where

import Control.Arrow (second)
import Data.Foldable (toList)
import Data.List (sortOn)

type LinearFunction = LinFunc Int Double

data LinFunc i a where
    LinFunc :: (Ord i, Enum i, Num a) => [(i, a)] -> LinFunc i a

deriving instance (Show i, Show a) => Show (LinFunc i a)

instance Foldable (LinFunc i) where
    foldMap f (LinFunc cs) = go (toEnum 0) $ sortOn fst cs
        where go _ [] = mempty
              go i cs = f (sum $ map snd eqc) `mappend` go (succ i) gtc
                    where (eqc, gtc) = span ((i ==) . fst) cs

    null (LinFunc cs) = null cs

    length (LinFunc []) = 0
    length (LinFunc cs) = 1 + fromEnum (maximum (map fst cs))

    elem a (LinFunc cs) = any ((a ==) . snd) cs

dense :: (Ord i, Enum i, Num a) => [a] -> LinFunc i a
dense = LinFunc . zip (enumFrom $ toEnum 0)

sparse :: (Ord i, Enum i, Num a) => [(i, a)] -> LinFunc i a
sparse = LinFunc

unpack :: (Ord i, Enum i, Num a) => [(i, a)] -> [a]
unpack = toList . sparse

coefficients :: LinFunc i a -> ([i], [a])
coefficients (LinFunc cs) = unzip cs
