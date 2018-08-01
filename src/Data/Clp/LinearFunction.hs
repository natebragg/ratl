{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module Data.Clp.LinearFunction (
    Linear((|*), (|+|)), (|-|),
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

class Linear f where
    (|*)  :: Num a => f a ->   a -> f a
    (|+|) :: Num a => f a -> f a -> f a

(|-|) :: (Linear f, Num a) => f a -> f a -> f a
l1 |-| l2 = l1 |+| l2 |* (-1)

infixl 7 |*
infixl 6 |+|
infixl 6 |-|

type LinearFunction = LinFunc Int Double

-- For efficiency, it's a critical invariant that LinFuncs are normalized.
-- Never construct with LinFunc, only sparse (or for debugging, dense).
data LinFunc i a where
    LinFunc :: (Ord i, Enum i, Eq a, Num a) => [(i, a)] -> LinFunc i a

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

instance Linear (LinFunc i) where
    (LinFunc f) |* n = sparse $ fmap (fmap (* n)) f
    (LinFunc f) |+| (LinFunc f') = sparse $ merge (sortOn fst f) (sortOn fst f')
        where merge  f [] = f
              merge [] f' = f'
              merge f@(ia@(i, a):ias) f'@(ia'@(i', a'):ia's) =
                case compare i i' of
                    LT -> ia:merge ias f'
                    EQ -> (i, a + a'):merge ias ia's
                    GT -> ia':merge f ia's

dense :: (Ord i, Enum i, Eq a, Num a) => [a] -> LinFunc i a
dense = sparse . zip (enumFrom $ toEnum 0)

sparse :: (Ord i, Enum i, Eq a, Num a) => [(i, a)] -> LinFunc i a
sparse = LinFunc . filter ((/= 0) . snd)

unpack :: (Ord i, Enum i, Eq a, Num a) => [(i, a)] -> [a]
unpack = toList . sparse

coefficients :: LinFunc i a -> ([i], [a])
coefficients (LinFunc cs) = unzip cs

type LinearFunFamily b = FunFamily b Int Double

newtype FunFamily b i a = FunFamily { unfamily :: [(b, LinFunc i a)] }
    deriving (Show, Eq, Ord)

instance Mapping (FunFamily b i a) b (LinFunc i a) where
    lookupBy f   = lookupBy f . unfamily
    updateBy f v = FunFamily . updateBy f v . unfamily
    deleteBy f   = FunFamily . deleteBy f . unfamily

    fromList = FunFamily
    elements = elements . unfamily

instance Eq b => Linear (FunFamily b i) where
    fs |*    n = FunFamily $ fmap (fmap (|* n)) $ unfamily fs
    fs |+| f's = FunFamily $ merge $ unfamily fs ++ unfamily f's
        where merge [] = []
              merge fs@((b, _):_) =
                case partition ((==b) . fst) fs of
                    (bs, unbs) -> (b, foldr1 (|+|) $ map snd bs):merge unbs
