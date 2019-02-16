{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Language.Ratl.Index (
  Indexable,
  Index,
  ContextIndex,
  context,
  deg,
  poly,
  index,
  indexDeg,
  zeroIndex,
  transform,
  factor,
  shift,
  project,
  projectDeg,
  shareCoef,
) where

import Control.Arrow (first, second, (***), (&&&))
import Data.Function (on)
import Data.List (sortBy, groupBy, inits, tails, intercalate, subsequences)
import Data.Mapping (mapConcat)
import Data.Monoid (Sum(getSum))
import Data.Semigroup (Semigroup(..))
import Language.Ratl.Ty (Ty(..))

class Indexable i t | i -> t where
    deg :: i -> Int
    poly :: i -> Double
    index :: t -> [[i]]
    zero :: i -> i
    factor :: i -> [(i, Int)]

data Index = AIndex
           | PIndex (Index, Index)
           | LIndex [Index]
    deriving (Eq, Ord)

instance Show Index where
    show AIndex = "∗"
    show (PIndex (i1, i2)) = '(' : show i1 ++ ", " ++ show i2 ++ ")"
    show (LIndex is) = '[' : intercalate ", " (map show is) ++ "]"

instance Indexable Index Ty where
    deg AIndex = 0
    deg (PIndex (i1, i2)) = deg i1 + deg i2
    deg (LIndex is) = length is + sum (map deg is)

    poly AIndex = 1.0
    poly (PIndex (i1, i2)) = poly i1 * poly i2
    poly (LIndex is) = sum $ map product $ subsequences $ map poly is

    index (NatTy)      = [[AIndex]]
    index (BooleanTy)  = [[AIndex]]
    index (UnitTy)     = [[AIndex]]
    index (SymTy)      = [[AIndex]]
    index (Tyvar _)    = [[AIndex]]
    index (PairTy t1 t2)  = do
        ds <- groupBy ((==) `on` deg . PIndex . heads) $
              (diagonals `on` index) t1 t2
        return $ ds >>= \(d1, d2) -> curry PIndex <$> d1 <*> d2
    index (ListTy t) = do
        cs <- combos (index t)
        return $ LIndex <$> (choices =<< cs)

    zero AIndex = AIndex
    zero (PIndex (i1, i2)) = PIndex (zero i1, zero i2)
    zero (LIndex _) = LIndex []

    factor (PIndex (i1, i2)) = [(PIndex (i, zero i2), d) | (i, d) <- factor i1] ++
                               [(PIndex (zero i1, i), d) | (i, d) <- factor i2]
    factor (LIndex is@(i:_)) = (LIndex [zero i], length is) :
                               [(LIndex (reverse $ i:zs), d) | (fs, zs) <- fzs, (i, d) <- fs]
            where fzs = map ((factor . head *** map zero) . splitAt 1 . reverse) $ tail $ inits is
    factor _ = []

data ContextIndex = CIndex { uncontext :: [Index] }
    deriving (Eq, Ord)

context :: [Index] -> ContextIndex
context = CIndex

instance Show ContextIndex where
    show (CIndex is) = '{' : intercalate ", " (map show is) ++ "}"

instance Semigroup ContextIndex where
    CIndex is <> CIndex js = CIndex (is <> js)

instance Monoid ContextIndex where
    mempty = CIndex []
    mappend = (<>)

instance Indexable ContextIndex [Ty] where
    deg (CIndex is) = sum $ map deg is

    poly (CIndex is) = product $ map poly is

    index = fmap (fmap $ CIndex . unpair) . index . foldr PairTy UnitTy
        where unpair (PIndex (i1, i2)) = i1:unpair i2
              unpair _ = []

    zero (CIndex is) = CIndex (map zero is)

    factor (CIndex is) = map (first CIndex) $ go (map factor is) (map zero is)
        where go [] [] = []
              go (f:fs) (z:zs) = map (first (:zs)) f ++ map (first (z:)) (go fs zs)

heads :: ([a], [b]) -> (a, b)
heads = head *** head

-- The cartesian product of two lists in diagonal order.  Using dynamic
-- programming, each diagonal is generated from the previous, intermediately
-- represented by the list tail beginning with the element of interes.
diagonals :: [a] -> [b] -> [(a, b)]
diagonals as bs = concatMap (map heads) $ go $ filter nonempty [(as, bs)]
    where go :: [([a], [b])] -> [[([a], [b])]]
          go [] = []
          go ds = ds:go (filter nonempty $ first tail (head ds):map (second tail) ds)
          nonempty = not . uncurry (||) . (null *** null)

-- All combinations of elements of the inputs ordered by total weight, as
-- determined by in index of the input element.  Starting with the empty
-- list, it incrementally increases the weight of previous solutions to the
-- desired weight by prepending input elements whose weight equals the
-- difference.  Previous solutions are remembered in an accumulator in
-- reverse order by overall weight.  This yields all solutions while only
-- increasing the size of each solution by one each iteration, as larger
-- solutions have already been created during the previous itration.
combos :: [a] -> [[[a]]]
combos xs = go [[[]]]
    where go css@(cs:_) = cs:go (cs':css)
            where cs' = concatMap (\(x, ys) -> map (x:) ys) $ zip xs css

choices :: [[a]] -> [[a]]
choices [] = [[]]
choices (is:cs) = [i:cs' | i <- is, cs' <- choices cs]

indexDeg :: Indexable i t => Int -> t -> [i]
indexDeg k = concat . take (k + 1) . index

zeroIndex :: Indexable i t => t -> i
zeroIndex = head . head . index

transform :: (forall a. [a] -> [a]) -> ContextIndex -> ContextIndex
transform f = context . f . uncontext

shift :: Ty -> [(ContextIndex, [ContextIndex])]
shift t@(ListTy _) = map go $ concat $ tail $ index [t]
    where go jl@(CIndex [LIndex (j:l)]) =
            case (deg j, CIndex [j], CIndex [LIndex l]) of
                (0, j, l) -> (j <> l, [jl, l])
                (_, j, l) -> (j <> l, [jl])
shift _ = []

project :: [Ty] -> [Ty] -> [(ContextIndex, [ContextIndex])]
project jtys gtys = map (flip (,) $ concat $ index gtys) $ concat $ index jtys

projectDeg :: Int -> [Ty] -> [Ty] -> [(ContextIndex, [ContextIndex])]
projectDeg k = ((map (\(ix, ixs) -> (ix, takeWhile (degK . (ix <>)) ixs)) .
                 takeWhile (degK . fst)) .) . project
    where degK = (<= k) . deg

shareCoef :: Num a => ContextIndex -> [(ContextIndex, a)]
shareCoef (CIndex []) = []
shareCoef (CIndex (ix:ixs)) = map (context . pure *** getSum) $ foldl shareAnother (unitCoef ix) ixs
    where shareAnother scs i1 = mapConcat [map (second (c *)) $ go i1 i2 | (i2, c) <- scs]
          go AIndex AIndex = unitCoef AIndex
          go (PIndex (i1, i2)) (PIndex (i1', i2')) =
            [(PIndex (i1, i2), n1 * n2) | ((i1, n1), (i2, n2)) <- diagonals (go i1 i1') (go i2 i2')]
          go (LIndex xs) (LIndex ys) =
            map ((head *** sum) . unzip) $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) $
                concatMap (map ((LIndex *** product) . unzip) . choices) $ interleave xs ys
          unitCoef a = [(a, 1)]
          interleave as [] = [map unitCoef as]
          interleave [] bs = [map unitCoef bs]
          interleave as bs = head $ foldr row (map pure $ tails bs') as
              where bs' = map unitCoef bs
                    col (this_cell, b) cell_right = (this_cell ++ map (b:) cell_right)
                    row a row_below = scanr col (last this_row) $ zip this_row bs'
                      where this_row = zipWith (++) (map (map (unitCoef a:)) row_below)
                                                    (zipWith (map . (:)) (map (go a) bs) (tail row_below) ++ [[]])
