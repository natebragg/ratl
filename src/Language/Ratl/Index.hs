{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Language.Ratl.Index (
  Indexable,
  Index,
  deg,
  poly,
  index,
  indexDeg,
  zeroIndex,
  factor,
  shift,
  projections,
  projectionsDeg,
  shareCoef,
) where

import Control.Arrow (first, second, (***), (&&&))
import Data.Function (on)
import Data.List (sortBy, groupBy, inits, tails, intercalate, subsequences)
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
    index (PairTy ts)  = do
        ds <- groupBy ((==) `on` deg . PIndex . heads) $
              uncurry (diagonals `on` index) ts
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

shift :: Ty -> [(Index, [Index])]
shift t@(ListTy _) = zip pis $ zipWith payfor sis lis
    where lis = concat $ tail $ index t
          sis = map uncons lis
          pis = map PIndex sis
          payfor (ih, it) il = if deg ih == 0 then [it, il] else [il]
          uncons (LIndex (i:is)) = (i, LIndex is)
shift _ = []

-- Vary each position infinitely while holding everything else constant.
--                   ,----- Per position
--                   |,---- Per position degree
--                   ||,--- Per exact position index
--                   |||,-- Per overall degree
--                   ||||,- Per full index
--                   |||||
--                   VVVVV  full   pos
projections :: Ty -> [[[[[(Index, Index)]]]]]
projections = go
    where assoc (a, (b, c)) = ((a, b), c)
          allpairs = (map (map (first PIndex . assoc) . uncurry diagonals) .) . diagonals
          go :: Ty -> [[[[[(Index, Index)]]]]]
          go (PairTy (t1, t2)) =
            let i1s = index t1
            in  map (map (\i1 -> map (map (flip (,) i1 . PIndex . (,) i1)) $ index t2)) i1s :
                map (map (map (allpairs i1s))) (go t2)
          go t = [map (map (pure . pure . (id &&& id))) $ index t]

-- Vary each position up to degree k while holding everything else constant.
--                             ,--- Per position
--                             |,-- Per position, position degree monotonic
--                             ||,- Per full index, overall degree monotonic
--                             |||
--                             VVV  full   pos
projectionsDeg :: Int -> Ty -> [[[(Index, Index)]]]
projectionsDeg k = map (concatMap overallDeg . take (k + 1)) . projections
    where overallDeg = map $ concat . (takeWhile $ (<= k) . deg . fst . head)

shareCoef :: Num a => Index -> Index -> [(Index, a)]
shareCoef = go
    where go AIndex AIndex = unitCoef AIndex
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
