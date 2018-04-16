module Language.Ratl.Index (
  Index,
  deg,
  index,
  indexDeg,
  zeroIndex,
  shift,
  inject,
) where

import Control.Arrow (first, second, (***))
import Data.Function (on)
import Data.List (groupBy, inits)
import Language.Ratl.Ty (Ty(..))

data Index = AIndex
           | VIndex
           | PIndex (Index, Index)
           | LIndex [Index]
    deriving (Eq, Ord, Show)

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

deg :: Index -> Int
deg AIndex = 0
deg VIndex = 0
deg (PIndex (i1, i2)) = deg i1 + deg i2
deg (LIndex is) = length is + sum (map deg is)

index :: Ty a -> [[Index]]
index (NatTy)      = [[AIndex]]
index (BooleanTy)  = [[AIndex]]
index (UnitTy)     = [[AIndex]]
index (SymTy)      = [[AIndex]]
index (Tyvar _)    = [[VIndex]]
index (PairTy ts)  = do
    ds <- groupBy ((==) `on` deg . PIndex . heads) $
          uncurry (diagonals `on` index) ts
    return $ ds >>= \(d1, d2) -> curry PIndex <$> d1 <*> d2
index (ListTy _ t) = do
    cs <- combos (index t)
    return $ LIndex <$> (choices =<< cs)

indexDeg :: Int -> Ty a -> [Index]
indexDeg k = concat . take (k + 1) . index

zeroIndex :: Ty a -> Index
zeroIndex = head . head . index

shift :: Ty a -> [(Index, (Index, Index))]
shift (ListTy _ t) = zip zeroes $ zip zeroes $ tail zeroes
    where zeroes = map LIndex $ inits $ repeat $ zeroIndex t
shift           _  = []

-- The goal of inject is to find the type index containing the given
-- index with every other position set to the inner type's zero index.
-- The core precondition is that the index must be in the projection
-- of the type's index set.
inject :: Ty a -> Index -> Maybe Index
inject (PairTy (t1, t2)) (PIndex (i1, i2)) = (PIndex .) . (,) <$> inject t1 i1 <*> inject t2 i2
inject (ListTy _ t)      (LIndex is)       = LIndex <$> traverse (inject t) is
inject ty                VIndex            = Just $ zeroIndex ty
inject ty AIndex | zeroIndex ty == AIndex  = Just AIndex
inject _                 _                 = Nothing
