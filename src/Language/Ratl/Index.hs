module Language.Ratl.Index (
  Index,
  deg,
  index,
  indexDeg,
  zeroIndex,
  shift,
  inject,
  extend,
  expand,
) where

import Control.Arrow (first, second, (***), (&&&))
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

index :: Ty -> [[Index]]
index (NatTy)      = [[AIndex]]
index (BooleanTy)  = [[AIndex]]
index (UnitTy)     = [[AIndex]]
index (SymTy)      = [[AIndex]]
index (Tyvar _)    = [[VIndex]]
index (PairTy ts)  = do
    ds <- groupBy ((==) `on` deg . PIndex . heads) $
          uncurry (diagonals `on` index) ts
    return $ ds >>= \(d1, d2) -> curry PIndex <$> d1 <*> d2
index (ListTy t) = do
    cs <- combos (index t)
    return $ LIndex <$> (choices =<< cs)

indexDeg :: Int -> Ty -> [Index]
indexDeg k = concat . take (k + 1) . index

zeroIndex :: Ty -> Index
zeroIndex = head . head . index

shift :: Ty -> [(((Index, Index), Index), (Index, Index))]
shift t@(ListTy _) = zip (zip sis pis) (zip tis lis)
    where lis = concat $ tail $ index t
          sis = map split lis
          pis = map PIndex sis
          tis = map snd sis
          split (LIndex (i:is)) = (i, LIndex is)
shift _ = []

-- The goal of inject is to find the type index containing the given
-- index with every other position set to the inner type's zero index.
-- The core precondition is that the index must be in the projection
-- of the type's index set.
inject :: Ty -> Index -> Maybe Index
inject (PairTy (t1, t2)) (PIndex (i1, i2)) = (PIndex .) . (,) <$> inject t1 i1 <*> inject t2 i2
inject (ListTy t)        (LIndex is)       = LIndex <$> traverse (inject t) is
inject ty                VIndex            = Just $ zeroIndex ty
inject ty AIndex | zeroIndex ty == AIndex  = Just AIndex
inject _                 _                 = Nothing

-- Extend takes a type and an index of equal or less width and yields an
-- index of the same degree but as wide as the type, extended at the init.
extend :: Ty -> Index -> Maybe Index
extend ty ix = revix <$> expand (revty ty) (revix ix)
    where revty (PairTy (t1, t2)) = rt t2 t1
          revty                ty = ty
          rt (PairTy (t1, t2)) t3 = rt t2 $ PairTy (t1, t3)
          rt                t1 t3 = PairTy (t1, t3)
          revix (PIndex (i1, i2)) = ri i2 i1
          revix                ix = ix
          ri (PIndex (i1, i2)) i3 = ri i2 $ PIndex (i1, i3)
          ri                i1 i3 = PIndex (i1, i3)

-- Extend takes a type and an index of equal or less width and yields an
-- index of the same degree but as wide as the type, expanded at the tail.
expand :: Ty -> Index -> Maybe Index
expand ty ix = inject ty =<< go ty ix
    where go (PairTy (t1, t2)) (PIndex (i1, i2)) = PIndex . (,) i1 <$> go t2 i2
          go (PairTy (t1, t2))                i1 = PIndex . (,) i1 <$> go t2 VIndex
          go                 _ (PIndex        _) = Nothing
          go                 _                 i = Just i
