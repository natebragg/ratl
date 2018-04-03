module Language.Ratl.Index (
  Index,
  deg,
  index,
  indexDeg,
) where

import Language.Ratl.Ty (Ty(..))

data Index = AIndex
           | LIndex [Index]
    deriving (Eq, Ord, Show)

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
deg (LIndex is) = length is + sum (map deg is)

index :: Ty a -> [[Index]]
index (NatTy)      = [[AIndex]]
index (BooleanTy)  = [[AIndex]]
index (UnitTy)     = [[AIndex]]
index (SymTy)      = [[AIndex]]
index (Tyvar _)    = [[AIndex]]
index (ListTy _ t) = do
    cs <- combos (index t)
    return $ LIndex <$> (choices =<< cs)

indexDeg :: Int -> Ty a -> [Index]
indexDeg k = concat . take (k + 1) . index
