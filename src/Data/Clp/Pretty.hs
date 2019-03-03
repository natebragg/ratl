{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Data.Clp.Pretty (
    Grid(..),
    renderGrid,
    renderGridCompact,
    renderGridDefault,
    varnames,
    renderEqn,
    renderEqnDefault,
) where

import Data.Clp.LinearFunction (
    LinearFunction,
    coefficients,
    sparse,
    )
import Data.Clp.Program (
    GeneralConstraint(Leq, Eql, Geq),
    GeneralForm(GeneralForm),
    )
import Data.Clp.Clp (
    OptimizationDirection,
    )

import Control.Arrow (first, (&&&))
import Data.Char (ord, chr, toLower)
import Data.Foldable (toList)
import Text.PrettyPrint (
    Doc,
    render,
    empty,
    char,
    text,
    vcat,
    hcat,
    hsep,
    punctuate,
    (<>),
    (<+>),
    ($+$),
    isEmpty,
    )
import Text.Printf (printf)

-- Pretty Helpers

bar :: Doc
bar = char '|'

-- GeneralConstraint/GeneralForm Helpers

fOfGC :: GeneralConstraint -> LinearFunction
fOfGC (Leq f _) = f
fOfGC (Eql f _) = f
fOfGC (Geq f _) = f

dOfGC :: GeneralConstraint -> Double
dOfGC (Leq _ d) = d
dOfGC (Eql _ d) = d
dOfGC (Geq _ d) = d

cOfGC :: GeneralConstraint -> String
cOfGC (Leq _ _) = "≤"
cOfGC (Eql _ _) = "="
cOfGC (Geq _ _) = "≥"

lfmap :: (LinearFunction -> LinearFunction) -> GeneralConstraint -> GeneralConstraint
lfmap g (Leq f n) = Leq (g f) n
lfmap g (Eql f n) = Eql (g f) n
lfmap g (Geq f n) = Geq (g f) n

normalize :: GeneralForm -> GeneralForm
normalize (GeneralForm a o cs) = GeneralForm a (renumber o) (map (lfmap renumber) cs)
    where off = minimum $ map (minimum . fst . coefficients) $ o : map fOfGC cs
          renumber = sparse . uncurry zip . first (map $ subtract off) . coefficients

-- Double Helpers

tryRounding :: Integral a => Double -> Maybe a
tryRounding d =
    let epsilon = 1.0e-9
        i = round d
    in  if abs (d - fromIntegral i) < epsilon
        then Just i else Nothing

digits :: Integral a => a -> Int
digits i | abs i <= 9 = 1
digits i = 1 + digits (i `div` 10)

-- String Helpers

pad :: Int -> String -> String
pad n s = replicate (n - length s) ' ' ++ s

encode :: Int -> String
encode n | n < -1 = "?"
encode (-1)       = "-"
encode 0          = " "
encode 1          = "+"
encode n | n < 10 = show n
encode n | n < 36 = [chr $ ord 'A' + n - 10]
encode n | n < 62 = [chr $ ord 'a' + n - 36]
encode _          = "!"

sign :: (Ord a, Num a) => a -> String
sign n | n < 0 = "-"
sign _         = "+"

-- Grid Pretty Printer

type ColWidth = Int
type Columns = Int
type Rows = Int
data Grid = Grid Rows Columns ColWidth

class PrettyGrid a where
    size :: a -> Grid
    prettyGrid :: Grid -> a -> Doc

renderGrid :: PrettyGrid a => Grid -> a -> String
renderGrid = (render .) . prettyGrid

renderGridCompact :: PrettyGrid a => a -> String
renderGridCompact a = renderGrid (Grid r c 1) a
    where Grid r c _ = size a

renderGridDefault :: PrettyGrid a => a -> String
renderGridDefault a = renderGrid (size a) a

gridValid :: (Grid -> a) -> Grid -> a
gridValid f (Grid r c w) | r < 0 || c < 0 || w < 1 =
    error (printf "invalid grid dimensions: (%d, %d*%d)" r c w)
gridValid f g = f g

magnitude :: Grid -> Int
magnitude (Grid r c w) = r * c * w

maxGrid :: Grid -> Grid -> Grid
maxGrid (Grid r c w) (Grid r' c' w') = Grid (max r r') (max c c') (max w w')

maximumGrid :: (Foldable f, PrettyGrid a) => f a -> Grid
maximumGrid f = foldr (maxGrid . size) (Grid 0 0 1) f

instance PrettyGrid Double where
    size d = Grid 1 1 (sign + w)
        where sign = if d < 0 then 1 else 0
              decimal = 2 -- '.1' or whatever
              w = case tryRounding d of
                    Just i -> digits i
                    Nothing -> digits (truncate d) + decimal

    prettyGrid = gridValid $ \g@(Grid r c w) d ->
        let (n, m) = (magnitude (size d), magnitude g)
            s = pad m $ case tryRounding d of
                    Just 0  -> " "
                    Just i  -> if m > 1 && n <= m then printf "%d"   i else encode i
                    Nothing -> if m > 1 && n <= m then printf "%.1f" d else encode $ truncate d
            chunk [] = []
            chunk s = let (h, t) = splitAt (c * w) s in h:chunk t
        in vcat $ map text $ chunk s

instance PrettyGrid LinearFunction where
    size f = Grid r (c * length f) w
        where Grid r c w = maximumGrid f

    prettyGrid = gridValid $ \g@(Grid r c w) f ->
        let f' = take c $ toList f ++ replicate (c - length f) 0.0
            ps = map (prettyGrid (Grid r 1 w)) f'
        in  if w == 1 then hcat ps else bar <> hcat (punctuate bar ps) <> bar

instance PrettyGrid GeneralConstraint where
    size gc = (\(Grid r c w) -> Grid r (c + 1 + c') w) $ maxGrid (Grid r' 0 w') $ size $ fOfGC gc
        where g@(Grid r' c' w') = size $ dOfGC gc

    prettyGrid = gridValid $ \g@(Grid r c w) gc ->
        let pf = prettyGrid (Grid r (c - 2) w) $ fOfGC gc
            pd = prettyGrid (Grid r 1 w) $ dOfGC gc
            pc = text $ pad w $ cOfGC gc
        in  pf <> pc <> if w == 1 then pd else bar <> pd <> bar

instance PrettyGrid GeneralForm where
    size gf = case normalize gf of
        (GeneralForm _ o cs) -> Grid (r * (1 + length cs)) c w
            where Grid r c w = maxGrid (size o) $ maximumGrid cs

    prettyGrid = gridValid $ \g@(Grid r c w) gf -> case normalize gf of
        (GeneralForm a o cs) -> vcat $ prettyGrid g' o : map (prettyGrid g') cs'
            where g' = Grid 1 c w
                  cs' = take (r - 1) $ cs ++ replicate (r - 1 - length cs) (sparse [] `Eql` 0.0)

-- Equation Pretty Printer

class PrettyEqn a where
    prettyEqn :: [String] -> a -> Doc

varnames :: [String] -- the alphabet in reverse starting from n skipping o and l
varnames = map pure alphabet ++ concatMap (\n -> map (:'_':show n) alphabet) [2..]
    where alphabet = ['n', 'm'] ++ ['k', 'j'..'a'] ++ ['z', 'y'..'p']

renderEqn :: PrettyEqn a => [String] -> a -> String
renderEqn = (render .) . prettyEqn

renderEqnDefault :: PrettyEqn a => a -> String
renderEqnDefault = render . prettyEqn varnames

instance PrettyEqn Double where
    prettyEqn (x:_) d = case (tryRounding d :: Maybe Int, x) of
        ( Just 0, _:_) -> empty
        ( Just 1, _:_) -> text $ x
        ( Just i,  "") -> text $ printf "%d"      i
        (Nothing,  "") -> text $ printf "%.1f"    d
        ( Just i,   _) -> text $ printf "%d*%s"   i x
        (Nothing,   _) -> text $ printf "%.1f*%s" d x

instance PrettyEqn LinearFunction where
    prettyEqn xs f =
        let f' = map (sign &&& abs) $ toList f
        in  case filter (not . isEmpty . snd) $ reverse $ zipWith (fmap . prettyEqn . pure) xs f' of
            [] -> char '0'
            ((s, p):sps) ->
                let p' = if s == "+" then p else text s <> p
                in  hsep $ p':map (uncurry ((<+>) . text)) sps

instance PrettyEqn GeneralConstraint where
    prettyEqn xs gc =
        prettyEqn xs (fOfGC gc) <+> text (cOfGC gc) <+> prettyEqn [""] (dOfGC gc)

instance PrettyEqn GeneralForm where
    prettyEqn xs (GeneralForm a o cs) =
        text (map toLower $ show a) <+> prettyEqn xs o $+$ vcat (map (prettyEqn xs) cs)
