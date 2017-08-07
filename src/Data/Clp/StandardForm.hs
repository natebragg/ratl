module Data.Clp.StandardForm (
    StandardForm(..),
    solve,
) where

import qualified Data.Clp.Clp as Clp

import System.IO.Unsafe (unsafePerformIO)

_DBL_MAX = encodeFloat s e
    where v = 1.0 :: Double
          r = floatRadix v
          d = floatDigits v
          e = (snd $ floatRange v) - d
          s = r ^ d - 1

newtype StandardForm = StandardForm ([Double], [([Double], Double)])

solve :: StandardForm -> [Double]
solve (StandardForm (objective, constraints)) =
    let (elements, bounds) = unzip constraints
        row_length = maximum $ map length elements
        columnBounds = [(0.0, _DBL_MAX, obj) | obj <- objective] ++
                       replicate (row_length - length objective) (0.0, _DBL_MAX, 0.0)
        rowBounds = [(0.0, bound) | bound <- bounds]
    in  unsafePerformIO $ do
    model <- Clp.newModel
    Clp.setLogLevel model Clp.None
    Clp.setOptimizationDirection model Clp.Maximize
    Clp.addColumns model columnBounds []
    Clp.addRows model rowBounds elements
    status <- Clp.initialSolve model
    case status of
        Clp.Optimal -> Clp.getColSolution model
        _ -> return []
