module Lemonade where

import qualified Data.Clp.Clp as Clp
import Control.Monad (forM, when)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Text.Printf (printf)

_DBL_MAX = encodeFloat s e
    where v = 1.0 :: Double
          r = floatRadix v
          d = floatDigits v
          e = (snd $ floatRange v) - d
          s = r ^ d - 1

input_by_columns :: Clp.SimplexHandle -> IO ()
input_by_columns model =
    let rowBounds = [(0.0, 60.0),  -- Time
                     (0.0, 200.0), -- Lemons
                     (0.0, 250.0), -- Sugar
                     (0.0, 240.0), -- Water
                     (0.0, 50.0)]  -- Vodka
    in do
    Clp.addRows model rowBounds []

    -- Regular
    let columnBounds = [(0.0, _DBL_MAX, 1.0)]
        elements = [[0.25, 1.0, 2.0, 2.0, 0.0]]
    Clp.addColumns model columnBounds elements

    -- Special
    let columnBounds = [(0.0, _DBL_MAX, 2.0)]
        elements = [[0.5, 1.0, 1.25, 0.6, 0.5]]
    Clp.addColumns model columnBounds elements

input_by_rows :: Clp.SimplexHandle -> IO ()
input_by_rows model =
    let columnBounds = [(0.0, _DBL_MAX, 1.0), -- Regular
                        (0.0, _DBL_MAX, 2.0)] -- Special
    in do
    Clp.addColumns model columnBounds []

    -- Time
    let rowBounds = [(0.0, 60.0)]
        elements = [[0.25, 0.5]]
    Clp.addRows model rowBounds elements

    -- Lemons
    let rowBounds = [(0.0, 200.0)]
        elements = [[1.0, 1.0]]
    Clp.addRows model rowBounds elements

    -- Sugar
    let rowBounds = [(0.0, 250.0)]
        elements = [[2.0, 1.25]]
    Clp.addRows model rowBounds elements

    -- Water
    let rowBounds = [(0.0, 240.0)]
        elements = [[2.0, 0.6]]
    Clp.addRows model rowBounds elements

    -- Vodka
    let rowBounds = [(0.0, 50.0)]
        elements = [[0.0, 0.5]]
    Clp.addRows model rowBounds elements

input_by_file :: Clp.SimplexHandle -> IO ()
input_by_file model = do
    status <- Clp.readMps model "lemonade.mps" True False
    when (status /= 0) $
        exitWith $ ExitFailure status

main :: IO [()]
main = do
    model <- Clp.newModel
    Clp.setLogLevel model Clp.None

    input_by_rows model

    Clp.setOptimizationDirection model Clp.Maximize

    status <- Clp.initialSolve model
    when (status /= Clp.Optimal) $
        exitWith $ ExitFailure $ fromEnum status

    printf "Solution: opt %s, ppi %s, pdi %s, plr %s, dlr %s, ilr %s, abn %s\n"
       <$> (show <$> Clp.isProvenOptimal model)
       <*> (show <$> Clp.isProvenPrimalInfeasible model)
       <*> (show <$> Clp.isProvenDualInfeasible model)
       <*> (show <$> Clp.isPrimalObjectiveLimitReached model)
       <*> (show <$> Clp.isDualObjectiveLimitReached model)
       <*> (show <$> Clp.isIterationLimitReached model)
       <*> (show <$> Clp.isAbandoned model)
       >>= id

    pr <- Clp.getRowActivity model
    forM (enumerate pr) $ \(row, pr_row) ->
        printf "row %d, value %f\n" row pr_row

    pc <- Clp.getColSolution model
    forM (enumerate pc) $ \(col, pc_col) ->
        printf "col %d, solution %f\n" col pc_col

    where enumerate :: [a] -> [(Int, a)]
          enumerate = zip [0..]
