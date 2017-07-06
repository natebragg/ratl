module Lemonade where

import qualified Data.Clp.Clp as Clp
import Foreign.Ptr (nullPtr)
import Foreign.Marshal.Array (peekArray, withArray)
import Foreign.C.String (withCString)
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
    let num_rows = 5
        --          Time Lemons  Sugar  Water Vodka
        rowLower = [ 0.0,   0.0,   0.0,   0.0,  0.0]
        rowUpper = [60.0, 200.0, 250.0, 240.0, 50.0]
    in do
    withArray rowLower $ \rowLower ->
        withArray rowUpper $ \rowUpper ->
            Clp.addRows model num_rows rowLower rowUpper nullPtr nullPtr nullPtr

    -- Regular
    let columnLower = [0.0]
        columnUpper = [_DBL_MAX]
        objective = [1.0]
        columnStarts = [0, num_rows]
        rows = [0, 1, 2, 3, 4]
        elements = [0.25, 1.0, 2.0, 2.0, 0.0]
    withArray columnLower $ \columnLower ->
        withArray columnUpper $ \columnUpper ->
            withArray objective $ \objective ->
                withArray columnStarts $ \columnStarts ->
                    withArray rows $ \rows ->
                        withArray elements $ \elements ->
                            Clp.addColumns model 1 columnLower columnUpper objective columnStarts rows elements

    -- Special
    let columnLower = [0.0]
        columnUpper = [_DBL_MAX]
        objective = [2.0]
        columnStarts = [0, num_rows]
        rows = [0, 1, 2, 3, 4]
        elements = [0.5, 1.0, 1.25, 0.6, 0.5]
    withArray columnLower $ \columnLower ->
        withArray columnUpper $ \columnUpper ->
            withArray objective $ \objective ->
                withArray columnStarts $ \columnStarts ->
                    withArray rows $ \rows ->
                        withArray elements $ \elements ->
                            Clp.addColumns model 1 columnLower columnUpper objective columnStarts rows elements

input_by_rows :: Clp.SimplexHandle -> IO ()
input_by_rows model =
    let num_cols = 2
        --              Regular   Special
        columnLower = [     0.0,      0.0]
        columnUpper = [_DBL_MAX, _DBL_MAX]
        objective   = [     1.0,      2.0]
    in do
    withArray columnLower $ \columnLower ->
        withArray columnUpper $ \columnUpper ->
            withArray objective $ \objective ->
                Clp.addColumns model num_cols columnLower columnUpper objective nullPtr nullPtr nullPtr
    -- Time
    let rowLower = [0.0]
        rowUpper = [60.0]
        rowStarts = [0, num_cols]
        columns = [0, 1]
        elements = [0.25, 0.5]
    withArray rowLower $ \rowLower ->
        withArray rowUpper $ \rowUpper ->
            withArray rowStarts $ \rowStarts ->
                withArray columns $ \columns ->
                    withArray elements $ \elements ->
                        Clp.addRows model 1 rowLower rowUpper rowStarts columns elements

    -- Lemons
    let rowLower = [0.0]
        rowUpper = [200.0]
        rowStarts = [0, num_cols]
        columns = [0, 1]
        elements = [1.0, 1.0]
    withArray rowLower $ \rowLower ->
        withArray rowUpper $ \rowUpper ->
            withArray rowStarts $ \rowStarts ->
                withArray columns $ \columns ->
                    withArray elements $ \elements ->
                        Clp.addRows model 1 rowLower rowUpper rowStarts columns elements

    -- Sugar
    let rowLower = [0.0]
        rowUpper = [250.0]
        rowStarts = [0, num_cols]
        columns = [0, 1]
        elements = [2.0, 1.25]
    withArray rowLower $ \rowLower ->
        withArray rowUpper $ \rowUpper ->
            withArray rowStarts $ \rowStarts ->
                withArray columns $ \columns ->
                    withArray elements $ \elements ->
                        Clp.addRows model 1 rowLower rowUpper rowStarts columns elements

    -- Water
    let rowLower = [0.0]
        rowUpper = [240.0]
        rowStarts = [0, num_cols]
        columns = [0, 1]
        elements = [2.0, 0.6]
    withArray rowLower $ \rowLower ->
        withArray rowUpper $ \rowUpper ->
            withArray rowStarts $ \rowStarts ->
                withArray columns $ \columns ->
                    withArray elements $ \elements ->
                        Clp.addRows model 1 rowLower rowUpper rowStarts columns elements

    -- Vodka
    let rowLower = [0.0]
        rowUpper = [50.0]
        rowStarts = [0, num_cols]
        columns = [0, 1]
        elements = [0.0, 0.5]
    withArray rowLower $ \rowLower ->
        withArray rowUpper $ \rowUpper ->
            withArray rowStarts $ \rowStarts ->
                withArray columns $ \columns ->
                    withArray elements $ \elements ->
                        Clp.addRows model 1 rowLower rowUpper rowStarts columns elements

input_by_file :: Clp.SimplexHandle -> IO ()
input_by_file model = do
    status <- withCString "lemonade.mps" $ \fs -> Clp.readMps model fs True False
    when (status /= 0) $
        exitWith $ ExitFailure status

main :: IO ()
main = do
    model <- Clp.newModel
    Clp.setLogLevel model 0

    input_by_rows model

    Clp.setOptimizationDirection model (-1)

    status <- Clp.initialSolve model
    when (status /= 0) $
        exitWith $ ExitFailure status

    printf "Solution: opt %s, ppi %s, pdi %s, plr %s, dlr %s, ilr %s, abn %s\n"
        (show $ Clp.isProvenOptimal model)
        (show $ Clp.isProvenPrimalInfeasible model)
        (show $ Clp.isProvenDualInfeasible model)
        (show $ Clp.isPrimalObjectiveLimitReached model)
        (show $ Clp.isDualObjectiveLimitReached model)
        (show $ Clp.isIterationLimitReached model)
        (show $ Clp.isAbandoned model)

    let nr = Clp.getNumRows model
    pr <- peekArray nr $ Clp.getRowActivity model
    forM (enumerate pr) $ \(row, pr_row) ->
        printf "row %d, value %f\n" row pr_row

    let nc = Clp.getNumCols model
    pc <- peekArray nc $ Clp.getColSolution model
    forM (enumerate pc) $ \(col, pc_col) ->
        printf "col %d, solution %f\n" col pc_col

    Clp.deleteModel model
    where enumerate :: [a] -> [(Int, a)]
          enumerate = zip [0..]
