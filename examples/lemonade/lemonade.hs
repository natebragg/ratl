module Lemonade where

import qualified Data.Clp.Clp as Clp
import Foreign.C.String (withCString)
import Foreign.Marshal.Array (peekArray)
import Control.Monad (forM, when)
import System.Exit (exitWith, ExitCode(ExitFailure))
import Text.Printf (printf)

main = do
    model <- Clp.newModel
    Clp.setLogLevel model 0

    status <- withCString "lemonade.mps" $ \fn -> Clp.readMps model fn True False
    when (status /= 0) $
        exitWith $ ExitFailure status

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
