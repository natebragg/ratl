module Data.Clp.Clp (
    version,
    versionMajor,
    versionMinor,
    versionRelease,

    SimplexHandle,

    newModel,

    readMps,
    addRows,
    addColumns,

    setOptimizationDirection,
    setLogLevel,

    initialSolve,

    isAbandoned,
    isProvenOptimal,
    isProvenPrimalInfeasible,
    isProvenDualInfeasible,
    isPrimalObjectiveLimitReached,
    isDualObjectiveLimitReached,
    isIterationLimitReached,

    getRowActivity,
    getColSolution,
) where

import Data.Clp.Managed (
    versionMajor,
    versionMinor,
    versionRelease,

    SimplexHandle,

    newModel,

    setOptimizationDirection,
    setLogLevel,

    initialSolve,

    getNumRows,
    getNumCols,

    isAbandoned,
    isProvenOptimal,
    isProvenPrimalInfeasible,
    isProvenDualInfeasible,
    isPrimalObjectiveLimitReached,
    isDualObjectiveLimitReached,
    isIterationLimitReached,
    )
import qualified Data.Clp.Managed as Clp

import Foreign.Ptr (nullPtr)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr)
import Foreign.C.String (peekCString, withCString)
import Foreign.Marshal.Array (peekArray, withArray, withArrayLen)
import System.IO.Unsafe (unsafePerformIO)

version :: String
version = unsafePerformIO $ peekCString Clp.version

readMps :: SimplexHandle -> String -> Bool -> Bool -> IO Int
readMps model fn keepNames ignoreErrors =
    withCString fn $ \fn -> Clp.readMps model fn keepNames ignoreErrors

addRows :: SimplexHandle -> [(Double, Double)] -> [[Double]] -> IO ()
addRows model bounds elematrix =
    let (rowLower, rowUpper) = unzip bounds
    in  withArrayLen rowLower $ \num_rows rowLower ->
        withArray rowUpper $ \rowUpper ->
            let addElements = Clp.addRows model num_rows rowLower rowUpper
            in  case elematrix of
                [] -> addElements nullPtr nullPtr nullPtr
                _ -> let rowStarts = 0 : map length elematrix
                         columns = concat $ map (map fst . zip [0..]) elematrix
                         elements = concat elematrix
                     in  withArray rowStarts $ withArray columns . (withArray elements .) . addElements

addColumns :: SimplexHandle -> [(Double, Double, Double)] -> [[Double]] -> IO ()
addColumns model bounds elematrix =
    let (columnLower, columnUpper, objective) = unzip3 bounds
    in  withArrayLen columnLower $ \num_cols columnLower ->
        withArray columnUpper $ \columnUpper ->
        withArray objective $ \objective ->
            let addElements = Clp.addColumns model num_cols columnLower columnUpper objective
            in case elematrix of
                [] -> addElements nullPtr nullPtr nullPtr
                _ -> let columnStarts = 0 : map length elematrix
                         rows = concat $ map (map fst . zip [0..]) elematrix
                         elements = concat elematrix
                     in  withArray columnStarts $ withArray rows . (withArray elements .) . addElements

getRowActivity :: SimplexHandle -> [Double]
getRowActivity model = do
    let nr = Clp.getNumRows model
    unsafePerformIO $ peekArray nr $ Clp.getRowActivity model

getColSolution :: SimplexHandle -> [Double]
getColSolution model = do
    let nc = Clp.getNumCols model
    unsafePerformIO $ peekArray nc $ Clp.getColSolution model
