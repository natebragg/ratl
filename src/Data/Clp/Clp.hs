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

    OptimizationDirection(..),
    setOptimizationDirection,
    objectiveValue,
    LogLevel(..),
    setLogLevel,

    Status(..),
    initialSolve,
    Pass(..),
    dual,

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

    objectiveValue,

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
import Foreign.ForeignPtr (ForeignPtr)
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
                _ -> let rowStarts = scanl (+) 0 $ map length elematrix
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
                _ -> let columnStarts = scanl (+) 0 $ map length elematrix
                         rows = concat $ map (map fst . zip [0..]) elematrix
                         elements = concat elematrix
                     in  withArray columnStarts $ withArray rows . (withArray elements .) . addElements

data OptimizationDirection = Maximize | Ignore | Minimize
    deriving (Eq, Ord, Enum)

setOptimizationDirection :: SimplexHandle -> OptimizationDirection -> IO ()
setOptimizationDirection model dir = Clp.setOptimizationDirection model $ (fromIntegral $ fromEnum dir) - 1.0

data LogLevel = None | Final | Factorizations | PlusABitMore | Verbose
    deriving (Eq, Ord, Enum)

setLogLevel :: SimplexHandle -> LogLevel -> IO ()
setLogLevel model level = Clp.setLogLevel model $ fromEnum level

data Status = Event3
            | Event2
            | Unknown
            | Optimal
            | PrimalInfeasible
            | DualInfeasible
            | Stopped
            | Errors
            | UserStopped
    deriving (Eq, Ord, Enum)

initialSolve :: SimplexHandle -> IO Status
initialSolve model = fmap (toEnum . (3 +)) $ Clp.initialSolve model

data Pass = Initial
          | ValuesPass
          | Cleanup
    deriving (Eq, Ord, Enum)

dual :: SimplexHandle -> Pass -> IO Status
dual model pass = fmap (toEnum . (3 +)) $ Clp.dual model $ fromEnum pass

getRowActivity :: SimplexHandle -> IO [Double]
getRowActivity model = do
    nr <- Clp.getNumRows model
    rs <- Clp.getRowActivity model
    peekArray nr rs

getColSolution :: SimplexHandle -> IO [Double]
getColSolution model = do
    nc <- Clp.getNumCols model
    cs <- Clp.getColSolution model
    peekArray nc cs
