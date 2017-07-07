module Data.Clp.Managed (
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

    getNumRows,
    getNumCols,

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

import Bindings.Clp.Clp (
    version,
    versionMajor,
    versionMinor,
    versionRelease,
    )
import qualified Bindings.Clp.Clp as Unmanaged

import Foreign.Ptr (Ptr)
import Foreign.C.String (CString)
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)

type SimplexHandle = ForeignPtr Unmanaged.Simplex

newModel :: IO SimplexHandle
newModel = Unmanaged.newModel >>= newForeignPtr Unmanaged.deleteModel

readMps :: SimplexHandle -> CString -> Bool -> Bool -> IO Int
readMps model filename keepNames ignoreErrors = withForeignPtr model $ \model ->
    Unmanaged.readMps model filename keepNames ignoreErrors

addRows :: SimplexHandle -> Int -> Ptr Double -> Ptr Double -> Ptr Int -> Ptr Int -> Ptr Double -> IO ()
addRows model number rowLower rowUpper rowStarts columns elements = withForeignPtr model $ \model ->
    Unmanaged.addRows model number rowLower rowUpper rowStarts columns elements

addColumns :: SimplexHandle -> Int -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Int -> Ptr Int -> Ptr Double -> IO ()
addColumns model number columnLower columnUpper objective columnStarts rows elements = withForeignPtr model $ \model ->
    Unmanaged.addColumns model number columnLower columnUpper objective columnStarts rows elements

setOptimizationDirection :: SimplexHandle -> Double -> IO ()
setOptimizationDirection model value = withForeignPtr model $ \model ->
    Unmanaged.setOptimizationDirection model value

setLogLevel :: SimplexHandle -> Int -> IO ()
setLogLevel model value = withForeignPtr model $ \model ->
    Unmanaged.setLogLevel model value

initialSolve :: SimplexHandle -> IO Int
initialSolve model = withForeignPtr model $ \model ->
    Unmanaged.initialSolve model

getNumRows :: SimplexHandle -> IO Int
getNumRows model = withForeignPtr model $ \model ->
    Unmanaged.getNumRows model

getNumCols :: SimplexHandle -> IO Int
getNumCols model = withForeignPtr model $ \model ->
    Unmanaged.getNumCols model

isAbandoned :: SimplexHandle -> IO Bool
isAbandoned model = withForeignPtr model $ \model ->
    Unmanaged.isAbandoned model

isProvenOptimal :: SimplexHandle -> IO Bool
isProvenOptimal model = withForeignPtr model $ \model ->
    Unmanaged.isProvenOptimal model

isProvenPrimalInfeasible :: SimplexHandle -> IO Bool
isProvenPrimalInfeasible model = withForeignPtr model $ \model ->
    Unmanaged.isProvenPrimalInfeasible model

isProvenDualInfeasible :: SimplexHandle -> IO Bool
isProvenDualInfeasible model = withForeignPtr model $ \model ->
    Unmanaged.isProvenDualInfeasible model

isPrimalObjectiveLimitReached :: SimplexHandle -> IO Bool
isPrimalObjectiveLimitReached model = withForeignPtr model $ \model ->
    Unmanaged.isPrimalObjectiveLimitReached model

isDualObjectiveLimitReached :: SimplexHandle -> IO Bool
isDualObjectiveLimitReached model = withForeignPtr model $ \model ->
    Unmanaged.isDualObjectiveLimitReached model

isIterationLimitReached :: SimplexHandle -> IO Bool
isIterationLimitReached model = withForeignPtr model $ \model ->
    Unmanaged.isIterationLimitReached model

getRowActivity :: SimplexHandle -> IO (Ptr Double)
getRowActivity model = withForeignPtr model $ \model ->
    Unmanaged.getRowActivity model

getColSolution :: SimplexHandle -> IO (Ptr Double)
getColSolution model = withForeignPtr model $ \model ->
    Unmanaged.getColSolution model
