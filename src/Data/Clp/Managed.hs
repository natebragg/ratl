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
import System.IO.Unsafe (unsafePerformIO)

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

getNumRows :: SimplexHandle -> Int
getNumRows model = unsafePerformIO $ withForeignPtr model $ \model ->
    return $ Unmanaged.getNumRows model

getNumCols :: SimplexHandle -> Int
getNumCols model = unsafePerformIO $ withForeignPtr model $ \model ->
    return $ Unmanaged.getNumCols model

isAbandoned :: SimplexHandle -> Bool
isAbandoned model = unsafePerformIO $ withForeignPtr model $ \model ->
    return $ Unmanaged.isAbandoned model

isProvenOptimal :: SimplexHandle -> Bool
isProvenOptimal model = unsafePerformIO $ withForeignPtr model $ \model ->
    return $ Unmanaged.isProvenOptimal model

isProvenPrimalInfeasible :: SimplexHandle -> Bool
isProvenPrimalInfeasible model = unsafePerformIO $ withForeignPtr model $ \model ->
    return $ Unmanaged.isProvenPrimalInfeasible model

isProvenDualInfeasible :: SimplexHandle -> Bool
isProvenDualInfeasible model = unsafePerformIO $ withForeignPtr model $ \model ->
    return $ Unmanaged.isProvenDualInfeasible model

isPrimalObjectiveLimitReached :: SimplexHandle -> Bool
isPrimalObjectiveLimitReached model = unsafePerformIO $ withForeignPtr model $ \model ->
    return $ Unmanaged.isPrimalObjectiveLimitReached model

isDualObjectiveLimitReached :: SimplexHandle -> Bool
isDualObjectiveLimitReached model = unsafePerformIO $ withForeignPtr model $ \model ->
    return $ Unmanaged.isDualObjectiveLimitReached model

isIterationLimitReached :: SimplexHandle -> Bool
isIterationLimitReached model = unsafePerformIO $ withForeignPtr model $ \model ->
    return $ Unmanaged.isIterationLimitReached model

getRowActivity :: SimplexHandle -> Ptr Double
getRowActivity model = unsafePerformIO $ withForeignPtr model $ \model ->
    return $ Unmanaged.getRowActivity model

getColSolution :: SimplexHandle -> Ptr Double
getColSolution model = unsafePerformIO $ withForeignPtr model $ \model ->
    return $ Unmanaged.getColSolution model
