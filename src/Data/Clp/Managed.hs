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

    optimizationDirection,
    setOptimizationDirection,
    rowLower,
    rowUpper,
    objective,
    columnLower,
    columnUpper,
    objectiveValue,
    setLogLevel,

    initialSolve,
    dual,

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
import Foreign.C.Types (CDouble(..), CInt(..))
import Foreign.ForeignPtr (ForeignPtr, newForeignPtr, withForeignPtr)

type SimplexHandle = ForeignPtr Unmanaged.Simplex

newModel :: IO SimplexHandle
newModel = Unmanaged.newModel >>= newForeignPtr Unmanaged.deleteModel

readMps :: SimplexHandle -> CString -> Bool -> Bool -> IO CInt
readMps model filename keepNames ignoreErrors = withForeignPtr model $ \model ->
    Unmanaged.readMps model filename keepNames ignoreErrors

addRows :: SimplexHandle -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()
addRows model number rowLower rowUpper rowStarts columns elements = withForeignPtr model $ \model ->
    Unmanaged.addRows model number rowLower rowUpper rowStarts columns elements

addColumns :: SimplexHandle -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()
addColumns model number columnLower columnUpper objective columnStarts rows elements = withForeignPtr model $ \model ->
    Unmanaged.addColumns model number columnLower columnUpper objective columnStarts rows elements

optimizationDirection :: SimplexHandle -> IO CDouble
optimizationDirection model = withForeignPtr model $ \model ->
    Unmanaged.optimizationDirection model

setOptimizationDirection :: SimplexHandle -> CDouble -> IO ()
setOptimizationDirection model value = withForeignPtr model $ \model ->
    Unmanaged.setOptimizationDirection model value

rowLower :: SimplexHandle -> IO (Ptr CDouble)
rowLower model = withForeignPtr model $ \model ->
    Unmanaged.rowLower model

rowUpper :: SimplexHandle -> IO (Ptr CDouble)
rowUpper model = withForeignPtr model $ \model ->
    Unmanaged.rowUpper model

objective :: SimplexHandle -> IO (Ptr CDouble)
objective model = withForeignPtr model $ \model ->
    Unmanaged.objective model

columnLower :: SimplexHandle -> IO (Ptr CDouble)
columnLower model = withForeignPtr model $ \model ->
    Unmanaged.columnLower model

columnUpper :: SimplexHandle -> IO (Ptr CDouble)
columnUpper model = withForeignPtr model $ \model ->
    Unmanaged.columnUpper model

objectiveValue :: SimplexHandle -> IO CDouble
objectiveValue model = withForeignPtr model $ \model ->
    Unmanaged.objectiveValue model

setLogLevel :: SimplexHandle -> CInt -> IO ()
setLogLevel model value = withForeignPtr model $ \model ->
    Unmanaged.setLogLevel model value

initialSolve :: SimplexHandle -> IO CInt
initialSolve model = withForeignPtr model $ \model ->
    Unmanaged.initialSolve model

dual :: SimplexHandle -> CInt -> IO CInt
dual model pass = withForeignPtr model $ \model ->
    Unmanaged.dual model pass

getNumRows :: SimplexHandle -> IO CInt
getNumRows model = withForeignPtr model $ \model ->
    Unmanaged.getNumRows model

getNumCols :: SimplexHandle -> IO CInt
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

getRowActivity :: SimplexHandle -> IO (Ptr CDouble)
getRowActivity model = withForeignPtr model $ \model ->
    Unmanaged.getRowActivity model

getColSolution :: SimplexHandle -> IO (Ptr CDouble)
getColSolution model = withForeignPtr model $ \model ->
    Unmanaged.getColSolution model
