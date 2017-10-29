{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Clp.Clp (
    version,
    versionMajor,
    versionMinor,
    versionRelease,

    Simplex,
    SimplexHandle,

    newModel,
    deleteModel,

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
    getNumElements,
    getIndices,
    getVectorLengths,
    getElements,
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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.String (CString)
import Foreign.C.Types (CDouble(..), CInt(..))

foreign import ccall "Clp_Version"        version        :: CString
foreign import ccall "Clp_VersionMajor"   versionMajor   :: CInt
foreign import ccall "Clp_VersionMinor"   versionMinor   :: CInt
foreign import ccall "Clp_VersionRelease" versionRelease :: CInt

data Simplex = Simplex
type SimplexHandle = Ptr Simplex

foreign import ccall "Clp_newModel"       newModel       :: IO SimplexHandle
foreign import ccall "&Clp_deleteModel"   deleteModel    :: FunPtr (SimplexHandle -> IO ())

foreign import ccall "Clp_readMps"        readMps        :: SimplexHandle -> CString -> Bool -> Bool -> IO CInt
foreign import ccall "Clp_addRows"        addRows        :: SimplexHandle -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()
foreign import ccall "Clp_addColumns"     addColumns     :: SimplexHandle -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()

foreign import ccall "Clp_optimizationDirection"         optimizationDirection         :: SimplexHandle -> IO CDouble
foreign import ccall "Clp_setOptimizationDirection"      setOptimizationDirection      :: SimplexHandle -> CDouble -> IO ()
foreign import ccall "Clp_rowLower"                      rowLower                      :: SimplexHandle -> IO (Ptr CDouble)
foreign import ccall "Clp_rowUpper"                      rowUpper                      :: SimplexHandle -> IO (Ptr CDouble)
foreign import ccall "Clp_objective"                     objective                     :: SimplexHandle -> IO (Ptr CDouble)
foreign import ccall "Clp_columnLower"                   columnLower                   :: SimplexHandle -> IO (Ptr CDouble)
foreign import ccall "Clp_columnUpper"                   columnUpper                   :: SimplexHandle -> IO (Ptr CDouble)
foreign import ccall "Clp_getNumElements"                getNumElements                :: SimplexHandle -> IO CInt
foreign import ccall "Clp_getIndices"                    getIndices                    :: SimplexHandle -> IO (Ptr CInt)
foreign import ccall "Clp_getVectorLengths"              getVectorLengths              :: SimplexHandle -> IO (Ptr CInt)
foreign import ccall "Clp_getElements"                   getElements                   :: SimplexHandle -> IO (Ptr CDouble)
foreign import ccall "Clp_objectiveValue"                objectiveValue                :: SimplexHandle -> IO CDouble
foreign import ccall "Clp_setLogLevel"                   setLogLevel                   :: SimplexHandle -> CInt -> IO ()

foreign import ccall "Clp_initialSolve"   initialSolve   :: SimplexHandle -> IO CInt
foreign import ccall "Clp_dual"           dual           :: SimplexHandle -> CInt -> IO CInt

foreign import ccall "Clp_getNumRows"     getNumRows     :: SimplexHandle -> IO CInt
foreign import ccall "Clp_getNumCols"     getNumCols     :: SimplexHandle -> IO CInt

foreign import ccall "Clp_isAbandoned"                   isAbandoned                   :: SimplexHandle -> IO Bool
foreign import ccall "Clp_isProvenOptimal"               isProvenOptimal               :: SimplexHandle -> IO Bool
foreign import ccall "Clp_isProvenPrimalInfeasible"      isProvenPrimalInfeasible      :: SimplexHandle -> IO Bool
foreign import ccall "Clp_isProvenDualInfeasible"        isProvenDualInfeasible        :: SimplexHandle -> IO Bool
foreign import ccall "Clp_isPrimalObjectiveLimitReached" isPrimalObjectiveLimitReached :: SimplexHandle -> IO Bool
foreign import ccall "Clp_isDualObjectiveLimitReached"   isDualObjectiveLimitReached   :: SimplexHandle -> IO Bool
foreign import ccall "Clp_isIterationLimitReached"       isIterationLimitReached       :: SimplexHandle -> IO Bool

foreign import ccall "Clp_getRowActivity" getRowActivity :: SimplexHandle -> IO (Ptr CDouble)
foreign import ccall "Clp_getColSolution" getColSolution :: SimplexHandle -> IO (Ptr CDouble)
