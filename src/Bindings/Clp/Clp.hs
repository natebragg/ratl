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

foreign import ccall unsafe "Clp_Version"        version        :: CString
foreign import ccall unsafe "Clp_VersionMajor"   versionMajor   :: CInt
foreign import ccall unsafe "Clp_VersionMinor"   versionMinor   :: CInt
foreign import ccall unsafe "Clp_VersionRelease" versionRelease :: CInt

data Simplex = Simplex
type SimplexHandle = Ptr Simplex

foreign import ccall unsafe "Clp_newModel"       newModel       :: IO SimplexHandle
foreign import ccall unsafe "&Clp_deleteModel"   deleteModel    :: FunPtr (SimplexHandle -> IO ())

foreign import ccall unsafe "Clp_readMps"        readMps        :: SimplexHandle -> CString -> Bool -> Bool -> IO CInt
foreign import ccall unsafe "Clp_addRows"        addRows        :: SimplexHandle -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()
foreign import ccall unsafe "Clp_addColumns"     addColumns     :: SimplexHandle -> CInt -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CInt -> Ptr CInt -> Ptr CDouble -> IO ()

foreign import ccall unsafe "Clp_optimizationDirection"         optimizationDirection         :: SimplexHandle -> IO CDouble
foreign import ccall unsafe "Clp_setOptimizationDirection"      setOptimizationDirection      :: SimplexHandle -> CDouble -> IO ()
foreign import ccall unsafe "Clp_rowLower"                      rowLower                      :: SimplexHandle -> IO (Ptr CDouble)
foreign import ccall unsafe "Clp_rowUpper"                      rowUpper                      :: SimplexHandle -> IO (Ptr CDouble)
foreign import ccall unsafe "Clp_objective"                     objective                     :: SimplexHandle -> IO (Ptr CDouble)
foreign import ccall unsafe "Clp_columnLower"                   columnLower                   :: SimplexHandle -> IO (Ptr CDouble)
foreign import ccall unsafe "Clp_columnUpper"                   columnUpper                   :: SimplexHandle -> IO (Ptr CDouble)
foreign import ccall unsafe "Clp_getNumElements"                getNumElements                :: SimplexHandle -> IO CInt
foreign import ccall unsafe "Clp_getIndices"                    getIndices                    :: SimplexHandle -> IO (Ptr CInt)
foreign import ccall unsafe "Clp_getVectorLengths"              getVectorLengths              :: SimplexHandle -> IO (Ptr CInt)
foreign import ccall unsafe "Clp_getElements"                   getElements                   :: SimplexHandle -> IO (Ptr CDouble)
foreign import ccall unsafe "Clp_objectiveValue"                objectiveValue                :: SimplexHandle -> IO CDouble
foreign import ccall unsafe "Clp_setLogLevel"                   setLogLevel                   :: SimplexHandle -> CInt -> IO ()

foreign import ccall unsafe "Clp_initialSolve"   initialSolve   :: SimplexHandle -> IO CInt
foreign import ccall unsafe "Clp_dual"           dual           :: SimplexHandle -> CInt -> IO CInt

foreign import ccall unsafe "Clp_getNumRows"     getNumRows     :: SimplexHandle -> IO CInt
foreign import ccall unsafe "Clp_getNumCols"     getNumCols     :: SimplexHandle -> IO CInt

foreign import ccall unsafe "Clp_isAbandoned"                   isAbandoned                   :: SimplexHandle -> IO Bool
foreign import ccall unsafe "Clp_isProvenOptimal"               isProvenOptimal               :: SimplexHandle -> IO Bool
foreign import ccall unsafe "Clp_isProvenPrimalInfeasible"      isProvenPrimalInfeasible      :: SimplexHandle -> IO Bool
foreign import ccall unsafe "Clp_isProvenDualInfeasible"        isProvenDualInfeasible        :: SimplexHandle -> IO Bool
foreign import ccall unsafe "Clp_isPrimalObjectiveLimitReached" isPrimalObjectiveLimitReached :: SimplexHandle -> IO Bool
foreign import ccall unsafe "Clp_isDualObjectiveLimitReached"   isDualObjectiveLimitReached   :: SimplexHandle -> IO Bool
foreign import ccall unsafe "Clp_isIterationLimitReached"       isIterationLimitReached       :: SimplexHandle -> IO Bool

foreign import ccall unsafe "Clp_getRowActivity" getRowActivity :: SimplexHandle -> IO (Ptr CDouble)
foreign import ccall unsafe "Clp_getColSolution" getColSolution :: SimplexHandle -> IO (Ptr CDouble)
