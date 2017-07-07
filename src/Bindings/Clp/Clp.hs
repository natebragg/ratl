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

import Foreign.Ptr (Ptr, FunPtr)
import Foreign.C.String (CString)

foreign import ccall "Clp_Version"        version        :: CString
foreign import ccall "Clp_VersionMajor"   versionMajor   :: Int
foreign import ccall "Clp_VersionMinor"   versionMinor   :: Int
foreign import ccall "Clp_VersionRelease" versionRelease :: Int

data Simplex = Simplex
type SimplexHandle = Ptr Simplex

foreign import ccall "Clp_newModel"       newModel       :: IO SimplexHandle
foreign import ccall "&Clp_deleteModel"   deleteModel    :: FunPtr (SimplexHandle -> IO ())

foreign import ccall "Clp_readMps"        readMps        :: SimplexHandle -> CString -> Bool -> Bool -> IO Int
foreign import ccall "Clp_addRows"        addRows        :: SimplexHandle -> Int -> Ptr Double -> Ptr Double -> Ptr Int -> Ptr Int -> Ptr Double -> IO ()
foreign import ccall "Clp_addColumns"     addColumns     :: SimplexHandle -> Int -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Int -> Ptr Int -> Ptr Double -> IO ()

foreign import ccall "Clp_setOptimizationDirection"      setOptimizationDirection      :: SimplexHandle -> Double -> IO ()
foreign import ccall "Clp_setLogLevel"                   setLogLevel                   :: SimplexHandle -> Int -> IO ()

foreign import ccall "Clp_initialSolve"   initialSolve   :: SimplexHandle -> IO Int

foreign import ccall "Clp_getNumRows"     getNumRows     :: SimplexHandle -> IO Int
foreign import ccall "Clp_getNumCols"     getNumCols     :: SimplexHandle -> IO Int

foreign import ccall "Clp_isAbandoned"                   isAbandoned                   :: SimplexHandle -> IO Bool
foreign import ccall "Clp_isProvenOptimal"               isProvenOptimal               :: SimplexHandle -> IO Bool
foreign import ccall "Clp_isProvenPrimalInfeasible"      isProvenPrimalInfeasible      :: SimplexHandle -> IO Bool
foreign import ccall "Clp_isProvenDualInfeasible"        isProvenDualInfeasible        :: SimplexHandle -> IO Bool
foreign import ccall "Clp_isPrimalObjectiveLimitReached" isPrimalObjectiveLimitReached :: SimplexHandle -> IO Bool
foreign import ccall "Clp_isDualObjectiveLimitReached"   isDualObjectiveLimitReached   :: SimplexHandle -> IO Bool
foreign import ccall "Clp_isIterationLimitReached"       isIterationLimitReached       :: SimplexHandle -> IO Bool

foreign import ccall "Clp_getRowActivity" getRowActivity :: SimplexHandle -> IO (Ptr Double)
foreign import ccall "Clp_getColSolution" getColSolution :: SimplexHandle -> IO (Ptr Double)
