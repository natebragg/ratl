module Data.Clp.Clp (
    version,
    versionMajor,
    versionMinor,
    versionRelease,

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

import Bindings.Clp.Clp hiding (version)
import qualified Bindings.Clp.Clp as Clp

import Foreign.C.String (peekCString)
import System.IO.Unsafe (unsafePerformIO)

version :: String
version = unsafePerformIO $ peekCString Clp.version
