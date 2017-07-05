{-# LANGUAGE ForeignFunctionInterface #-}

module Bindings.Clp.Clp (
    version,
    versionMajor,
    versionMinor,
    versionRelease,
) where

import Foreign.C.String (CString, peekCString)

foreign import ccall "Clp_Version"        version'       :: CString
version :: String
version = unsafePerformIO $ peekCString version'

foreign import ccall "Clp_VersionMajor"   versionMajor   :: Int         
foreign import ccall "Clp_VersionMinor"   versionMinor   :: Int         
foreign import ccall "Clp_VersionRelease" versionRelease :: Int         
