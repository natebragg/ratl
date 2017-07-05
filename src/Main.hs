module Main where

import Bindings.Clp.Clp

main :: IO ()
main = do
  putStrLn $ "Using Clp version " ++ version ++ ": "
                                  ++ show (versionMajor, versionMinor, versionRelease)
