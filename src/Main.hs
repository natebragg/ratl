module Main where

import Data.Clp.Clp

main :: IO ()
main = do
  putStrLn $ "Using Clp version " ++ version ++ ": "
                                  ++ show (versionMajor, versionMinor, versionRelease)
