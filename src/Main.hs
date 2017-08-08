module Main where

import Control.Applicative (empty)
import Control.Monad (when)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans (liftIO)
import Control.Arrow (second)
import Data.Either (lefts)
import Data.List (intercalate)
import Text.Parsec (runParserT)
import System.Exit (exitFailure)
import System.IO (readFile)
import System.Environment (getArgs)

import Data.Clp.Clp
import Data.Clp.StandardForm (StandardForm(..), solve)
import Language.Ratl.Parser (prog, val)
import Language.Ratl.Ty (
    Ty(..),
    FunTy(..),
    )
import Language.Ratl.Eval (run)
import Language.Ratl.Elab (
    FunEnv,
    Cost,
    Constraint,
    freshListTy,
    freshFunTy,
    check,
    )


xlate :: Constraint -> ([Double], Cost)
xlate (cs, c) = (xl 0 (replicate (1 + (maximum $ map fst cs)) 0.0) cs, c)
    where xl _      []           _ = []
          xl n (r:row)          [] = r:xl (n + 1) row cs
          xl n (r:row) ((q, v):cs) = xl n ((if q == n then r + v else r):row) cs

objective :: FunEnv -> [Double]
objective sigma = fst $ xlate $ (obj sigma, 0.0)
    where obj [] = []
          obj ((_, Arrow q (ListTy p _) _):sigma) = (q, 1.0):(p, 1000):obj sigma
          obj ((_, Arrow q            _ _):sigma) = (q, 1.0):obj sigma

interpret :: [Double] -> FunTy -> String
interpret optimum (Arrow q (ListTy p _) _) = let lin = optimum !! p in (if lin /= 0.0 then show lin ++ "*n + " else "") ++ (show $ optimum !! q)
interpret optimum (Arrow q            _ _) = show $ optimum !! q

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ "Ratl: Resource-aware toy language"
    putStrLn $ "Using Clp version " ++ version ++ ": "
                                    ++ show (versionMajor, versionMinor, versionRelease)
    case args of
        [] -> putStrLn $ "filename required"
        (fn:args) -> do
            inp <- readFile fn
            let argstr = intercalate " " args
            result <- runMaybeT $ flip evalStateT 0 $ do
                parse <- runParserT prog () fn inp
                pargs <- runParserT val () "" argstr
                case (parse, pargs) of
                    (Right p, Right a) -> do
                        checked <- check p
                        return (checked, p, a)
                    (e1, e2) -> do
                        liftIO $ mapM_ print $ lefts [e1] ++ lefts [e2]
                        liftIO $ exitFailure
            case result of
                Nothing ->
                    putStrLn "Typechecking failed"
                Just ((env, constraints), p, a) -> do
                    let optimum = solve $ StandardForm (objective env, map xlate constraints)
                    when (null optimum) $ do
                        putStrLn "Analysis was infeasible"
                        exitFailure
                    let complexities = map (second $ interpret optimum) env
                    mapM_ (\(f, complexity) -> putStrLn $ show f ++ ": " ++ complexity) complexities
                    print $ run p a
