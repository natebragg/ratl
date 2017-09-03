module Main where

import Control.Applicative (empty)
import Control.Monad (when)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans (liftIO)
import Control.Arrow (second)
import Data.Either (lefts)
import Data.List (intercalate)
import Text.Parsec (runParser)
import System.Exit (exitFailure)
import System.IO (readFile)
import System.Environment (getArgs)

import Data.Clp.Clp
import Data.Clp.StandardForm (StandardForm(..), solve)
import Language.Ratl.Parser (prog, val)
import Language.Ratl.Anno (Anno, annotate)
import Language.Ratl.Ty (
    Ty(..),
    FunTy(..),
    )
import Language.Ratl.Eval (run)
import Language.Ratl.Elab (
    FunEnv,
    Cost,
    Constraint,
    check,
    )


xlate :: Constraint -> ([Double], Cost)
xlate (cs, c) = (map negate $ xl 0 (replicate (1 + (maximum $ map fst cs)) 0.0) cs, negate c)
    where xl _      []           _ = []
          xl n (r:row)          [] = r:xl (n + 1) row cs
          xl n (r:row) ((q, v):cs) = xl n ((if q == n then r + v else r):row) cs

objective :: FunEnv Anno -> [Double]
objective sigma = fst $ xlate $ (obj sigma, 0.0)
    where obj [] = []
          obj ((_, Arrow q (ListTy p _) _):sigma) = (q, 1.0):(p, 1000):obj sigma
          obj ((_, Arrow q            _ _):sigma) = (q, 1.0):obj sigma

interpret :: [Double] -> FunTy Anno -> String
interpret optimum (Arrow q (ListTy p _) _) = lin_term ++ join ++ const_term
    where lin = optimum !! p
          const = optimum !! q
          lin_term = if lin /= 0.0 then show lin ++ "*n" else ""
          join = if lin == 0.0 || const == 0.0 then "" else if const >= 0.0 then " + " else " - "
          const_term = if const /= 0.0 || lin == 0.0 then show $ abs const else ""
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
            let parse = runParser prog () fn inp
            let pargs = runParser val () "" argstr
            result <- runMaybeT $ flip evalStateT 0 $ do
                case (parse, pargs) of
                    (Right p, Right a) -> do
                        p' <- annotate p
                        checked <- check p'
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
