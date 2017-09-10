module Main where

import Control.Applicative (empty)
import Control.Monad (when)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans (liftIO)
import Control.Arrow (second)
import Data.Either (lefts)
import Data.List (intercalate, transpose)
import Text.Parsec (runParser)
import System.Exit (exitFailure)
import System.IO (readFile)
import System.Environment (getArgs)

import Data.Clp.Clp
import Data.Clp.StandardForm (StandardForm(..), solve)
import Language.Ratl.Parser (prog, val)
import Language.Ratl.Anno (Anno, annotate)
import Language.Ratl.Basis (basis)
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

objective :: FunEnv Anno -> Int -> [Double]
objective sigma degree = fst $ xlate $ (concatMap (objF . snd) sigma, 0.0)
    where payIf d = if degree == d then 1.0 else 0.0
          objF (Arrow q tys _) = (q, payIf 0):concatMap objTy tys
          objTy (ListTy p _) = map (second ((1000 *) . payIf)) $ zip [p] [1..]
          objTy           _  = []

interpret :: [Double] -> FunTy Anno -> String
interpret optimum (Arrow q [ListTy p _] _) = lin_term ++ join ++ const_term
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
            let deg_max = 1
            inp <- readFile fn
            let argstr = intercalate " " args
            let parse = runParser prog () fn inp
            let pargs = runParser val () "" argstr
            result <- runMaybeT $ flip evalStateT 0 $ do
                case (parse, pargs) of
                    (Right p, Right a) -> do
                        p' <- annotate $ basis ++ p
                        checked <- check p'
                        return (checked, p', a)
                    (e1, e2) -> do
                        liftIO $ mapM_ print $ lefts [e1] ++ lefts [e2]
                        liftIO $ exitFailure
            case result of
                Nothing ->
                    putStrLn "Typechecking failed"
                Just ((env, constraints), p, a) -> do
                    let constraints' = map xlate constraints
                    let objectives = map (objective env) [deg_max,deg_max-1..0]
                    let optimum = solve $ StandardForm (map sum $ transpose objectives, constraints')
                    when (null optimum) $ do
                        putStrLn "Analysis was infeasible"
                        exitFailure
                    let complexities = map (second $ interpret optimum) env
                    mapM_ (\(f, complexity) -> putStrLn $ show f ++ ": " ++ complexity) complexities
                    print $ run p a
