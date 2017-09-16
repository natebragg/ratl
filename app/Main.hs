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
import Data.Clp.Program (LinearProgram(..), LinearFunction(..), GeneralConstraint(..), GeneralForm(..))
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
    check,
    )

progressive_solve :: [GeneralForm] -> [([Double], Double)]
progressive_solve gfs = fst $ foldl accum ([], []) gfs
    where accum (ss, cs') (GeneralForm dir obj cs) = (ss ++ [sol], (obj `Leq` snd sol):cs)
            where sol = solve $ GeneralForm dir obj (cs' ++ cs)

objective :: FunEnv Anno -> Int -> LinearFunction
objective sigma degree = Sparse $ concatMap (objF . snd) sigma
    where payIf d = if degree == d then 1.0 else 0.0
          objF (Arrow q tys _) = (q, payIf 0):concatMap objTy tys
          objTy (ListTy p _) = map (second payIf) $ zip [p] [1..]
          objTy           _  = []

interpret :: [[Double]] -> FunTy Anno -> String
interpret optimums (Arrow q [ListTy p _] _) = lin_term ++ join ++ const_term
    where lin = optimums !! 0 !! p
          const = optimums !! 1 !! q
          lin_term = if lin /= 0.0 then show lin ++ "*n" else ""
          join = if lin == 0.0 || const == 0.0 then "" else if const >= 0.0 then " + " else " - "
          const_term = if const /= 0.0 || lin == 0.0 then show $ abs const else ""
interpret optimums (Arrow q            _ _) = show $ optimums !! 1 !! q

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
                    let objectives = map (objective env) [deg_max,deg_max-1..0]
                    let programs = [GeneralForm Minimize o constraints | o <- objectives]
                    let (optimums, _) = unzip $ progressive_solve programs
                    when (any null optimums) $ do
                        putStrLn "Analysis was infeasible"
                        exitFailure
                    let complexities = map (second $ interpret optimums) env
                    mapM_ (\(f, complexity) -> putStrLn $ show f ++ ": " ++ complexity) complexities
                    print $ run p a
