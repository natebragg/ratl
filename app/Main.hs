module Main where

import Control.Monad (when, forM)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans (liftIO)
import Data.Either (lefts)
import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust)
import Text.Parsec (runParser)
import System.Exit (exitFailure)
import System.IO (readFile)
import System.Environment (getArgs)

import Data.Clp.Clp
import Data.Clp.Program (LinearProgram(solve), GeneralConstraint(Leq), GeneralForm(..))
import Language.Ratl.Parser (prog, val)
import Language.Ratl.Anno (annotate)
import Language.Ratl.Basis (basis)
import Language.Ratl.Ast (Var(V))
import Language.Ratl.Eval (run)
import Language.Ratl.Elab (check)

progressive_solve :: [GeneralForm] -> [([Double], Double)]
progressive_solve gfs = fst $ foldl accum ([], []) gfs
    where accum (ss, cs') (GeneralForm dir obj cs) = (ss ++ [sol], (obj `Leq` snd sol):cs)
            where sol = solve $ GeneralForm dir obj (cs' ++ cs)

pretty_bound :: [Double] -> String
pretty_bound cs = if null bounds then show 0.0 else intercalate " + " bounds
    where bounds = reverse $ concatMap coeff $ zip [0..] $ reverse cs
          coeff (_, 0.0) = []
          coeff (0,   c) = [show c]
          coeff (1,   c) = [show c ++ "*n"]
          coeff (n,   c) = [show c ++ "*n^" ++ show n]

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
                        p' <- annotate deg_max $ basis ++ p
                        checked <- check deg_max p'
                        return (checked, p', a)
                    (e1, e2) -> do
                        liftIO $ mapM_ print $ lefts [e1] ++ lefts [e2]
                        liftIO $ exitFailure
            case result of
                Nothing ->
                    putStrLn "Typechecking failed"
                Just (programs, p, a) -> do
                    let module_programs = filter (isNothing . flip lookup basis . fst) programs
                    feasible <- forM module_programs $ \(f, program) ->  do
                       let (optimums, magnitudes) = unzip $ progressive_solve program
                       let infeasible = any null optimums
                       let bound = if infeasible
                                   then ": Analysis was infeasible"
                                   else ": " ++ pretty_bound magnitudes
                       putStrLn $ show f ++ bound
                       return $ (f, not infeasible)
                    when (fromJust $ lookup (V "main") feasible) $
                        print $ run p a
