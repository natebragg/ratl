module Main where

import Control.Monad (when, forM)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Trans (liftIO)
import Data.Either (lefts, rights)
import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust)
import Text.Parsec (runParser)
import Text.Read (readEither)
import System.Exit (exitSuccess, exitFailure)
import System.IO (readFile)
import System.Environment (getArgs)
import System.Console.GetOpt (usageInfo, getOpt, OptDescr(..), ArgDescr(..), ArgOrder(RequireOrder))

import qualified Data.Clp.Clp as Clp (version)
import Data.Clp.Program (LinearProgram(solve), GeneralConstraint(Leq), GeneralForm(..))
import Language.Ratl.Parser (prog, val)
import Language.Ratl.Anno (annotate)
import Language.Ratl.Basis (basis)
import Language.Ratl.Ast (Var(V))
import Language.Ratl.Eval (run)
import Language.Ratl.Elab (check)
import PackageInfo (version, appName, synopsis)

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

data Flag = Version
          | Help
          | DegreeMax String
    deriving Eq

opts :: [OptDescr Flag]
opts = [Option ['v'] ["version"] (NoArg Version) "Version information.",
        Option ['h'] ["help"] (NoArg Help) "Print this message.",
        Option ['d'] ["degreemax"] (ReqArg DegreeMax "DEGREE") "Maximum degree of analysis."]

printUsage :: IO ()
printUsage = putStr (usageInfo header opts)
    where header = "Usage: " ++ appName ++ " [-d] filename <args>\n" ++ synopsis ++ "\n"

printVersion :: IO ()
printVersion = do
    putStrLn $ appName ++ ": " ++ synopsis
    putStrLn $ "Version " ++ version
    putStrLn $ "Using Clp version " ++ Clp.version

handleArgs :: IO (Int, String, String)
handleArgs = do
    args <- getArgs
    case getOpt RequireOrder opts args of
        (os,  _, [])
            | elem Help os -> printUsage >> exitSuccess
            | elem Version os -> printVersion >> exitSuccess
        (os, [], es) -> putStrLn "Filename required" >> putStr (concat es) >> printUsage >> exitFailure
        (os, fn:args, es)
            | not $ null es -> putStr (concat es) >> printUsage >> exitFailure
            | otherwise -> let degrees = [readEither d | DegreeMax d <- os] in
                           case (lefts degrees, rights degrees) of
                             ([], degrees) -> return (last $ 1:degrees, fn, unwords args)
                             ( _, _) -> do putStrLn "Integer required for degreemax"
                                           printUsage >> exitFailure

main :: IO ()
main = do   (deg_max, fn, cmdline) <- handleArgs
            when (deg_max < 0) $ do
                putStrLn "Maximum degree cannot be negative"
                exitFailure
            when (deg_max > 1) $ do
                putStrLn "Maximum degree greater than 1 not supported"
                exitFailure
            inp <- readFile fn
            let parse = runParser prog () fn inp
            let pargs = runParser val () "" cmdline
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
