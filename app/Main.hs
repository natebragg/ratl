{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow (second)
import Control.Monad (when, forM)
import Control.Monad.Except (runExceptT)
import Data.Either (lefts, rights)
import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust)
import Text.Parsec (parse, eof)
import Text.Printf (printf)
import Text.Read (readEither)
import System.Exit (exitSuccess, exitFailure)
import System.IO (readFile)
import System.Environment (getArgs)
import System.Console.GetOpt (usageInfo, getOpt, OptDescr(..), ArgDescr(..), ArgOrder(RequireOrder))

import qualified Data.Clp.Clp as Clp (version)
import Data.Clp.Program (LinearProgram(solve), GeneralConstraint(Leq), GeneralForm(..))
import Language.Ratl.Reader (sexp, sexps)
import Language.Ratl.Parser (preorder, prog)
import Language.Ratl.Basis (prims, basis)
import Language.Ratl.Val (embed)
import Language.Ratl.Ast (
    Ex(..),
    Var(V),
    Fun(..),
    Prog,
    lookupFun,
    mapFun,
    connects,
    scSubprograms,
    )
import Language.Ratl.Eval (run)
import Language.Ratl.Elab (check, checkEx)
import PackageInfo (version, appName, synopsis)

progressive_solve :: [GeneralForm] -> [([Double], Double)]
progressive_solve = fst . foldl accum ([], [])
    where accum (ss, cs') (GeneralForm dir obj cs) = (ss ++ [sol], (obj `Leq` snd sol):cs')
            where sol = solve $ GeneralForm dir obj (cs' ++ cs)

pretty_bound :: [Double] -> String
pretty_bound cs = if null bounds then show 0.0 else intercalate " + " bounds
    where bounds = reverse $ concatMap coeff $ zip [0..] $ reverse cs
          coeff (_, c) | c < 1.0e-9 = []
          coeff (0, c) = [printf "%.1f" c]
          coeff (1, c) = [printf "%.1f" c ++ "*n"]
          coeff (n, c) = [printf "%.1f" c ++ "*n^" ++ show n]

callgraph :: Prog -> [Prog]
callgraph = scSubprograms . (connects =<< flatten . mapFun (second calls))
    where flatten = concatMap $ uncurry (map . (,))
          calls (Native _ _ _) = []
          calls (Fun _ _ e) = ecalls e
          ecalls (App f es) = f : concatMap ecalls es
          ecalls (If ep et ef) = concatMap ecalls [ep, et, ef]
          ecalls (Let ds e) = concatMap ecalls $ map snd ds ++ [e]
          ecalls _ = []

data Flag = Version
          | Help
          | Mode String
          | DegreeMax String
    deriving Eq

data Mode = Analyze
          | Run
    deriving (Show, Read, Ord, Bounded, Enum, Eq)

modelist :: String
modelist = intercalate ", " (map show [(Analyze)..])

opts :: [OptDescr Flag]
opts = [Option ['v'] ["version"] (NoArg Version) "Version information.",
        Option ['h'] ["help"] (NoArg Help) "Print this message.",
        Option ['m'] ["mode"] (ReqArg Mode "MODE") ("One of: " ++ modelist),
        Option ['d'] ["degreemax"] (ReqArg DegreeMax "DEGREE") "Maximum degree of analysis."]

printUsage :: IO ()
printUsage = putStr (usageInfo header opts)
    where header = "Usage: " ++ appName ++ " [-d <n>] [-m <m>] filename <args>\n" ++ synopsis ++ "\n"

printVersion :: IO ()
printVersion = do
    putStrLn $ appName ++ ": " ++ synopsis
    putStrLn $ "Version " ++ version
    putStrLn $ "Using Clp version " ++ Clp.version

handleArgs :: IO (Int, Mode, String, String)
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
                           let modes = [readEither m | Mode m <- os] in
                           case (lefts degrees, lefts modes, rights degrees, rights modes) of
                             ([], [], ds, ms) ->
                                return (last $ 1:ds, last $ Run:ms, fn, unwords args)
                             (ds, ms,  _, _) -> do
                                when (not $ null ds) $
                                     putStrLn "Option degreemax requires integer argument"
                                when (not $ null ms) $
                                     putStrLn $ "Option mode requires one of: " ++ modelist
                                printUsage >> exitFailure

handleE (Left e) = print e >> exitFailure
handleE (Right a) = return a

handleEx m = runExceptT m >>= handleE

main :: IO ()
main = do
    (deg_max, mode, fn, cmdline) <- handleArgs
    when (deg_max < 0) $ do
        putStrLn "Maximum degree cannot be negative"
        exitFailure
    inp <- readFile fn
    sb <- handleE $ parse (sexps <* eof) "initial basis" basis
    b <- handleE $ parse (prog <* eof) "initial basis" $ preorder sb
    let prims_basis = prims `mappend` b
    sm <- handleE $ parse (sexps <* eof) fn inp
    m <- handleE $ parse (prog <* eof) fn $ preorder sm
    let p = callgraph $ prims_basis `mappend` m
    a <- if mode /= Run then return $ embed (0 :: Int) else
        handleE $ fmap embed $ parse (sexp <* eof) "command line" cmdline
    eqns <- handleEx $ check deg_max p
    let mainapp a = (App (V "main") [(Val a)])
    cl_eqns <- if mode /= Run then return [] else do
        eqns <- handleEx $ checkEx deg_max p $ mainapp a
        return [(V fn, eqns)]
    let module_eqns = cl_eqns ++ filter (isNothing . lookupFun prims_basis . fst) eqns
    forM module_eqns $ \(f, (ixs, eqns)) -> do
        let (optimums, magnitudes) = unzip $ progressive_solve eqns
        let infeasible = any null optimums
        let bound = if infeasible
                    then ": Analysis was infeasible"
                    else ": " ++ pretty_bound magnitudes
        putStrLn $ show f ++ bound
        return $ (f, not infeasible)
    when (mode == Run) $ do
        v <- handleEx $ run p $ mainapp a
        print v
