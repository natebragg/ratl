{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow (second)
import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (State, get, put, runState)
import Data.Either (lefts, rights)
import Data.Foldable (toList)
import Data.List (intercalate)
import Data.Maybe (isNothing, fromJust)
import Data.Traversable (for)
import Data.Tuple (swap)
import Text.Parsec (parse, many1, eof)
import Text.Printf (printf)
import Text.Read (readEither)
import System.Exit (exitSuccess, exitFailure)
import System.IO (readFile)
import System.Environment (getArgs)
import System.Console.GetOpt (usageInfo, getOpt, OptDescr(..), ArgDescr(..), ArgOrder(RequireOrder))

import qualified Data.Clp.Clp as Clp (version)
import Data.Clp.Program (LinearProgram(solve), LinearFunction, GeneralConstraint(Leq), GeneralForm(..))
import Data.Clp.Pretty (varnames, renderEqn)
import Language.Ratl.Reader (sexp, sexps)
import Language.Ratl.Parser (iterator, prog, eol)
import Language.Ratl.Basis (prims, basis)
import Language.Ratl.Index (ContextIndex, factor)
import Language.Ratl.Val (embed)
import Language.Ratl.Ast (
    Ex(..),
    Var(V),
    Fun(..),
    Prog,
    tyOf,
    lookupFun,
    mapFun,
    connects,
    scSubprograms,
    )
import Language.Ratl.Eval (run)
import Language.Ratl.Elab (elaborate)
import Language.Ratl.Anno (annotate, annotateEx)
import PackageInfo (version, appName, synopsis)

progressive_solve :: [GeneralForm] -> [(LinearFunction, Double)]
progressive_solve = fst . foldl accum ([], [])
    where accum (ss, cs') (GeneralForm dir obj cs) = (ss ++ [sol], (obj `Leq` snd sol):cs')
            where sol = solve $ GeneralForm dir obj (cs' ++ cs)

pretty_bound :: Bool -> [(ContextIndex, Int)] -> LinearFunction -> String
pretty_bound explicit ixs cs = bound ++ explanation
    where (bound, (_, vs)) = flip runState (varnames, []) $ flip renderEqn cs <$> traverse coeff cixs
          cixs = fmap (flip lookup $ map swap ixs) [0..maximum $ map snd ixs]
          explanation = let n = length vs in
                if explicit && n > 0 || n > 1 then ("\n  where" ++ concat descriptions) else ""
          descriptions = map (\(ix, x) -> "\n    " ++ x ++ " is for index " ++ show ix) vs
          coeff  Nothing  = return " "
          coeff (Just ix) = traverse poly (factor ix) >>= \es -> return (intercalate "*" es)
          poly :: (ContextIndex, Int) -> State ([String], [(ContextIndex, String)]) String
          poly (ix, d) = do
                let exp = if d > 1 then '^' : show d else ""
                (xs, vs) <- get
                case lookup ix vs of
                    Just x -> return $ x ++ exp
                    Nothing -> do
                        let x = head xs
                        put (tail xs, (ix, x):vs)
                        return $ x ++ exp

callgraph :: Prog -> [Prog]
callgraph = scSubprograms . (connects =<< flatten . mapFun (second calls))
    where flatten = concatMap $ uncurry (map . (,))
          calls (Native _ _) = []
          calls (Fun _ _ e) = ecalls e
          ecalls (App f es) = f : concatMap ecalls es
          ecalls (If ep et ef) = concatMap ecalls [ep, et, ef]
          ecalls (Let ds e) = concatMap ecalls $ map snd ds ++ [e]
          ecalls _ = []

data Flag = Version
          | Help
          | Mode String
          | Explicit
          | DegreeMax String
    deriving Eq

data Mode = Analyze
          | Run
          | Check
    deriving (Show, Read, Ord, Bounded, Enum, Eq)

modelist :: String
modelist = intercalate ", " (map show [(Analyze)..])

opts :: [OptDescr Flag]
opts = [Option ['v'] ["version"] (NoArg Version) "Version information.",
        Option ['h'] ["help"] (NoArg Help) "Print this message.",
        Option ['m'] ["mode"] (ReqArg Mode "MODE") ("One of: " ++ modelist),
        Option ['e'] ["explicit"] (NoArg Explicit) ("Always include explanation of bounds."),
        Option ['d'] ["degreemax"] (ReqArg DegreeMax "DEGREE") "Maximum degree of analysis."]

printUsage :: IO ()
printUsage = putStr (usageInfo header opts)
    where header = "Usage: " ++ appName ++ " [-d <n>] [-m <m>] filename <args>\n" ++ synopsis ++ "\n"

printVersion :: IO ()
printVersion = do
    putStrLn $ appName ++ ": " ++ synopsis
    putStrLn $ "Version " ++ version
    putStrLn $ "Using Clp version " ++ Clp.version

handleArgs :: IO (Int, Mode, Bool, String, String)
handleArgs = do
    args <- getArgs
    case getOpt RequireOrder opts args of
        (os,  _, [])
            | Help    `elem` os -> printUsage >> exitSuccess
            | Version `elem` os -> printVersion >> exitSuccess
        (os, [], es) -> putStrLn "Filename required" >> putStr (concat es) >> printUsage >> exitFailure
        (os, fn:args, es)
            | not $ null es -> putStr (concat es) >> printUsage >> exitFailure
            | otherwise -> let degrees = [readEither d | DegreeMax d <- os]
                               modes = [readEither m | Mode m <- os]
                               explicit = Explicit `elem` os
                           in  case (lefts degrees, lefts modes, rights degrees, rights modes) of
                             ([], [], ds, ms) ->
                                return (last $ 1:ds, last $ Run:ms, explicit, fn, unwords args)
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
    (deg_max, mode, explicit, fn, cmdline) <- handleArgs
    when (deg_max < 0) $ do
        putStrLn "Maximum degree cannot be negative"
        exitFailure
    handleE $ elaborate mempty prims
    sb <- handleE $ parse (sexps <* eof) "initial basis" basis
    b <- handleE $ parse (prog <* eol) "initial basis" $ iterator sb
    let prims_basis = prims `mappend` b
    handleE $ elaborate prims_basis b
    inp <- readFile fn
    sm <- handleE $ parse (sexps <* eof) fn inp
    m <- handleE $ parse (prog <* eol) fn $ iterator sm
    let prims_basis_module = prims_basis `mappend` m
    let p = callgraph $ prims_basis_module
    pty <- traverse (handleE . elaborate prims_basis_module) p
    mainapp <- App (V "main") <$> if mode /= Run then return [] else
        handleE $ parse (many1 (Val <$> embed <$> sexp) <* eof) "command line" cmdline
    when (mode == Check) $ do
        for (filter (isNothing . lookupFun prims_basis . fst) $ concat pty) $ \(x, f) ->
            putStrLn $ show x ++ ": " ++ show (tyOf f)
        exitSuccess
    eqns <- annotate deg_max pty
    cl_eqns <- if mode /= Run then return [] else do
        e <- handleE $ elaborate prims_basis_module mainapp
        eqns <- annotateEx deg_max pty e
        return [(V fn, eqns)]
    let module_eqns = cl_eqns ++ filter (isNothing . lookupFun prims_basis . fst) eqns
    for module_eqns $ \(f, (ixs, eqns)) -> do
        let (optimums, _) = unzip $ progressive_solve eqns
        let feasible = filter (not . null) optimums
        let infeasible = null feasible
        let bound = if infeasible
                    then ": Analysis was infeasible"
                    else ": " ++ pretty_bound explicit ixs (last feasible)
        putStrLn $ show f ++ bound
        return $ (f, not infeasible)
    when (mode == Run) $ do
        v <- handleEx $ run p mainapp
        print v
