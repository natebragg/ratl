{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow (first, second)
import Control.Monad (when)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (State, get, put, runState)
import Data.Either (partitionEithers)
import Data.Foldable (toList)
import Data.List (intercalate, partition)
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
import Data.Clp.Pretty (renderGridCompact, renderGridDefault, varnames, renderEqn, renderEqnDefault)
import Language.Ratl.Reader (sexps)
import Language.Ratl.Parser (iterator, prog, ex, eol)
import Language.Ratl.Basis (prims, basis)
import Language.Ratl.Index (ContextIndex, factor)
import Language.Ratl.Val (embed)
import Language.Ratl.Ast (
    Ex(..),
    Var(V),
    Fun(..),
    Prog,
    tyOf,
    tyGet,
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
          coeff (Just ix) = intercalate "*" <$> traverse poly (factor ix)
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
          | Command String
          | Name String
          | Explicit
          | Debug String
          | DegreeMax String
    deriving Eq

data Mode = Analyze
          | Run
          | Check
    deriving (Show, Read, Ord, Bounded, Enum, Eq)

modelist :: String
modelist = intercalate ", " (map show [(Analyze)..])

data Debug = Grid
           | Compact
           | Eqns
    deriving (Show, Read, Ord, Bounded, Enum, Eq)

debuglist :: String
debuglist = intercalate ", " (map show [(Grid)..])

opts :: [OptDescr Flag]
opts = [Option ['v'] ["version"] (NoArg Version) "Version information.",
        Option ['h'] ["help"] (NoArg Help) "Print this message.",
        Option ['m'] ["mode"] (ReqArg Mode "MODE") ("One of: " ++ modelist),
        Option ['c'] ["command"] (ReqArg Command "COMMAND") ("Specify the command to execute.  Default is \"(main <args>)\"."),
        Option ['n'] ["name"] (ReqArg Name "NAME") ("Restrict analysis to supplied names.  Default is all top-level names in the file.  Special name \"initial basis\" analyzes same."),
        Option ['e'] ["explicit"] (NoArg Explicit) ("Always include explanation of bounds."),
        Option ['g'] ["debug"] (ReqArg Debug "DEBUG") ("Print debugging info; one of: " ++ debuglist),
        Option ['d'] ["degreemax"] (ReqArg DegreeMax "DEGREE") "Maximum degree of analysis."]

printUsage :: IO ()
printUsage = putStr (usageInfo header opts)
    where header = "Usage: " ++ appName ++ " [-d <n>] [-m <m>] [-c <c>] [-n <x>] [-e] [-g <g>] filename <args>\n" ++ synopsis ++ "\n"

printVersion :: IO ()
printVersion = do
    putStrLn $ appName ++ ": " ++ synopsis
    putStrLn $ "Version " ++ version
    putStrLn $ "Using Clp version " ++ Clp.version

defCmd :: [String] -> String
defCmd args = "(main " ++ unwords args ++ ")"

handleArgs :: IO (Int, Mode, Maybe Debug, Bool, String, String, [String])
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
                               debugs = [readEither m | Debug m <- os]
                               commands = [c | Command c <- os]
                               names = [x | Name x <- os]
                               explicit = Explicit `elem` os
                           in  case (partitionEithers degrees, partitionEithers modes, partitionEithers debugs) of
                             (([], ds), ([], ms), ([], gs)) ->
                                return (last $ 1:ds, last $ Run:ms, last $ Nothing:map Just gs, explicit, fn, last $ defCmd args:commands, names)
                             ((ds, _), (ms, _),  (gs, _)) -> do
                                when (not $ null ds) $
                                     putStrLn "Option degreemax requires integer argument"
                                when (not $ null ms) $
                                     putStrLn $ "Option mode requires one of: " ++ modelist
                                when (not $ null gs) $
                                     putStrLn $ "Option debug requires one of: " ++ debuglist
                                printUsage >> exitFailure

handleE (Left e) = print e >> exitFailure
handleE (Right a) = return a

handleEx m = runExceptT m >>= handleE

main :: IO ()
main = do
    (deg_max, mode, debug, explicit, fn, cmdline, names) <- handleArgs
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
    sa <- handleE $ parse (sexps <* eof) "command line" cmdline
    mainapp <- handleE $ parse (ex <* eol) "command line" $ iterator sa
    mty <- if cmdline /= defCmd [] || mode == Run
           then handleE $ Just <$> elaborate prims_basis_module mainapp
           else return Nothing
    let mod_names = mapFun fst m
        pb_names = mapFun fst prims_basis
        valid = (`elem` mod_names ++ pb_names)
        special = (== "initial basis")
        (anno_names, bad_names) = case second (partition valid . map V) $ partition special names of
            ([], ([], []))  -> (mod_names, [])
            ([], names) -> names
            (_:_, names) -> first (pb_names ++) names
        name_requested = (`elem` anno_names) . fst
    when (not $ null bad_names) $
        putStrLn $ "Warning: cannot analyze unknown name" ++
                   (if null (tail bad_names) then " " else "s ") ++
                   intercalate ", " (map (show . show) bad_names)
    when (mode == Check) $ do
        when (not $ isNothing mty) $
            putStrLn $ fn ++ ": " ++ show (tyGet $ fromJust mty)
        for (filter name_requested $ concat pty) $ \(x, f) ->
            putStrLn $ show x ++ ": " ++ show (tyOf f)
        exitSuccess
    eqns <- annotate deg_max pty $ filter (any name_requested) pty
    cl_eqns <- if isNothing mty then return [] else do
        eqns <- annotateEx deg_max pty $ fromJust mty
        return [(V fn, eqns)]
    let module_eqns = cl_eqns ++ filter name_requested eqns
    for module_eqns $ \(f, (ixs, eqns)) -> do
        let (optimums, _) = unzip $ progressive_solve eqns
            (infeas, feasible) = partition (null . snd) $ reverse $ zip eqns optimums
            best_sln = take 1 $ feasible ++ infeas
        for debug $ (for best_sln .) $ curry $ \case
            (Grid   , (e, o)) -> putStrLn $ unlines [renderGridDefault e, "", renderGridDefault o, ""]
            (Compact, (e, o)) -> putStrLn $ unlines [renderGridCompact e, "", renderGridCompact o, ""]
            (Eqns   , (e, o)) -> putStrLn $ unlines [renderEqnDefault  e, "", renderEqnDefault  o, ""]
        let infeasible = null feasible
        let bound = if infeasible
                    then ": Analysis was infeasible"
                    else ": " ++ pretty_bound explicit ixs (snd $ head feasible)
        putStrLn $ show f ++ bound
        return $ (f, not infeasible)
    when (mode == Run) $ do
        v <- handleEx $ run p mainapp
        print v
