module Main where

import Control.Monad (when)
import Control.Monad.State (evalStateT)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Arrow (second)
import System.Exit (exitFailure)

import Data.Clp.Clp
import Data.Clp.StandardForm (StandardForm(..), solve)
import Language.Ratl.Ast (
    Nat(..),
    toNat,
    List(..),
    Var(..),
    Val(..),
    Fun(..),
    Ex(..),
    Prog,
    )
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
interpret optimum (Arrow q (ListTy p _) _) = (show $ optimum !! p) ++ "*n + " ++ (show $ optimum !! q)
interpret optimum (Arrow q            _ _) = show $ optimum !! q

main :: IO ()
main = do
    putStrLn $ "Using Clp version " ++ version ++ ": "
                                    ++ show (versionMajor, versionMinor, versionRelease)
    result <- runMaybeT $ flip evalStateT 0 $ do
        sum_ty  <- freshListTy NatTy >>= flip freshFunTy NatTy
        main_ty <- freshListTy NatTy >>= flip freshFunTy NatTy
        id_list_ty <- freshListTy NatTy >>= \ty -> freshFunTy ty ty
        let p = [((V "sum"), Fun sum_ty (V "vals") $
                    If (Var $ V "vals")
                        (Plus (Head $ Var $ V "vals")
                              (App (V "sum")
                                   (Tail $ Var $ V "vals")))
                        (Val $ Nat Z)),
                ((V "id_list"), Fun id_list_ty (V "args") $
                    Var $ V "args"),
                ((V "main"), Fun main_ty (V "args") $
                    App (V "sum") (Var $ V "args"))]
        checked <- check p
        return (checked, p)
    case result of
        Nothing ->
            putStrLn "Typechecking failed"
        Just ((env, constraints), p) -> do
            print env
            let optimum = solve $ StandardForm (objective env, map xlate constraints)
            when (null optimum) $ do
                putStrLn "Analysis was infeasible"
                exitFailure
            let complexities = map (second $ interpret optimum) env
            mapM_ (\(V f, complexity) -> putStrLn $ f ++ ": " ++ complexity) complexities
            print $ run p [1..10]
