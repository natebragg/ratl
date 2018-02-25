module Language.Ratl.Eval (
    run,
) where

import Language.Ratl.Ast (
    Embeddable(..),
    Var(..),
    Val(..),
    Fun(..),
    Ex(..),
    Prog,
    lookupFun,
    )

import Data.Maybe (listToMaybe, catMaybes, fromJust)
import Control.Monad (guard)

lookupFunBySCP :: [Prog a] -> Var -> Maybe (Fun a)
lookupFunBySCP p x = listToMaybe $ catMaybes $ map (flip lookupFun x) p

run :: [Prog a] -> Ex -> Val
run phi = eval []
    where eval rho (Var x) = fromJust $ lookup x rho
          eval rho (Val v) = v
          eval rho (If ep et ef) =
                if project $ eval rho ep
                then eval rho et
                else eval rho ef
          eval rho (App x es) = fromJust $ do
                let vs = map (eval rho) es
                f <- lookupFunBySCP phi x
                case f of
                    Fun    _ x b -> Just $ eval (zip [x] vs) b
                    Native _ a f -> do guard $ a == length vs
                                       return $ f vs
          eval rho (Let ds e) =
                let evalds rho'          [] = rho'
                    evalds rho' ((x, e):ds) = evalds ((x, eval rho e):rho') ds
                in eval (evalds rho ds) e
