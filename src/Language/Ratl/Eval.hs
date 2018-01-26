module Language.Ratl.Eval (
    run,
) where

import Language.Ratl.Ast (
    Embeddable(..),
    Nat(..),
    List(..),
    Var(..),
    Val(..),
    Fun(..),
    Ex(..),
    Prog,
    lookupFun,
    )

import Data.Maybe (fromJust)
import Control.Monad (guard)

run :: Prog a -> Val -> Val
run phi args = eval [] (App (V "main") [(Val args)])
    where eval rho (Var x) = fromJust $ lookup x rho
          eval rho (Val v) = v
          eval rho (If ep et ef) =
                case eval rho ep of
                    (List Nil) -> eval rho ef
                    (Nat (N 0))-> eval rho ef
                    _          -> eval rho et
          eval rho (App x es) = fromJust $ do
                let vs = map (eval rho) es
                f <- lookupFun phi x
                case f of
                    Fun    _ x b -> Just $ eval (zip [x] vs) b
                    Native _ a f -> do guard $ a == length vs
                                       return $ f vs
