module Language.Ratl.Eval (
    run,
) where

import Data.Maybe (fromJust)

import Language.Ratl.Ast (
    Embeddable(..),
    Nat(..),
    List(..),
    Var(..),
    Val(..),
    Fun(..),
    Ex(..),
    Prog,
    )
import Language.Ratl.Basis (apply)

run :: Prog a -> Val -> Val
run phi args = eval [] (App (V "main") [(Val args)])
    where eval rho (Var x) = fromJust $ lookup x rho
          eval rho (Val v) = v
          eval rho (App (V "if") [ep, et, ef]) =
                case eval rho ep of
                    (List Nil) -> eval rho ef
                    (Nat (N 0))-> eval rho ef
                    _          -> eval rho et
          eval rho (App f es) =
                let vs = map (eval rho) es in
                case (apply f vs, fromJust (lookup f phi)) of
                    (Just v, _) -> v
                    (Nothing, Fun _ x b) -> eval (zip [x] vs) b
