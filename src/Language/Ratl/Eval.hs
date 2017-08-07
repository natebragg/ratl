module Language.Ratl.Eval (
    run,
) where

import Data.Maybe (fromJust)

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

plus :: Nat -> Nat -> Nat
plus Z n      = n
plus n Z      = n
plus (S n) n' = plus n (S n')

embed :: [Int] -> List
embed []     = Nil
embed (x:xs) = Cons (Nat $ toNat x) (embed xs)

run :: Prog -> [Int] -> Val
run phi args = eval [] (App (V "main") (Val $ List $ embed args))
    where eval rho (Plus e1 e2)  = let Nat n1 = eval rho e1 in let Nat n2 = eval rho e2 in Nat $ plus n1 n2
          eval rho (Head e)      = let List (Cons x  _) = eval rho e in x
          eval rho (Tail e)      = let List (Cons _ xs) = eval rho e in List xs
          eval rho (Var x)       = fromJust $ lookup x rho
          eval rho (Val v)       = v
          eval rho (App f e)     = let (Fun _ x b) = fromJust (lookup f phi) in eval ((x, eval rho e):rho) b
          eval rho (If ep et ef) =
                case eval rho ep of
                    (List Nil) -> eval rho ef
                    (Nat Z)    -> eval rho ef
                    _          -> eval rho et
