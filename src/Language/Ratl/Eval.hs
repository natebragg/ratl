{-# LANGUAGE FlexibleContexts #-}

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

import Control.Monad (when)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Except.Extra (unlessJust)
import Data.Maybe (listToMaybe, catMaybes)

lookupFunBySCP :: [Prog a] -> Var -> Maybe (Fun a)
lookupFunBySCP p x = listToMaybe $ catMaybes $ map (flip lookupFun x) p

data RuntimeError = ArityError Int Int
                  | NameError Var

instance Show RuntimeError where
    show (ArityError f a) = "Expected " ++ show f ++ " arguments, but got " ++ show a ++ " at runtime."
    show (NameError x) = "Name " ++ show x ++ " is not defined at runtime."

run :: MonadError RuntimeError m => [Prog a] -> Ex -> m Val
run phi = eval []
    where eval :: MonadError RuntimeError m => [(Var, Val)] -> Ex -> m Val
          eval rho (Var x) = unlessJust (lookup x rho) $
                                throwError $ NameError x
          eval rho (Val v) = return v
          eval rho (If ep et ef) = do
                v <- eval rho ep
                if project v
                then eval rho et
                else eval rho ef
          eval rho (App x es) = do
                vs <- traverse (eval rho) es
                f <- unlessJust (lookupFunBySCP phi x) $
                        throwError $ NameError x
                app f vs
          eval rho (Let ds e) =
                let evalds rho'          [] = return rho'
                    evalds rho' ((x, e):ds) = do
                        v <- eval rho e
                        evalds ((x, v):rho') ds
                in do rho' <- evalds rho ds
                      eval rho' e
          app :: MonadError RuntimeError m => Fun a -> [Val] -> m Val
          app (Fun    _ x b) vs = do
                let a = length [x]
                when (a /= length vs) $
                    throwError $ ArityError a (length vs)
                eval (zip [x] vs) b
          app (Native _ a f) vs = do
                when (a /= length vs) $
                    throwError $ ArityError a (length vs)
                return $ f vs
