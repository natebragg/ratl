module Language.Ratl.Elab (
    FunEnv,
    Cost,
    Constraint,
    freshListTy,
    freshFunTy,
    check,
) where

import Data.Maybe (isJust)
import Control.Applicative (empty)
import Control.Arrow (second)
import Control.Monad (guard)
import Control.Monad.State (StateT, get, put)
import Control.Monad.Trans.Maybe (MaybeT)

import Language.Ratl.Ty (
    Anno,
    Ty(..),
    isListTy,
    isNatTy,
    eqTy,
    FunTy(..),
    )

import Language.Ratl.Ast (
    Nat(..),
    List(..),
    Var(..),
    Val(..),
    Fun(..),
    Ex(..),
    Prog,
    )

type FunEnv = [(Var, FunTy)]

type Cost = Double

type Constraint = ([(Anno, Double)], Cost)

k_plus, k_head, k_tail, k_var, k_val, k_app, k_ifp, k_ift, k_iff :: Cost
k_plus = 1.0
k_head = 1.0
k_tail = 1.0
k_var = 1.0
k_val = 1.0
k_app = 2.0
k_ifp = 1.0
k_ift = 1.0
k_iff = 1.0

freshAnno :: Monad m => StateT Anno m Anno
freshAnno = do
    q <- get
    put (q + 1)
    return q

freshListTy :: Monad m => Ty -> StateT Anno m Ty
freshListTy tau = do
    p <- freshAnno
    return $ ListTy p tau

freshFunTy :: Monad m => Ty -> Ty -> StateT Anno m FunTy
freshFunTy tau tau' = do
    q <- freshAnno
    return $ Arrow q tau tau'

check :: Monad m => Prog -> StateT Anno (MaybeT m) (FunEnv, [Constraint])
check fs = (,) sigma <$> concat <$> mapM (elabF . snd) fs
    where sigma = map (second tyOf) fs
          tyOf (Fun ty _ _) = ty
          elabF :: Monad m => Fun -> StateT Anno (MaybeT m) [Constraint]
          elabF (Fun (Arrow qf ty ty') x e) = do
                    (ty'', q, cs) <- elabE [(x, ty)] e
                    guard $ eqTy ty' ty''
                    let equiv = case (ty', ty'') of
                                 (ListTy pf _, ListTy p _) | p /= pf -> [([(p, 1.0), (pf, -1.0)], 0.0),
                                                                         ([(p, -1.0), (pf, 1.0)], 0.0)]
                                 _                                   -> []
                    return $ ([(q, -1.0), (qf, 1.0)], 0.0):
                             ([(q, 1.0), (qf, -1.0)], 0.0):equiv ++ cs
          elabE :: Monad m => [(Var, Ty)] -> Ex -> StateT Anno (MaybeT m) (Ty, Anno, [Constraint])
          elabE gamma e = elab e
             where elab :: Monad m => Ex -> StateT Anno (MaybeT m) (Ty, Anno, [Constraint])
                   elab (Plus e1 e2)  = do (ty1, q1, cs1) <- elab e1
                                           (ty2, q2, cs2) <- elab e2
                                           q <- freshAnno
                                           guard $ isNatTy ty1 && isNatTy ty2
                                           return (NatTy, q, ([(q, 1.0), (q1, -1.0), (q2, -1.0)], k_plus):cs1 ++ cs2)
                   elab (Head e)      = do (ty, qe, cs) <- elab e
                                           guard $ isListTy ty
                                           let ListTy p ty' = ty
                                           q <- freshAnno
                                           return (ty', q, ([(q, 1.0), (p, 1.0), (qe, -1.0)], k_head):cs)
                   elab (Tail e)      = do (ty, qe, cs) <- elab e
                                           guard $ isListTy ty
                                           let ListTy p _ = ty
                                           q <- freshAnno
                                           return (ty, q, ([(q, 1.0), (p, 1.0), (qe, -1.0)], k_tail):cs)
                   elab (Var x)       = do let bnd = lookup x gamma
                                           guard $ isJust bnd
                                           let Just ty = bnd
                                           q <- freshAnno
                                           return (ty, q, [([(q, 1.0)], k_var)])
                   elab (Val v)       = do ty <- elabV v
                                           q <- freshAnno
                                           return (ty, q, [([(q, 1.0)], k_val)])
                   elab (App f e)     = do (ty, qe, cs) <- elab e
                                           q <- freshAnno
                                           let sig = lookup f sigma
                                           guard $ isJust sig
                                           let Just (Arrow qf ty' ty'') = sig
                                           guard $ eqTy ty ty'
                                           let equiv = case (ty, ty') of
                                                        (ListTy p _, ListTy pf _) | p /= pf -> [([(p, 1.0), (pf, -1.0)], 0.0),
                                                                                                ([(p, -1.0), (pf, 1.0)], 0.0)]
                                                        _                                   -> []
                                           return (ty'', q, ([(q, 1.0), (qe, -1.0), (qf, -1.0)], k_app):equiv ++ cs)
                   elab (If ep et ef) = do (typ, qp, csp) <- elab ep
                                           (tyt, qt, cst) <- elab et
                                           (tyf, qf, csf) <- elab ef
                                           q <- freshAnno
                                           guard $ eqTy tyt tyf
                                           return (tyt, q, ([(q, 1.0), (qp, -1.0), (qt, -1.0)], k_ifp + k_ift):
                                                           ([(q, 1.0), (qp, -1.0), (qf, -1.0)], k_ifp + k_iff):csp ++ cst ++ csf)
          elabV :: Monad m => Val -> StateT Anno (MaybeT m) Ty
          elabV (Nat _)  = return NatTy
          elabV (List l) = elabL l
          elabL :: Monad m => List -> StateT Anno (MaybeT m) Ty
          elabL Nil        = freshListTy MysteryTy
          elabL (Cons v l) = do
                vty <- elabV v
                lty <- elabL l
                case lty of
                    ListTy p MysteryTy      -> return $ ListTy p vty
                    ListTy _ l | eqTy l vty -> return lty
                    _                       -> empty
