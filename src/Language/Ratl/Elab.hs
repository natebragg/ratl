module Language.Ratl.Elab (
    FunEnv,
    Cost,
    Constraint,
    check,
) where

import Data.Maybe (isJust)
import Control.Applicative (empty)
import Control.Arrow (second)
import Control.Monad (guard)
import Control.Monad.State (StateT)
import Control.Monad.Trans.Maybe (MaybeT)

import Language.Ratl.Anno (
    Anno,
    freshAnno,
    freshListTy,
    )
import Language.Ratl.Ty (
    Ty(..),
    isListTy,
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

type FunEnv a = [(Var, FunTy a)]

type Cost = Double

type Constraint = ([(Anno, Double)], Cost)

k_head, k_tail, k_var, k_val, k_app, k_ifp, k_ift, k_iff :: Cost
k_head = 1.0
k_tail = 1.0
k_var = 1.0
k_val = 1.0
k_app = 2.0
k_ifp = 1.0
k_ift = 1.0
k_iff = 1.0

data Annos = Pay Anno | Exchange Anno Anno

transact :: (Annos, Double) -> [(Anno, Double)]
transact (Pay q, c) = [(q, c)]
transact (Exchange q' q'', c) = [(q', c), (q'', negate c)]

check :: Monad m => Prog Anno -> StateT Anno (MaybeT m) (FunEnv Anno, [Constraint])
check fs = (,) sigma <$> concat <$> mapM (elabF . snd) fs
    where sigma = map (second tyOf) fs
          tyOf (Fun ty _ _) = ty
          tyOf (Native ty _ _) = ty
          elabF :: Monad m => Fun Anno -> StateT Anno (MaybeT m) [Constraint]
          elabF (Fun (Arrow qf [ty] ty') x e) = do
                    (ty'', q, cs) <- elabE [(x, ty)] e
                    guard $ eqTy ty' ty''
                    let equiv = case (ty', ty'') of
                                 (ListTy pf _, ListTy p _) | p /= pf -> [([(p, 1.0), (pf, -1.0)], 0.0),
                                                                         ([(p, -1.0), (pf, 1.0)], 0.0)]
                                 _                                   -> []
                    return $ ((qf, 1.0):transact (q, -1.0), 0.0):
                             ((qf, -1.0):transact (q, 1.0), 0.0):equiv ++ cs
          elabF (Native (Arrow qf _ _) _ _) = do
                    return []
          elabE :: Monad m => [(Var, Ty Anno)] -> Ex -> StateT Anno (MaybeT m) (Ty Anno, Annos, [Constraint])
          elabE gamma e = elab e
             where elab :: Monad m => Ex -> StateT Anno (MaybeT m) (Ty Anno, Annos, [Constraint])
                   elab (Var x)       = do let bnd = lookup x gamma
                                           guard $ isJust bnd
                                           let Just ty = bnd
                                           q <- freshAnno
                                           return (ty, Pay q, [([(q, 1.0)], k_var)])
                   elab (Val v)       = do ty <- elabV v
                                           q <- freshAnno
                                           return (ty, Pay q, [([(q, 1.0)], k_val)])
                   elab (App (V "head") es) =
                                        do [(ty, qe, cs)] <- mapM elab es
                                           guard $ isListTy ty
                                           let ListTy p ty' = ty
                                           q' <- freshAnno
                                           q'' <- freshAnno
                                           return (ty', Exchange q' q'', ([(q', 1.0), (q'', -1.0), (p, 1.0)] ++ transact (qe, -1.0), k_head):cs)
                   elab (App (V "tail") es) =
                                        do [(ty, qe, cs)] <- mapM elab es
                                           guard $ isListTy ty
                                           let ListTy p _ = ty
                                           q' <- freshAnno
                                           q'' <- freshAnno
                                           return (ty, Exchange q' q'', ([(q', 1.0), (q'', -1.0), (p, 1.0)] ++ transact (qe, -1.0), k_tail):cs)
                   elab (App (V "if") es) =
                                        do [(typ, qp, csp),
                                            (tyt, qt, cst),
                                            (tyf, qf, csf)] <- mapM elab es
                                           q <- freshAnno
                                           guard $ eqTy tyt tyf
                                           return (tyt, Pay q, ((q, 1.0):transact (qp, -1.0) ++ transact (qt, -1.0), k_ifp + k_ift):
                                                               ((q, 1.0):transact (qp, -1.0) ++ transact (qf, -1.0), k_ifp + k_iff):csp ++ cst ++ csf)
                   elab (App f es)    = do (tys, qs, cs) <- unzip3 <$> mapM elab es
                                           q <- freshAnno
                                           let sig = lookup f sigma
                                           guard $ isJust sig
                                           let Just (Arrow qf tys' ty'') = sig
                                           guard $ all (uncurry eqTy) (zip tys tys')
                                           let ts = concatMap (transact . flip (,) (-1.0)) qs
                                           let equiv = flip concatMap (zip tys tys') $ \(ty, ty') -> case (ty, ty') of
                                                        (ListTy p _, ListTy pf _) | p /= pf -> [([(p, 1.0), (pf, -1.0)], 0.0),
                                                                                                ([(p, -1.0), (pf, 1.0)], 0.0)]
                                                        _                                   -> []
                                           return (ty'', Pay q, ([(q, 1.0), (qf, -1.0)] ++ ts, k_app):equiv ++ concat cs)
          elabV :: Monad m => Val -> StateT Anno (MaybeT m) (Ty Anno)
          elabV (Nat _)  = return NatTy
          elabV (List l) = elabL l
          elabL :: Monad m => List -> StateT Anno (MaybeT m) (Ty Anno)
          elabL Nil        = freshListTy MysteryTy
          elabL (Cons v l) = do
                vty <- elabV v
                lty <- elabL l
                case lty of
                    ListTy p MysteryTy      -> return $ ListTy p vty
                    ListTy _ l | eqTy l vty -> return lty
                    _                       -> empty
