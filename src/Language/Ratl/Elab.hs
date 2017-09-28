{-# LANGUAGE FlexibleContexts #-}

module Language.Ratl.Elab (
    FunEnv,
    check,
) where

import Data.List (intersect, union, nub, foldl', transpose)
import Control.Applicative (empty)
import Control.Arrow (second, (&&&), (***))
import Control.Monad (guard, MonadPlus(..))
import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader, runReaderT, asks)

import Data.Clp.Clp (OptimizationDirection(Minimize))
import Data.Clp.Program (
    LinearFunction(..),
    GeneralConstraint(..),
    GeneralForm(..),
    )
import Language.Ratl.Anno (
    Anno,
    freshAnno,
    freshListTy,
    )
import Language.Ratl.Ty (
    Ty(..),
    eqTy,
    varname,
    varnum,
    FunTy(..),
    )
import Language.Ratl.Ast (
    Nat(..),
    List(..),
    Var(..),
    Val(..),
    Fun(..),
    Ex(..),
    Prog(..),
    )

type FunEnv a = [(Var, FunTy a)]
type TyvarEnv a = [(String, Ty a)]
type ProgEnv = [(Var, [GeneralForm])]

k_var, k_val, k_app, k_ifp, k_ift, k_iff :: Double
k_var = 1.0
k_val = 1.0
k_app = 2.0
k_ifp = 1.0
k_ift = 1.0
k_iff = 1.0

data Annos = Pay Anno | Exchange Anno Anno

freein :: Ty a -> [String]
freein         NatTy = []
freein (ListTy _ ty) = freein ty
freein     (Tyvar y) = [y]

solve :: TyvarEnv a -> (Ty a, Ty a) -> TyvarEnv a
solve theta (ty, ty') = compose (assoc (tysubst theta ty) (tysubst theta ty')) theta
  where assoc (ListTy _ ty) (ListTy _ ty') = assoc ty ty'
        assoc     (Tyvar x)            ty' = [(x, ty')]
        assoc             _              _ = []

compose :: TyvarEnv a -> TyvarEnv a -> TyvarEnv a
compose theta2 theta1 = map (id &&& replace) domain
  where domain  = union (map fst theta2) (map fst theta1)
        replace = tysubst theta2 . varsubst theta1

varsubst :: TyvarEnv a -> String -> Ty a
varsubst theta x = maybe (Tyvar x) id $ lookup x theta

tysubst :: TyvarEnv a -> Ty a -> Ty a
tysubst theta = subst
  where subst          NatTy = NatTy
        subst (ListTy ps ty) = ListTy ps $ subst ty
        subst      (Tyvar x) = varsubst theta x

instantiate :: [Ty a] -> FunTy a -> FunTy a
instantiate tys (Arrow q tys' ty) = Arrow q (map (tysubst varenv) tys') (tysubst varenv ty)
    where (tys'', ty') = (map (tysubst env) tys', tysubst env ty)
            where env = zip (map varname $ intersect frees bound) (map (Tyvar . varname) [next_var..])
                  next_var = 1 + (maximum $ union frees bound)
                  frees = nub $ map varnum $ concatMap freein tys
                  bound = nub $ map varnum $ concatMap freein $ ty:tys'
          varenv = foldl' solve [] $ zip tys'' tys

objective :: FunTy Anno -> Int -> LinearFunction
objective fty degree = Sparse $ objF fty
    where payIf d = if degree == d then 1.0 else 0.0
          objF (Arrow q tys _) = (q, payIf 0):concatMap objTy tys
          objTy (ListTy ps _) = map (second payIf) $ zip ps [1..]
          objTy            _  = []

transact :: (Annos, Double) -> [(Anno, Double)]
transact (Pay q, c) = [(q, c)]
transact (Exchange q' q'', c) = [(q', c), (q'', negate c)]

shift :: [a] -> ([a], [[a]])
shift ps = (p_1, transpose [ps, p_ik])
    where (p_1, p_ik) = splitAt 1 ps

hoist :: MonadPlus m => Maybe a -> m a
hoist = maybe mzero return

check :: (MonadPlus m, MonadState Anno m) => Int -> Prog Anno -> m ProgEnv
check deg_max (Prog fs) = programs
    where sigma = map (second tyOf) fs
          tyOf (Fun ty _ _) = ty
          tyOf (Native ty _ _) = ty
          programs = do (los, cs) <- (zip (map fst fs) *** concat) <$> unzip <$> mapM (elabF . snd) fs
                        return $ map (second $ \os -> [GeneralForm Minimize o cs | o <- os]) los
          elabF :: (MonadPlus m, MonadState Anno m) => Fun Anno -> m ([LinearFunction], [GeneralConstraint])
          elabF (Fun fty@(Arrow qf tys ty') x e) = do
                    (ty, q, cs) <- runReaderT (elabE e) (zip [x] tys)
                    let ty'' = tysubst (solve [] (ty, ty')) ty
                    guard $ eqTy ty' ty''
                    let equiv = case (ty', ty'') of
                                 (ListTy pfs _, ListTy ps _) -> [Sparse [(p, 1.0), (pf, -1.0)] `Eql` 0.0 | (p, pf) <- zip ps pfs, p /= pf]
                                 _                           -> []
                    let objectives = map (objective fty) [deg_max,deg_max-1..0]
                    return $ (objectives, (Sparse ((qf, 1.0):transact (q, -1.0)) `Eql` 0.0):equiv ++ cs)
          elabF (Native (Arrow qf _ _) _ _) = do
                    return ([], [])
          elabE :: (MonadPlus m, MonadState Anno m, MonadReader [(Var, Ty Anno)] m) => Ex -> m (Ty Anno, Annos, [GeneralConstraint])
          elabE (Var x)    = do ty <- hoist =<< asks (lookup x)
                                q <- freshAnno
                                return (ty, Pay q, [Sparse [(q, 1.0)] `Geq` k_var])
          elabE (Val v)    = do ty <- elabV v
                                q <- freshAnno
                                return (ty, Pay q, [Sparse [(q, 1.0)] `Geq` k_val])
          elabE (App f es) = do (tys, qs, cs) <- unzip3 <$> mapM elabE es
                                sig@(Arrow qf tys' ty'') <- instantiate tys <$> hoist (lookup f sigma)
                                q <- freshAnno
                                guard $ all (uncurry eqTy) (zip tys tys')
                                let equiv = flip concatMap (zip tys tys') $ \(ty, ty') -> case (ty, ty') of
                                             (ListTy ps _, ListTy pfs _) -> [Sparse [(p, 1.0), (pf, -1.0)] `Eql` 0.0 | (p, pf) <- zip ps pfs, p /= pf]
                                             _                           -> []
                                let ts = map (transact . flip (,) (-1.0)) qs
                                case (f, ts, tys, ty'') of
                                     (V "if", [tp, tt, tf], _, _) ->
                                            return (ty'', Pay q, (Sparse ((q, 1.0):(qf, -1.0):tp ++ tt) `Geq` (k_ifp + k_ift)):
                                                                 (Sparse ((q, 1.0):(qf, -1.0):tp ++ tf) `Geq` (k_ifp + k_iff)):equiv ++ concat cs)
                                     (V "tail", _, [ListTy ps _], ListTy rs _) ->
                                         do q' <- freshAnno
                                            return (ty'', Exchange q q', (Sparse ([(q, 1.0), (q', -1.0), (qf, -1.0)] ++ sh_p1 ++ concat ts) `Geq` k_app):equiv ++ sh_p_ik ++ concat cs)
                                          where sh_p_ik = [Sparse ((r, -1.0):map (flip (,) 1.0) sps) `Geq` 0.0 | (sps, r) <- zip p_ik rs, not $ elem r sps]
                                                sh_p1 = map (flip (,) 1.0) p_1
                                                (p_1, p_ik) = shift ps
                                     _ -> return (ty'', Pay q, (Sparse ([(q, 1.0), (qf, -1.0)] ++ concat ts) `Geq` k_app):equiv ++ concat cs)
          elabV :: (MonadPlus m, MonadState Anno m) => Val -> m (Ty Anno)
          elabV (Nat _)  = return NatTy
          elabV (List l) = elabL l
          elabL :: (MonadPlus m, MonadState Anno m) => List -> m (Ty Anno)
          elabL Nil        = freshListTy deg_max $ Tyvar "a"
          elabL (Cons v l) = do
                vty <- elabV v
                lty <- elabL l
                case lty of
                    ListTy ps (Tyvar _)     -> return $ ListTy ps vty
                    ListTy _ l | eqTy l vty -> return lty
                    _                       -> empty
