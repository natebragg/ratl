{-# LANGUAGE FlexibleContexts #-}

module Language.Ratl.Elab (
    check,
) where

import Data.List (intersect, union, nub, foldl', transpose)
import Data.Map (fromListWith, foldMapWithKey)
import Control.Applicative (empty)
import Control.Arrow (second, (&&&))
import Control.Monad (guard, MonadPlus(..))
import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader, runReaderT, withReaderT, ReaderT, ask, asks)
import Control.Monad.Writer (MonadWriter, runWriterT, mapWriterT, WriterT, tell)

import Data.Clp.Clp (OptimizationDirection(Minimize))
import Data.Clp.Program (
    LinearFunction(..),
    GeneralConstraint(..),
    GeneralForm(..),
    )
import Language.Ratl.Anno (
    Anno,
    annotate,
    reannotate,
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

type TyEnv a = [(Var, Ty a)]
type SharedTys a = [(Ty a, [Ty a])]
type TyvarEnv a = [(String, Ty a)]
type ProgEnv = [(Var, [GeneralForm])]

data Cost = Cost { k_var, k_val, k_app, k_ifp, k_ift, k_iff :: Double }

constant = Cost {
        k_var = 1.0,
        k_val = 1.0,
        k_app = 2.0,
        k_ifp = 1.0,
        k_ift = 1.0,
        k_iff = 1.0
    }

data Resource = Consume Anno | Supply Anno

exchange :: Resource -> (Anno, Double)
exchange (Consume q) = (q, 1.0)
exchange (Supply  q) = (q, -1.0)

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

instantiatefun :: [Ty a] -> Fun a -> Fun a
instantiatefun tys (Fun ty x e) = Fun (instantiate tys ty) x e
instantiatefun tys (Native ty a f) = Native (instantiate tys ty) a f

objective :: FunTy Anno -> Int -> LinearFunction
objective fty degree = Sparse $ objF fty
    where payIf d = if degree == d then 1.0 else 0.0
          objF (Arrow q tys _) = (q, payIf 0):concatMap objTy tys
          objTy (ListTy ps _) = map (second payIf) $ zip ps [1..]
          objTy            _  = []

shift :: [a] -> ([a], [[a]])
shift ps = (p_1, transpose [ps, p_ik])
    where (p_1, p_ik) = splitAt 1 ps

hoist :: MonadPlus m => Maybe a -> m a
hoist = maybe mzero return

class Comparable a where
    relate :: (LinearFunction -> Double -> GeneralConstraint) -> a Anno -> [a Anno] -> [GeneralConstraint]
    equate :: a Anno -> [a Anno] -> [GeneralConstraint]
    exceed :: a Anno -> [a Anno] -> [GeneralConstraint]

    equate = relate Eql
    exceed = relate Geq

instance Comparable Ty where
    relate c t ts = [Sparse (map exchange (Consume p:map Supply qs)) `c` 0.0 | (p, qs) <- zip (qsOf t) $ transpose $ map qsOf ts, not $ elem p qs]
        where qsOf (ListTy qs _) = qs
              qsOf            _  = []

instance Comparable FunTy where
    relate c a as = (concatMap (uncurry (relate c)) $ zip (psOf a) (transpose $ map psOf as) ++ [(rOf a, map rOf as)]) ++
                    [Sparse (map exchange (Consume p:map Supply qs)) `c` 0.0 | (p, qs) <- [(qsOf a, map qsOf as)], not $ elem p qs]
        where qsOf (Arrow qs   _   _) = qs
              psOf (Arrow  _ tys   _) = tys
              rOf  (Arrow  _   _ ty') = ty'

check :: (MonadPlus m, MonadState Anno m) => Int -> Prog Anno -> m ProgEnv
check deg_max (Prog fs) = programs
    where tyOf (Fun ty _ _) = ty
          tyOf (Native ty _ _) = ty
          share m = tell (empty, m)
          constrain c = tell (c, empty)
          gamma x = asks (lookup x . fst)
          costof k = asks (k . snd)
          programs = do (los, cs) <- runWriterT $ zip (map fst fs) <$> mapM (elabF . snd) fs
                        return $ map (second $ \os -> [GeneralForm Minimize o cs | o <- os]) los
          elabF :: (MonadPlus m, MonadState Anno m) => Fun Anno -> WriterT [GeneralConstraint] m [LinearFunction]
          elabF f@(Fun fty _ _) = do
                    mapWriterT (fmap $ second $ uncurry (++) . second (foldMapWithKey equate . fromListWith (++))) $
                        runReaderT (elabFE f) constant
                    return $ map (objective fty) [deg_max,deg_max-1..0]
          elabF (Native _ _ _) = do
                    return []
          elabFE :: (MonadPlus m, MonadState Anno m, MonadWriter ([GeneralConstraint], SharedTys Anno) m) => Fun Anno -> ReaderT Cost m ()
          elabFE (Fun (Arrow qf tys ty') x e) = do
                    (ty, q) <- withReaderT ((,) $ zip [x] tys) $ elabE e
                    let ty'' = tysubst (solve [] (ty, ty')) ty
                    guard $ eqTy ty' ty''
                    constrain $ equate ty' [ty'']
                    constrain [Sparse (map exchange [Consume qf, q]) `Eql` 0.0]
          elabFE (Native _ _ _) = do
                    return ()
          elabE :: (MonadPlus m, MonadState Anno m, MonadWriter ([GeneralConstraint], SharedTys Anno) m) => Ex -> ReaderT (TyEnv Anno, Cost) m (Ty Anno, Resource)
          elabE (Var x)    = do ty <- hoist =<< gamma x
                                ty' <- reannotate ty
                                share [(ty, [ty'])]
                                q <- freshAnno
                                k <- costof k_var
                                constrain [Sparse [exchange $ Consume q] `Geq` k]
                                return (ty', Supply q)
          elabE (Val v)    = do ty <- elabV v
                                q <- freshAnno
                                k <- costof k_val
                                constrain [Sparse [exchange $ Consume q] `Geq` k]
                                return (ty, Supply q)
          elabE (App f es) = do (tys, qs) <- unzip <$> mapM elabE es
                                asc <- hoist (lookup f fs)
                                fun <- annotate deg_max $ instantiatefun tys asc
                                sig@(Arrow qf tys' ty'') <- annotate deg_max $ tyOf fun
                                constrain $ equate sig [tyOf asc, tyOf fun]
                                q <- freshAnno
                                guard $ all (uncurry eqTy) (zip tys tys')
                                constrain $ concatMap (uncurry equate) $ zip tys $ map (:[]) tys'
                                case (f, qs, tys', ty'') of
                                     (V "if", [qip, qit, qif], [typ, tyt, tyf], _) ->
                                         do [kp, kt, kf] <- sequence [costof k_ifp, costof k_ift, costof k_iff]
                                            constrain $ exceed tyt [ty''] ++ exceed tyf [ty'']
                                            constrain [Sparse (map exchange [Consume q, Supply qf, qip, qit]) `Geq` (kp + kt),
                                                       Sparse (map exchange [Consume q, Supply qf, qip, qif]) `Geq` (kp + kf)]
                                            return (ty'', Supply q)
                                     (V "tail", _, [ListTy ps _], ListTy rs _) ->
                                         do k <- costof k_app
                                            constrain [Sparse (map exchange (Supply q:Supply qf:sh_p1 ++ qs)) `Geq` k]
                                            constrain [Sparse (map exchange (Supply r:map Consume sps)) `Geq` 0.0 | (sps, r) <- zip p_ik rs, not $ elem r sps]
                                            return (ty'', Consume q)
                                          where sh_p1 = map Consume p_1
                                                (p_1, p_ik) = shift ps
                                     _ -> do k <- costof k_app
                                             constrain [Sparse (map exchange (Consume q:Supply qf:qs)) `Geq` k]
                                             return (ty'', Supply q)
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
