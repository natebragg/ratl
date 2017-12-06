{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ratl.Elab (
    check,
) where

import Data.List (intersect, union, nub, foldl', transpose)
import Data.Map (fromListWith, foldMapWithKey)
import Data.Maybe (listToMaybe, isJust)
import Control.Applicative (empty)
import Control.Arrow (second, (&&&))
import Control.Monad (guard, forM, MonadPlus(..), void)
import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader, runReaderT, withReaderT, ReaderT, asks)
import Control.Monad.Writer (MonadWriter, runWriterT, execWriterT, mapWriterT, WriterT, tell)

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
    Prog,
    lookupFun,
    updateFun,
    mapFun,
    travFun,
    connects,
    scSubprograms,
    )

type TyEnv a = [(Var, Ty a)]
type SharedTys a = [(Ty a, [Ty a])]
type TyvarEnv a = [(String, Ty a)]
type ProgEnv = [(Var, [GeneralForm])]

data Cost = Cost { k_var, k_val, k_ap1, k_ap2, k_ifp, k_ift, k_iff, k_ifc :: Double }

constant = Cost {
        k_var = 1.0,
        k_val = 1.0,
        k_ap1 = 1.0,
        k_ap2 = 1.0,
        k_ifp = 1.0,
        k_ift = 1.0,
        k_iff = 1.0,
        k_ifc = 1.0
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
          objF (Arrow (q, _) tys _) = (q, payIf 0):concatMap objTy tys
          objTy (ListTy ps _) = map (second payIf) $ zip ps [1..]
          objTy            _  = []

shift :: [a] -> [[a]]
shift ps = [p_1] ++ transpose [ps, p_ik]
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
                    [Sparse (map exchange (Consume p:map Supply qs)) `c` 0.0 | (p, qs) <- [(qOf a, map qOf as), (q'Of a, map q'Of as)], not $ elem p qs]
        where qOf  (Arrow (q,  _)   _   _) = q
              q'Of (Arrow (_, q')   _   _) = q'
              psOf (Arrow       _ tys   _) = tys
              rOf  (Arrow       _   _ ty') = ty'

callgraph :: Prog a -> Prog a
callgraph = connects =<< flatten . mapFun (second calls)
    where flatten = concatMap $ uncurry (map . (,))
          calls (Native _ _ _) = []
          calls (Fun _ _ e) = ecalls e
              where ecalls (App f es) = f : concatMap ecalls es
                    ecalls _ = []

data CheckE = CheckE {
        env :: TyEnv Anno,
        checkF :: CheckF
    }

data CheckF = CheckF {
        degree :: Int,
        comp :: Prog Anno,
        cost :: Cost
    }

check :: (MonadPlus m, MonadState Anno m) => Int -> Prog () -> m ProgEnv
check deg_max p_ = programs
    where p = callgraph p_
          scps = scSubprograms p
          lookupSCP :: Var -> Maybe (Prog ())
          lookupSCP x = listToMaybe $ filter (isJust . flip lookupFun x) scps
          tyOf :: Fun a -> FunTy a
          tyOf (Fun ty _ _) = ty
          tyOf (Native ty _ _) = ty
          share m = tell (empty, m)
          constrain c = tell (c, empty)
          gamma x = asks (lookup x . env)
          costof k = asks (k . cost . checkF)
          degreeof :: MonadReader CheckE m => m Int
          degreeof = asks (degree . checkF)
          annoMax a = degreeof >>= flip annotate a
          lookupThisSCP x = asks (flip lookupFun x . comp . checkF)
          programs = fmap concat $ forM scps $ \scp -> do
                    scp' <- annotate deg_max scp
                    (los, cs) <- runWriterT $ elabSCP scp'
                    return $ map (second $ \os -> [GeneralForm Minimize o cs | o <- os]) los
          elabSCP :: (MonadPlus m, MonadState Anno m) => Prog Anno -> WriterT [GeneralConstraint] m [(Var, [LinearFunction])]
          elabSCP scp = travFun (traverse (elabF scp)) scp
          elabF :: (MonadPlus m, MonadState Anno m) => Prog Anno -> Fun Anno -> WriterT [GeneralConstraint] m [LinearFunction]
          elabF scp f = do
                    mapWriterT (fmap $ second $ uncurry (++) . second (foldMapWithKey equate . fromListWith (++))) $
                        runReaderT (elabFE f) $ CheckF deg_max scp constant
                    return $ map (objective (tyOf f)) [deg_max,deg_max-1..0]
          elabFE :: (MonadPlus m, MonadState Anno m, MonadWriter ([GeneralConstraint], SharedTys Anno) m) => Fun Anno -> ReaderT CheckF m ()
          elabFE (Fun (Arrow (qf, qf') tys ty') x e) = do
                    (ty, (q, q')) <- withReaderT (CheckE (zip [x] tys)) $ elabE e
                    let ty'' = tysubst (solve [] (ty, ty')) ty
                    guard $ eqTy ty' ty''
                    constrain $ equate ty' [ty'']
                    constrain [Sparse (map exchange [Consume qf, Supply q]) `Eql` 0.0]
                    constrain [Sparse (map exchange [Supply qf', Consume q']) `Eql` 0.0]
          elabFE (Native (Arrow (qf, qf') [ListTy ps _] (ListTy rs _)) _ _) = -- hack for tail
                    constrain [Sparse (map exchange (Supply r:map Consume sps)) `Eql` 0.0 |
                               (r, sps) <- zip (qf':rs) (tail $ shift (qf:ps)), not $ elem r sps]
          elabFE (Native (Arrow (qf, qf') _ _) _ _) =
                    constrain [Sparse [exchange $ Consume qf,   exchange $ Supply qf'] `Geq` 0.0]
          elabE :: (MonadPlus m, MonadState Anno m, MonadWriter ([GeneralConstraint], SharedTys Anno) m) => Ex -> ReaderT CheckE m (Ty Anno, (Anno, Anno))
          elabE (Var x)    = do ty <- hoist =<< gamma x
                                ty' <- reannotate ty
                                share [(ty, [ty'])]
                                q  <- freshAnno
                                q' <- freshAnno
                                k <- costof k_var
                                constrain [Sparse [exchange $ Consume q, exchange $ Supply q'] `Geq` k]
                                return (ty', (q, q'))
          elabE (Val v)    = do ty <- elabV v
                                q  <- freshAnno
                                q' <- freshAnno
                                k <- costof k_val
                                constrain [Sparse [exchange $ Consume q, exchange $ Supply q'] `Geq` k]
                                return (ty, (q, q'))
          elabE (App f es) = do (tys, (qs, q's)) <- second unzip <$> unzip <$> mapM elabE es
                                sig@(Arrow (qf, qf') tys' ty'') <- lookupThisSCP f >>= \case
                                    Just asc -> do
                                            return $ instantiate tys $ tyOf asc
                                    Nothing -> do
                                        scp <- hoist (lookupSCP f)
                                        fun <- instantiatefun (map void tys) <$> hoist (lookupFun scp f)
                                        scp' <- annoMax $ updateFun scp f fun
                                        constrain =<< execWriterT (elabSCP scp')
                                        tyOf <$> hoist (lookupFun scp' f)
                                q  <- freshAnno
                                q' <- freshAnno
                                guard $ all (uncurry eqTy) (zip tys tys')
                                constrain $ concatMap (uncurry equate) $ zip tys $ map (:[]) tys'
                                case (f, qs, q's, tys') of
                                     (V "if", [qip, qit, qif], [qip', qit', qif'], [typ, tyt, tyf]) ->
                                         do [kp, kt, kf, kc] <- sequence [costof k_ifp, costof k_ift, costof k_iff, costof k_ifc]
                                            constrain $ exceed tyt [ty''] ++ exceed tyf [ty'']
                                            constrain [Sparse [exchange $ Consume q,    exchange $ Supply qip] `Geq` kp,
                                                       Sparse [exchange $ Consume qip', exchange $ Supply qf,    exchange $ Supply qit] `Geq` kt,
                                                       Sparse [exchange $ Consume qip', exchange $ Supply qf,    exchange $ Supply qif] `Geq` kf,
                                                       Sparse [exchange $ Consume qf',  exchange $ Consume qit', exchange $ Supply q']  `Geq` kc,
                                                       Sparse [exchange $ Consume qf',  exchange $ Consume qif', exchange $ Supply q']  `Geq` kc]
                                     _ -> do k1 <- costof k_ap1
                                             k2 <- costof k_ap2
                                             constrain [Sparse [exchange $ Consume q_in, exchange $ Supply q_out] `Geq` k1 |
                                                        (q_in, q_out) <- zip (q:q's) (qs ++ [qf])]
                                             constrain [Sparse (map exchange [Consume qf', Supply q']) `Geq` k2]
                                return (ty'', (q, q'))
          elabV :: (MonadPlus m, MonadState Anno m, MonadReader CheckE m) => Val -> m (Ty Anno)
          elabV (Nat _)  = return NatTy
          elabV (List l) = elabL l
          elabL :: (MonadPlus m, MonadState Anno m, MonadReader CheckE m) => List -> m (Ty Anno)
          elabL Nil        = annoMax $ ListTy [] $ Tyvar "a"
          elabL (Cons v l) = do
                vty <- elabV v
                lty <- elabL l
                case lty of
                    ListTy ps (Tyvar _)     -> return $ ListTy ps vty
                    ListTy _ l | eqTy l vty -> return lty
                    _                       -> empty
