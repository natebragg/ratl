{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ratl.Anno (
    annotate,
    annotateEx
) where

import Data.List (union, find)
import Data.Map (foldMapWithKey, traverseWithKey, elems, fromList, toList, mapKeys)
import Data.Map.Monoidal (MonoidalMap(..), singleton, keys)
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Foldable (traverse_, foldrM)
import Data.Traversable (for)
import Control.Arrow (first, second)
import Control.Monad (zipWithM)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Except.Extra (unlessJust)
import Control.Monad.State (MonadState, evalStateT, get, put)
import Control.Monad.Reader (MonadReader, runReaderT, withReaderT, mapReaderT, ReaderT, asks)
import Control.Monad.Writer (MonadWriter, runWriterT, execWriterT, mapWriterT, WriterT, tell)

import Data.Clp.Clp (OptimizationDirection(Minimize))
import Data.Clp.LinearFunction (LinearFunction, sparse)
import Data.Clp.Program (
    GeneralConstraint(..),
    GeneralForm(..),
    Objective,
    )
import Language.Ratl.Index (
    Index,
    poly,
    deg,
    index,
    indexDeg,
    zeroIndex,
    shift,
    projectionsDeg,
    )
import Language.Ratl.Ty (
    Ty(..),
    unpair,
    FunTy(..),
    )
import Language.Ratl.Val (
    List(..),
    Val(..),
    )
import Language.Ratl.Ast (
    Var(..),
    Fun(..),
    Ex(..),
    ExTy(..),
    Prog,
    tyOf,
    tyGet,
    lookupFun,
    updateFun,
    mapFun,
    travFun,
    )
import qualified Language.Ratl.Elab as Elab (
    Env(..),
    elab,
    instantiate,
    )


type Anno = Int

type IxEnv a = [(Index, a)]
type VarEnv a = [(Var, IxEnv a)]
type FunEnv a = [(Var, (Fun, (IxEnv a, IxEnv a)))]
type SharedTys a = MonoidalMap (IxEnv a) [IxEnv a]
type Eqn = (IxEnv Anno, [GeneralForm])
type EqnEnv = [(Var, Eqn)]

data Cost = Cost { k_var,
                   k_val,
                   k_ap1, k_ap2,
                   k_ifp, k_ift, k_iff, k_ifc,
                   k_lt1, k_lt2 :: Double }
    deriving Eq

constant = Cost {
        k_var = 1.0,
        k_val = 1.0,
        k_ap1 = 1.0,
        k_ap2 = 1.0,
        k_ifp = 1.0,
        k_ift = 1.0,
        k_iff = 1.0,
        k_ifc = 1.0,
        k_lt1 = 1.0,
        k_lt2 = 1.0
    }

zero = Cost {
        k_var = 0.0,
        k_val = 0.0,
        k_ap1 = 0.0,
        k_ap2 = 0.0,
        k_ifp = 0.0,
        k_ift = 0.0,
        k_iff = 0.0,
        k_ifc = 0.0,
        k_lt1 = 0.0,
        k_lt2 = 0.0
    }

data Resource = Consume Anno | Supply Anno

exchange :: Resource -> (Anno, Double)
exchange (Consume q) = (q, 1.0)
exchange (Supply  q) = (q, -1.0)

data CheckE = CheckE {
        env :: VarEnv Anno,
        checkF :: CheckF
    }

data CheckF = CheckF {
        degree :: Int,
        scps :: [Prog],
        comp :: FunEnv Anno,
        cost :: Cost
    }

data AnnoError = ProjectionError Ty Index

instance Show AnnoError where
    show (ProjectionError t i) = "Index " ++ show i ++ " does not appear to be a projection of type " ++ show t ++ "."

-- General Helpers

updateBy :: (a -> Bool) -> b -> [(a, b)] -> [(a, b)]
updateBy f b [] = error "BUG: update failure"
updateBy f b ((a,_):abs) | f a = (a,b):abs
updateBy f b (ab:abs) = ab:updateBy f b abs

update :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
update a = updateBy (a ==)

assoc :: (a, (b, c)) -> ((a, b), c)
assoc (a, (b, c)) = ((a, b), c)

zipR :: [a] -> [b] -> ([(a,b)], ([a], [b]))
zipR     []     bs = ([], ([], bs))
zipR     as     [] = ([], (as, []))
zipR (a:as) (b:bs) = first ((a,b) :) $ zipR as bs

-- IxEnv Helpers

isZero :: Index -> Bool
isZero = (0 ==) . deg

lookupZero :: IxEnv a -> a
lookupZero = snd . fromJust . find (isZero . fst)

deleteZero :: IxEnv a -> IxEnv a
deleteZero ixs = filter (not . isZero . fst) ixs

updateZero :: a -> IxEnv a -> IxEnv a
updateZero = updateBy isZero

-- Annotation Helpers

reannotate :: (Traversable t, MonadState Anno m) => t a -> m (t Anno)
reannotate = traverse $ const freshAnno

freshAnno :: MonadState Anno m => m Anno
freshAnno = do
    q <- get
    put (q + 1)
    return q

freshIxEnv :: MonadState Anno m => Int -> Ty -> m (IxEnv Anno)
freshIxEnv k t = traverse (reannotate . flip (,) ()) $ indexDeg k t

freshBounds :: (MonadReader CheckE m, MonadState Anno m) => Ty -> m (IxEnv Anno, IxEnv Anno)
freshBounds t = do
    k <- degreeof
    q  <- freshIxEnv k t
    q' <- rezero q
    return (q, q')

freshFunBounds :: MonadState Anno m => Int -> Fun -> m (IxEnv Anno, IxEnv Anno)
freshFunBounds k fun = do
    let Arrow t t' = tyOf fun
    q  <- freshIxEnv k t
    q' <- freshIxEnv k t'
    return (q, q')

rezero :: MonadState Anno m => IxEnv Anno -> m (IxEnv Anno)
rezero qs = do
    q_0' <- freshAnno
    return $ updateZero q_0' qs

-- Constraint Helpers

relate :: (LinearFunction -> Double -> GeneralConstraint) -> IxEnv Anno -> [IxEnv Anno] -> [GeneralConstraint]
relate c i is = [sparse (map exchange (Consume p:map Supply ps)) `c` 0.0 | (ix, p) <- i, ps <- [mapMaybe (lookup ix) is], not $ null ps, not $ elem p ps]

equate :: IxEnv Anno -> [IxEnv Anno] -> [GeneralConstraint]
equate = relate Eql

exceed :: IxEnv Anno -> [IxEnv Anno] -> [GeneralConstraint]
exceed = relate Geq

equatePoly :: IxEnv Anno -> [IxEnv (Anno, Double)] -> [GeneralConstraint]
equatePoly i is = [sparse (exchange (Consume p):map (fmap negate) pcs) `Eql` 0.0 | (ix, p) <- i, pcs <- [mapMaybe (lookup ix) is], not $ null pcs, not $ elem p $ map fst pcs]

buildPoly :: [IxEnv a] -> [[[(Index, Index)]]] -> [[IxEnv (a, Double)]]
buildPoly = zipWith $ \qs -> concatMap $ map pure . flip mapMaybe qs . xlate
    where xlate ixs (ix, a) = (\ix' -> (ix', (a, poly ix / poly ix'))) <$> lookup ix ixs

objective :: IxEnv Anno -> [Index] -> Objective
objective qs ixs = sparse $ map (flip (,) 1 . fromJust . flip lookup qs) ixs

-- Reader/Writer/State Helpers

lookupSCP :: MonadReader CheckE m => Var -> m Prog
lookupSCP x = asks (head . filter (isJust . flip lookupFun x) . scps . checkF)

share :: (Monoid a, MonadWriter (a, SharedTys b) m) => SharedTys b -> m ()
share m = tell (mempty, m)

constrain :: (Monoid b, MonadWriter ([GeneralConstraint], b) m) => [GeneralConstraint] -> m ()
constrain c = tell (c, mempty)

constrainShares :: ([GeneralConstraint], SharedTys Anno) -> [GeneralConstraint]
constrainShares = uncurry (++) . second (foldMapWithKey equate . getMonoidalMap)

gamma :: MonadReader CheckE m => Var -> m (IxEnv Anno)
gamma x = asks (fromJust . lookup x . env)

costof :: MonadReader CheckE m => (Cost -> a) -> m a
costof k = asks (k . cost . checkF)

degreeof :: MonadReader CheckE m => m Int
degreeof = asks (degree . checkF)

lookupThisSCP :: MonadReader CheckE m => Var -> m (Maybe (Fun, (IxEnv Anno, IxEnv Anno)))
lookupThisSCP x = asks (lookup x . comp . checkF)

-- Type Helpers

runElabEx :: MonadReader Elab.Env m => Ex -> m ExTy
runElabEx e = asks (either (error "runElabEx") id . runReaderT (Elab.elab e))

-- The Engine

annotate :: (MonadError AnnoError m) => Int -> [Prog] -> m EqnEnv
annotate deg_max p = flip evalStateT 0 $ fmap concat $ for p $ \scp -> do
    scp' <- travFun (traverse $ \f -> (,) f <$> freshFunBounds deg_max f) scp
    let checkState = CheckF {degree = deg_max, scps = p, comp = scp', cost = constant}
    cs <- execWriterT $ runReaderT (annoSCP scp') checkState
    for scp' $ traverse $ \(fun, (pqs, _)) -> do
        let Arrow pty _ = tyOf fun
        progs <- for (reverse $ take (deg_max + 1) $ index pty) $ \ixs -> do
            let obj = objective pqs ixs
            return $ GeneralForm Minimize obj cs
        return (pqs, progs)

annotateEx :: (MonadError AnnoError m) => Int -> [Prog] -> Ex -> m Eqn
annotateEx deg_max p e = flip evalStateT 0 $ do
    let checkState = CheckF {degree = deg_max, scps = p, comp = mempty, cost = constant}
    ety <- runReaderT (runElabEx e) (Elab.Env [] [])
    let ty = tyGet ety
    ((q, q'), css) <- runWriterT $ runReaderT (anno ety) (CheckE [] checkState)
    progs <- for [[zeroIndex ty]] $ \ixs -> do
        let obj = objective q ixs
        return $ GeneralForm Minimize obj $ constrainShares css
    return (q, progs)

annoSCP :: (MonadError AnnoError m, MonadState Anno m) => FunEnv Anno -> ReaderT CheckF (WriterT [GeneralConstraint] m) ()
annoSCP = traverse_ (traverse_ $ mapReaderT (mapWriterT (fmap $ second $ constrainShares)) . annoFE)
    where annoFE :: (MonadError AnnoError m, MonadState Anno m) => (Fun, (IxEnv Anno, IxEnv Anno)) -> ReaderT CheckF (WriterT ([GeneralConstraint], SharedTys Anno) m) ()
          annoFE (Fun (Arrow pty rty) x e, (pqs, rqs)) = do
                    xqs <- rezero pqs
                    phi <- asks (concatMap (mapFun (second tyOf)) . scps)
                    ety <- runReaderT (runElabEx e) (Elab.Env (zip [x] (unpair pty)) phi)
                    let ety' = Elab.instantiate [rty] ety
                    (q, q') <- withReaderT (CheckE (zip [x] [xqs])) $ anno ety'
                    let q_0   = lookupZero q
                        pqs_0 = lookupZero pqs
                    constrain [sparse (map exchange [Consume pqs_0, Supply q_0]) `Eql` 0.0]
                    constrain $ equate rqs [q']
          annoFE (Native (Arrow pty@(ListTy pt)          rt) _ _, (pqs, rqs)) | pt == rt = consShift pty rqs [] [] pqs -- hack for car
          annoFE (Native (Arrow pty@(ListTy pt) (ListTy rt)) _ _, (pqs, rqs)) | pt == rt = consShift pty [] rqs [] pqs -- hack for cdr
          annoFE (Native (Arrow (PairTy (_, ListTy _)) rty@(ListTy _)) _ _, (pqs, rqs))  = consShift rty [] [] pqs rqs -- hack for cons
          annoFE (Native (Arrow ty ty') _ _, (pqs, rqs)) = do
                    let q_0  = lookupZero pqs
                        q_0' = lookupZero rqs
                    constrain [sparse [exchange $ Consume q_0, exchange $ Supply q_0'] `Eql` 0.0]
          consShift :: (MonadError AnnoError m, MonadState Anno m) => Ty -> IxEnv Anno -> IxEnv Anno -> IxEnv Anno -> IxEnv Anno -> ReaderT CheckF (WriterT ([GeneralConstraint], SharedTys Anno) m) ()
          consShift ty_l@(ListTy ty_h) qs_h qs_t qs_p qs_l = do
              let ty_p = PairTy (ty_h, ty_l)
              k <- asks degree
              q's_p <- if null qs_p then freshIxEnv k ty_p else return qs_p
              let limit (i, is) = const (i, is) <$> lookup i q's_p
                  Just shs = sequence $ takeWhile isJust $ map limit $ shift ty_l
                  ss = map (\(i, is) -> (,) <$> lookup i q's_p <*> sequence (filter isJust $ map (flip lookup qs_l) is)) shs
                  q's = buildPoly (repeat q's_p) $ projectionsDeg k ty_p
              constrain [sparse (map exchange (Supply q:map Consume ps)) `Eql` 0.0 |
                         Just (q, ps) <- ss]
              constrain $ concat $ zipWith equatePoly [qs_h, qs_t] q's

class Annotate a where
    anno :: (MonadError AnnoError m, MonadState Anno m) => a -> ReaderT CheckE (WriterT ([GeneralConstraint], SharedTys Anno) m) (IxEnv Anno, IxEnv Anno)

instance Annotate ExTy where
    anno (VarTy _ x) = do
        qx <- gamma x
        q  <- traverse reannotate qx
        q' <- rezero q
        share $ singleton qx [q]
        let q_0  = lookupZero q
            q_0' = lookupZero q'
        k <- costof k_var
        constrain [sparse [exchange $ Consume q_0, exchange $ Supply q_0'] `Eql` k]
        return (q, q')
    anno (ValTy ty v) = do
        (q, q') <- freshBounds ty
        k <- costof k_val
        let q_0  = lookupZero q
            q_0' = lookupZero q'
        constrain [sparse [exchange $ Consume q_0, exchange $ Supply q_0'] `Eql` k]
        return (q, q')
    anno (IfTy ty ep et ef) = do
        (qp, qp') <- anno ep
        ((qt, qt'), (tcs, tss)) <- mapReaderT runWriterT $ anno et
        ((qf, qf'), (fcs, fss)) <- mapReaderT runWriterT $ anno ef
        constrain tcs
        constrain fcs
        sharemap <- traverse (fmap <$> (,) <*> traverse reannotate) $ union (keys tss) (keys fss)
        share $ MonoidalMap $ fromList $ map (second pure) sharemap
        let reannotateShares ss = do
                ss' <- traverseWithKey (flip (fmap . flip (,)) . traverse reannotate) $
                        mapKeys (fromJust . flip lookup sharemap) $ getMonoidalMap ss
                constrain $ concatMap (uncurry exceed . second (pure . fst)) $ toList ss'
                return $ MonoidalMap $ fromList $ elems ss'
        share =<< reannotateShares tss
        share =<< reannotateShares fss
        (q, q') <- freshBounds ty
        [kp, kt, kf, kc] <- sequence [costof k_ifp, costof k_ift, costof k_iff, costof k_ifc]
        let q_0   = lookupZero q
            q_0'  = lookupZero q'
            qp_0  = lookupZero qp
            qp_0' = lookupZero qp'
            qt_0  = lookupZero qt
            qt_0' = lookupZero qt'
            qf_0  = lookupZero qf
            qf_0' = lookupZero qf'
        constrain $ exceed (deleteZero qt) [q] ++ exceed (deleteZero qf) [q]
        constrain [sparse [exchange $ Consume q_0,   exchange $ Supply qp_0] `Geq` kp,
                   sparse [exchange $ Consume qp_0', exchange $ Supply qt_0] `Geq` kt,
                   sparse [exchange $ Consume qp_0', exchange $ Supply qf_0] `Geq` kf,
                   sparse [exchange $ Consume qt_0', exchange $ Supply q_0'] `Geq` kc,
                   sparse [exchange $ Consume qf_0', exchange $ Supply q_0'] `Geq` kc]
        return (q, q')
    anno (AppTy _ f es) = do
        (qs, qs') <- unzip <$> traverse anno es
        degree <- degreeof
        let tys = map tyGet es
        (Arrow ty _, (qf, qf')) <- lookupThisSCP f >>= \case
            Just (asc, (qa, qa')) -> do
                cost_free <- costof (== zero)
                let fun = Elab.instantiate tys asc
                if degree <= 1 || cost_free then
                    return (tyOf fun, (qa, qa'))
                else do
                    scp <- asks (comp . checkF)
                    (qf, qf') <- freshFunBounds degree fun
                    -- this is cheating for polymorphic mutual recursion; should instantiate tys over the scp somehow
                    cfscp <- traverse (traverse $ \(f, _) -> (,) f <$> freshFunBounds (degree - 1) f) $ update f (fun, (qf, qf')) scp
                    let Just (_, (qcf, qcf')) = lookup f cfscp
                    constrain $ equate qf  [qa,  qcf]
                    constrain $ equate qf' [qa', qcf']
                    constrain =<< withReaderT (\ce -> (checkF ce) {degree = degree - 1, comp = cfscp, cost = zero}) (mapReaderT execWriterT (annoSCP cfscp))
                    return (tyOf fun, (qf, qf'))
            Nothing -> do
                scp <- lookupSCP f
                let fun = Elab.instantiate tys $ fromJust $ lookupFun scp f
                scp' <- travFun (traverse $ \f -> (,) f <$> freshFunBounds degree f) $ updateFun scp f fun
                constrain =<< withReaderT (\ce -> (checkF ce) {comp = scp'}) (mapReaderT execWriterT (annoSCP scp'))
                return $ first tyOf $ fromJust $ lookup f scp'
        let pairinits (PairTy (t1, t2)) = t1:map (PairTy . (,) t1) (pairinits t2)
            pairinits t = [t]
            itys = pairinits ty
        qt <- rezero qf
        qts <- foldrM ((\ixs envs -> (:envs) <$> ixs) . freshIxEnv degree) [qt] $ init itys
        q  <- rezero qf'
        q' <- rezero qf'
        let qf_0  = lookupZero qf
            qf_0' = lookupZero qf'
            q_0   = lookupZero q
            q_0'  = lookupZero q'
            qs_0  = map lookupZero qs
            qs_0' = map lookupZero qs'
            qxt   = buildPoly qts $ map (last . projectionsDeg degree) itys
            qts_0 = map lookupZero qts
        constrain $ concat $ zipWith equatePoly qs qxt
        k1 <- costof k_ap1
        k2 <- costof k_ap2
        c  <- freshAnno
        let (qs_0_args, ([q_ap_0], _)) = zipR (q_0:qs_0') qts_0
        constrain [sparse [exchange $ Consume q_in, exchange $ Supply q_out] `Eql` k1 |
                   (q_in, q_out) <- qs_0_args]
        constrain [sparse (map exchange [Consume q_ap_0, Supply qf_0, Supply c]) `Eql` k1]
        constrain [sparse (map exchange [Supply q_0', Consume qf_0', Consume c]) `Eql` k2]
        return (q, q')
    anno (LetTy _ ds e) = do
        (xs, (qs, qs')) <- second unzip <$> unzip <$> traverse (traverse anno) ds
        qxs <- traverse rezero qs
        (qe, qe') <- withReaderT (\ce -> ce {env = reverse (zip xs qxs) ++ env ce}) $ anno e
        q  <- rezero qe
        q' <- rezero qe'
        k1 <- costof k_lt1
        k2 <- costof k_lt2
        let q_0   = lookupZero q
            q_0'  = lookupZero q'
            qs_0  = map lookupZero qs
            qs_0' = map lookupZero qs'
            qe_0  = lookupZero qe
            qe_0' = lookupZero qe'
        constrain [sparse [exchange $ Consume q_in, exchange $ Supply q_out] `Geq` k1 |
                   (q_in, q_out) <- zip (q_0:qs_0') (qs_0 ++ [qe_0])]
        constrain [sparse (map exchange [Supply q_0', Consume qe_0']) `Eql` k2]
        return (q, q')
