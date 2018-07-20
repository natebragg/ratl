{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ratl.Anno (
    annotate,
    annotateEx
) where

import Data.List (transpose, union, find)
import Data.Map (foldMapWithKey, traverseWithKey, elems, fromList, toList, mapKeys)
import Data.Map.Monoidal (MonoidalMap(..), singleton, keys)
import Data.Mapping (Mapping(lookupBy, updateBy, deleteBy, lookup, update, elements, values))
import qualified Data.Mapping as Mapping (fromList)
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Foldable (traverse_, foldrM)
import Data.Traversable (for)
import Control.Arrow (first, second)
import Control.Monad (zipWithM, mfilter)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Except.Extra (unlessJust)
import Control.Monad.State (MonadState, evalStateT, get, put)
import Control.Monad.Reader (MonadReader, runReaderT, withReaderT, mapReaderT, ReaderT, asks)
import Control.Monad.Writer (MonadWriter, runWriterT, execWriterT, mapWriterT, WriterT, tell)
import Prelude hiding (lookup)

import Data.Clp.Clp (OptimizationDirection(Minimize))
import Data.Clp.LinearFunction (
    (|*), (|+|), (|-|),
    LinearFunction,
    LinearFunFamily,
    sparse,
    coefficients,
    )
import Data.Clp.Program (
    GeneralConstraint, (==$), (>=$),
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
    TypedFun(..),
    TypedEx(..),
    TypedProg,
    tyOf,
    tyGet,
    )
import qualified Language.Ratl.Elab as Elab (
    instantiate,
    solve,
    )


type Anno = Int

type IxEnv = LinearFunFamily Index
type VarEnv = [(Var, IxEnv)]
type FunEnv = [(Var, (TypedFun, (IxEnv, IxEnv)))]
type SharedTys = MonoidalMap IxEnv [IxEnv]
type Eqn = ([(Index, Anno)], [GeneralForm])
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

data CheckE = CheckE {
        env :: VarEnv,
        checkF :: CheckF
    }

data CheckF = CheckF {
        degree :: Int,
        scps :: [TypedProg],
        comp :: FunEnv,
        cost :: Cost
    }

data AnnoError = ProjectionError Ty Index

instance Show AnnoError where
    show (ProjectionError t i) = "Index " ++ show i ++ " does not appear to be a projection of type " ++ show t ++ "."

-- IxEnv Helpers

isZero :: Index -> Bool
isZero = (0 ==) . deg

coerceZero :: IxEnv -> LinearFunction
coerceZero = fromJust . lookupBy isZero

filterZero :: IxEnv -> IxEnv
filterZero = deleteBy (not . isZero)

updateZero :: LinearFunction -> IxEnv -> IxEnv
updateZero = updateBy isZero

-- Annotation Helpers

freshAnno :: MonadState Anno m => m LinearFunction
freshAnno = do
    q <- get
    put (q + 1)
    return $ sparse [(q, 1)]

freshIxEnv :: MonadState Anno m => Int -> Ty -> m IxEnv
freshIxEnv k t = Mapping.fromList <$> traverse (\i -> (,) i <$> freshAnno) (indexDeg k t)

freshBounds :: (MonadReader CheckE m, MonadState Anno m) => Ty -> m (IxEnv, IxEnv)
freshBounds t = do
    k <- degreeof
    q  <- freshIxEnv k t
    q' <- rezero q
    return (q, q')

freshFunBounds :: MonadState Anno m => Int -> TypedFun -> m (IxEnv, IxEnv)
freshFunBounds k fun = do
    let Arrow t t' = tyOf fun
    q  <- freshIxEnv k t
    q' <- freshIxEnv k t'
    return (q, q')

rezero :: MonadState Anno m => IxEnv -> m IxEnv
rezero qs = do
    q_0' <- freshAnno
    return $ updateZero q_0' qs

reannotate :: MonadState Anno m => IxEnv -> m IxEnv
reannotate ixs = Mapping.fromList <$> traverse (traverse $ const freshAnno) (elements ixs)

-- Constraint Helpers

buildPoly :: [IxEnv] -> [[[(Index, Index)]]] -> [[[(Index, LinearFunction)]]]
buildPoly = zipWith $ \qs -> concatMap $ map pure . flip mapMaybe (elements qs) . xlate
    where xlate ixs (ix, lf) = (\ix' -> (ix', lf |* (poly ix / poly ix'))) <$> lookup ix ixs

nonEmptyConstraints c ixs k =
    case (mfilter (any (/= 0)) $          lookupBy isZero ixs,
           filter (any (/= 0)) $ values $ deleteBy isZero ixs) of
        (Just z, nz) -> (z `c` k):map (`c` 0) nz
        (     _, nz) ->           map (`c` 0) nz

(==*) :: LinearFunFamily Index -> Double -> [GeneralConstraint]
(==*) = nonEmptyConstraints (==$)

(>=*) :: LinearFunFamily Index -> Double -> [GeneralConstraint]
(>=*) = nonEmptyConstraints (>=$)

infix 4 ==*
infix 4 >=*

-- Reader/Writer/State Helpers

lookupSCP :: MonadReader CheckE m => Var -> m TypedProg
lookupSCP x = asks (head . filter (isJust . lookup x) . scps . checkF)

share :: (Monoid a, MonadWriter (a, SharedTys) m) => SharedTys -> m ()
share m = tell (mempty, m)

constrain :: (Monoid b, MonadWriter ([GeneralConstraint], b) m) => [GeneralConstraint] -> m ()
constrain c = tell (c, mempty)

constrainShares :: ([GeneralConstraint], SharedTys) -> [GeneralConstraint]
constrainShares = uncurry (++) . second (foldMapWithKey equate . getMonoidalMap)
    where equate i is = (foldl (|-|) i is ==* 0)

gamma :: MonadReader CheckE m => Var -> m IxEnv
gamma x = asks (fromJust . lookup x . env)

costof :: MonadReader CheckE m => (Cost -> a) -> m a
costof k = asks (k . cost . checkF)

degreeof :: MonadReader CheckE m => m Int
degreeof = asks (degree . checkF)

lookupThisSCP :: MonadReader CheckE m => Var -> m (Maybe (TypedFun, (IxEnv, IxEnv)))
lookupThisSCP x = asks (lookup x . comp . checkF)

-- The Engine

annoSCP :: (MonadError AnnoError m, MonadState Anno m) => FunEnv -> ReaderT CheckF (WriterT [GeneralConstraint] m) ()
annoSCP = traverse_ (traverse_ $ mapReaderT (mapWriterT (fmap $ second $ constrainShares)) . annoFE)
    where annoFE :: (MonadError AnnoError m, MonadState Anno m) => (TypedFun, (IxEnv, IxEnv)) -> ReaderT CheckF (WriterT ([GeneralConstraint], SharedTys) m) ()
          annoFE (TypedFun (Arrow pty rty) x e, (pqs, rqs)) = do
              xqs <- rezero pqs
              (q, q') <- withReaderT (CheckE (zip [x] [xqs])) $ anno e
              constrain $ [coerceZero pqs |-| coerceZero q ==$ 0]
              constrain $ rqs |-| q' ==* 0
          annoFE (TypedNative (Arrow pty@(ListTy pt)          rt) _ _, (pqs, rqs)) | pt == rt = consShift pty rqs lz lz pqs -- hack for car
          annoFE (TypedNative (Arrow pty@(ListTy pt) (ListTy rt)) _ _, (pqs, rqs)) | pt == rt = consShift pty lz rqs lz pqs -- hack for cdr
          annoFE (TypedNative (Arrow (PairTy (_, ListTy _)) rty@(ListTy _)) _ _, (pqs, rqs))  = consShift rty lz lz pqs rqs -- hack for cons
          annoFE (TypedNative (Arrow ty ty') _ _, (pqs, rqs)) = do
              constrain $ [coerceZero pqs |-| coerceZero rqs ==$ 0]
          lz :: IxEnv
          lz = Mapping.fromList []
          consShift :: (MonadError AnnoError m, MonadState Anno m) => Ty -> IxEnv -> IxEnv -> IxEnv -> IxEnv -> ReaderT CheckF (WriterT ([GeneralConstraint], SharedTys) m) ()
          consShift ty_l@(ListTy ty_h) qs_h qs_t qs_p qs_l = do
              let ty_p = PairTy (ty_h, ty_l)
              k <- asks degree
              q's_p <- if null (values qs_p) then freshIxEnv k ty_p else return qs_p
              let limit (i, is) = const (i, is) <$> lookup i q's_p
                  Just shs = sequence $ takeWhile isJust $ map limit $ shift ty_l
                  ss = map (\(i, is) -> (,) <$> lookup i q's_p <*> sequence (filter isJust $ map (flip lookup qs_l) is)) shs
                  q's = buildPoly (repeat q's_p) $ projectionsDeg k ty_p
              constrain [foldl (|+|) (q |* (-1)) ps ==$ 0 |
                         Just (q, ps) <- ss]
              constrain [foldl (|-|) p pcs ==$ 0 |
                         (q_in, q_out) <- zip [qs_h, qs_t] q's, (ix, p) <- elements q_in, pcs <- [mapMaybe (lookup ix) q_out], not $ null pcs]

class Annotate a where
    anno :: (MonadError AnnoError m, MonadState Anno m) => a -> ReaderT CheckE (WriterT ([GeneralConstraint], SharedTys) m) (IxEnv, IxEnv)

instance Annotate TypedEx where
    anno (TypedVar _ x) = do
        qx <- gamma x
        q  <- reannotate qx
        q' <- rezero q
        share $ singleton qx [q]
        k <- costof k_var
        constrain $ q |-| q' ==* k
        return (q, q')
    anno (TypedVal ty v) = do
        (q, q') <- freshBounds ty
        k <- costof k_val
        constrain $ q |-| q' ==* k
        return (q, q')
    anno (TypedIf ty ep et ef) = do
        (qp, qp') <- anno ep
        ((qt, qt'), (tcs, tss)) <- mapReaderT runWriterT $ anno et
        ((qf, qf'), (fcs, fss)) <- mapReaderT runWriterT $ anno ef
        constrain tcs
        constrain fcs
        sharemap <- traverse (fmap <$> (,) <*> reannotate) $ union (keys tss) (keys fss)
        share $ MonoidalMap $ fromList $ map (second pure) sharemap
        let reannotateShares ss = do
                ss' <- traverseWithKey (flip (fmap . flip (,)) . reannotate) $
                        mapKeys (fromJust . flip lookup sharemap) $ getMonoidalMap ss
                constrain $ concat [s |-| s' >=* 0 | (s, (s', _)) <- toList ss']
                return $ MonoidalMap $ fromList $ elems ss'
        share =<< reannotateShares tss
        share =<< reannotateShares fss
        (q, q') <- freshBounds ty
        [kp, kt, kf, kc] <- sequence [costof k_ifp, costof k_ift, costof k_iff, costof k_ifc]
        constrain [coerceZero q   |-| coerceZero qp >=$ kp]
        constrain [coerceZero qp' |-| coerceZero qt >=$ kt]
        constrain [coerceZero qp' |-| coerceZero qf >=$ kf]
        constrain $ qt' |-| q' >=* kc
        constrain $ qf' |-| q' >=* kc
        return (q, q')
    anno (TypedApp _ f es) = do
        (qs, qs') <- unzip <$> traverse anno es
        degree <- degreeof
        let tys = map tyGet es
        (Arrow ty _, (qf, qf')) <- lookupThisSCP f >>= \case
            Just (asc, (qa, qa')) -> do
                cost_free <- costof (== zero)
                let Arrow ty ty' = tyOf asc
                    theta = Elab.solve tys (unpair ty ++ [ty'])
                    fun = Elab.instantiate theta asc
                if degree <= 1 || cost_free then
                    return (tyOf fun, (qa, qa'))
                else do
                    scp <- asks (comp . checkF)
                    (qf, qf') <- freshFunBounds degree fun
                    -- this is cheating for polymorphic mutual recursion; should instantiate tys over the scp somehow
                    cfscp <- traverse (traverse $ \(f, _) -> (,) f <$> freshFunBounds (degree - 1) f) $ update f (fun, (qf, qf')) scp
                    let Just (_, (qcf, qcf')) = lookup f cfscp
                    constrain $ qf  |-| qa  |-| qcf  ==* 0
                    constrain $ qf' |-| qa' |-| qcf' ==* 0
                    constrain =<< withReaderT (\ce -> (checkF ce) {degree = degree - 1, comp = cfscp, cost = zero}) (mapReaderT execWriterT (annoSCP cfscp))
                    return (tyOf fun, (qf, qf'))
            Nothing -> do
                scp <- lookupSCP f
                let asc = fromJust $ lookup f scp
                    Arrow ty ty' = tyOf asc
                    theta = Elab.solve tys (unpair ty ++ [ty'])
                    fun = Elab.instantiate theta asc
                scp' <- traverse (traverse $ \f -> (,) f <$> freshFunBounds degree f) $ update f fun scp
                constrain =<< withReaderT (\ce -> (checkF ce) {comp = scp'}) (mapReaderT execWriterT (annoSCP scp'))
                return $ first tyOf $ fromJust $ lookup f scp'
        let pairinits (PairTy (t1, t2)) = t1:map (PairTy . (,) t1) (pairinits t2)
            pairinits t = [t]
            itys = pairinits ty
            pis = map (transpose . last . projectionsDeg degree) itys
            ips = map (map (map (\(a, b) -> (b, a)))) pis
        (qxs, qxs') <- withReaderT (\ce -> ce { checkF = (checkF ce) {cost = zero}}) $ unzip <$> do
            let flippedZipWithM a b c f = zipWithM f a (zip b c)
            let ess = zipWith (map . const) es ips
            flippedZipWithM ess qs qs' $ \es (qx_0, qx'_0) -> do
                (qxs_j, qx's_j) <- unzip <$> mapM anno (tail es)
                return $ (qx_0:qxs_j, qx'_0:qx's_j)
        qt <- rezero qf
        qts <- foldrM ((\ixs envs -> (:envs) <$> ixs) . freshIxEnv degree) [qt] $ init itys
        q  <- rezero qf'
        q' <- rezero qf'
        let qxrs' = zipWith reifyqs qxs' ips
                where reifyqs = (foldl1 (|+|) .) . zipWith makefull
                      makefull q ip = Mapping.fromList $ mapMaybe (\(ix, p) -> flip (,) p <$> lookup ix ip) $ elements q
        k1 <- costof k_ap1
        k2 <- costof k_ap2
        c  <- freshAnno
        constrain [q_in |-| q_out ==$ 0.0 |
                   (q_in, q_out) <- concat $ zipWith zip (map (map coerceZero) qxs) $ [coerceZero q]:map values qts]
        constrain $ concat [q_in |-| q_out ==* k1 | (q_in, q_out) <- zip qxrs' qts]
        constrain [(coerceZero $ last qts) |-| coerceZero qf |-| c ==$ k1]
        constrain [c |+| coerceZero qf' |-| coerceZero q' ==$ k2]
        return (q, q')
    anno (TypedLet _ ds e) = do
        (xs, (qs, qs')) <- second unzip <$> unzip <$> traverse (traverse anno) ds
        qxs <- traverse rezero qs
        (qe, qe') <- withReaderT (\ce -> ce {env = reverse (zip xs qxs) ++ env ce}) $ anno e
        q  <- rezero qe
        q' <- rezero qe'
        k1 <- costof k_lt1
        k2 <- costof k_lt2
        constrain [coerceZero q_in |-| coerceZero q_out >=$ k1 |
                   (q_in, q_out) <- zip (q:qs') (qs ++ [qe])]
        constrain $ filterZero qe' |-| filterZero q' ==* k2
        return (q, q')

makeEqn :: Int -> IxEnv -> [GeneralConstraint] -> Ty -> Eqn
makeEqn k q cs ty =
    let objective = foldr1 (|+|) . map (fromJust . flip lookup q)
        program = flip (GeneralForm Minimize) cs . objective
        progs = reverse $ take (k + 1) $ map program $ index ty
        resource = (\[a] -> a) . fst . coefficients
        indexmap = map (fmap resource) $ elements q
    in (indexmap, progs)

annotate :: MonadError AnnoError m => Int -> [TypedProg] -> m EqnEnv
annotate k p = flip evalStateT 0 $ fmap concat $ for p $ \scp -> do
    scp' <- traverse (traverse $ \f -> (,) f <$> freshFunBounds k f) scp
    let checkState = CheckF {degree = k, scps = p, comp = scp', cost = constant}
    cs <- execWriterT $ runReaderT (annoSCP scp') checkState
    for scp' $ traverse $ \(fun, (pqs, _)) -> do
        let Arrow pty _ = tyOf fun
        return $ makeEqn k pqs cs pty

annotateEx :: MonadError AnnoError m => Int -> [TypedProg] -> TypedEx -> m Eqn
annotateEx k p e = flip evalStateT 0 $ do
    let checkState = CheckF {degree = k, scps = p, comp = mempty, cost = constant}
    ((q, q'), css) <- runWriterT $ runReaderT (anno e) (CheckE [] checkState)
    return $ makeEqn 0 q (constrainShares css) (tyGet e)
