{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Language.Ratl.Anno (
    annotate,
    annotateEx
) where

import Control.Arrow (first, second)
import Control.Monad (zipWithM, mfilter)
import Control.Monad.RWS (MonadRWS, evalRWST)
import Control.Monad.RWS.Extra (MonadRS, MonadWS, evalRWT)
import Control.Monad.Reader (MonadReader, asks, local)
import Control.Monad.State (MonadState, evalStateT, get, put)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Foldable (traverse_, foldrM)
import Data.List (transpose, intersect, union, inits)
import Data.Mapping (Mapping(..), (<?<))
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Traversable (for)
import Numeric.Algebra (
    Additive(..),
    Semiring,
    Group(..),
    Monoidal,
    LeftModule(..),
    RightModule(..),
    sum)
import qualified Numeric.Algebra as Alg (zero)
import Prelude hiding (lookup, sum, negate, (+), (-))

import Data.Clp.Clp (OptimizationDirection(Minimize))
import Data.Clp.LinearFunction (
    LinearFunction,
    LinearFunFamily,
    sparse,
    coefficients,
    )
import Data.Clp.Program (
    GeneralConstraint, (==$), (>=$),
    GeneralForm(..),
    )
import Language.Ratl.Index (
    Indexable,
    Index,
    ContextIndex,
    context,
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
    FunTy(..),
    )
import Language.Ratl.Ast (
    Var(..),
    TypedFun(..),
    TypedEx(..),
    TypedProg,
    tyOf,
    tyGet,
    )
import Language.Ratl.Elab (
    Unifiable(uid),
    unify,
    )
import qualified Language.Ratl.Elab as Elab (
    instantiate,
    solve,
    )


type Anno = Int

data LNVar = BVar Int
           | FVar Var
    deriving (Eq, Ord, Show)

data IndexEnv t i where
    IndexEnv :: Indexable i t => {
        ixTy :: t,
        eqns :: LinearFunFamily i
    } -> IndexEnv t i

deriving instance (Show t, Show i) => Show (IndexEnv t i)

instance (Eq i, Indexable i t, Unifiable t) => Additive (IndexEnv t i) where
    q + p = IndexEnv (ixTy q `unify` ixTy p) (eqns q + eqns p)

instance (Eq i, Indexable i t, Unifiable t, Semiring r, LeftModule r (LinearFunFamily i)) => LeftModule r (IndexEnv t i) where
    n .* q = q {eqns = n .* eqns q}

instance (Eq i, Indexable i t, Unifiable t, Semiring r, RightModule r (LinearFunFamily i)) => RightModule r (IndexEnv t i) where
    q *. n = q {eqns = eqns q *. n}

instance (Eq i, Indexable i t, Unifiable t) => Monoidal (IndexEnv t i) where
    zero = IndexEnv uid Alg.zero

instance (Eq i, Indexable i t, Unifiable t) => Group (IndexEnv t i) where
    negate q = q {eqns = negate $ eqns q}

type IxEnv = IndexEnv Ty Index
type CIxEnv = IndexEnv [Ty] ContextIndex
type VarEnv = [(LNVar, IxEnv)]
type FunEnv = [(Var, (TypedFun, (CIxEnv, IxEnv)))]
type Eqn = ([(ContextIndex, Anno)], [GeneralForm])
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

data AnnoState = AnnoState {
        degree :: Int,
        scps :: [TypedProg],
        comp :: FunEnv,
        cost :: Cost
    }

-- Locally Nameless Representation Helpers

bindvars :: [a] -> [(LNVar, a)]
bindvars =
    let boundvars = (map BVar [0..])
    in \xs -> zip boundvars $ reverse xs

varclose :: [Var] -> VarEnv -> VarEnv
varclose xs fvs = fvs <?< bindvars (map FVar xs)

-- IxEnv Helpers

isZero :: Indexable i t => i -> Bool
isZero = (0 ==) . deg

coerceZero :: Indexable i t => IndexEnv t i -> LinearFunction
coerceZero = fromJust . lookupBy isZero . eqns

updateZero :: Indexable i t => LinearFunction -> IndexEnv t i -> IndexEnv t i
updateZero f q = q {eqns = updateBy isZero (Just $ zeroIndex $ ixTy q) f $ eqns q}

-- Annotation Helpers

freshAnno :: MonadState Anno m => m LinearFunction
freshAnno = do
    q <- get
    put (q + 1)
    return $ sparse [(q, 1)]

freshIxEnv :: (MonadRS AnnoState Anno m, Indexable i t) => t -> m (IndexEnv t i)
freshIxEnv t = do
    k <- degreeof
    IndexEnv t <$> fromList <$> traverse (\i -> (,) i <$> freshAnno) (indexDeg k t)

freshBounds :: MonadRS AnnoState Anno m => Ty -> m (IxEnv, IxEnv)
freshBounds t = do
    q  <- freshIxEnv t
    q' <- rezero q
    return (q, q')

freshFunBounds :: MonadRS AnnoState Anno m => TypedFun -> m (CIxEnv, IxEnv)
freshFunBounds fun = do
    let Arrow t t' = tyOf fun
    q  <- freshIxEnv t
    q' <- freshIxEnv t'
    return (q, q')

rezero :: (MonadState Anno m, Indexable i t) => IndexEnv t i -> m (IndexEnv t i)
rezero qs = do
    q_0' <- freshAnno
    return $ updateZero q_0' qs

reannotate :: MonadState Anno m => IxEnv -> m IxEnv
reannotate q = (\eqns' -> q {eqns = eqns'}) <$> fromList <$> traverse (traverse $ const freshAnno) (elements $ eqns q)

-- Constraint Helpers

buildPoly :: [LinearFunFamily ContextIndex] -> [[[(ContextIndex, Index)]]] -> [[[(Index, LinearFunction)]]]
buildPoly = zipWith $ \qs -> concatMap $ map pure . flip mapMaybe (elements qs) . xlate
    where xlate ixs (ix, lf) = (\ix' -> (ix', lf *. (poly ix / poly ix'))) <$> lookup ix ixs

nonEmptyConstraints :: Indexable i t => (LinearFunction -> Double -> GeneralConstraint) -> IndexEnv t i -> Double -> [GeneralConstraint]
nonEmptyConstraints c q k =
    case (mfilter (not . null) $          lookupBy isZero $ eqns q,
           filter (not . null) $ values $ deleteBy isZero $ eqns q) of
        (Just z, nz) -> (z `c` k):map (`c` 0) nz
        (     _, nz) ->           map (`c` 0) nz

(==*) :: Indexable i t => IndexEnv t i -> Double -> [GeneralConstraint]
(==*) = nonEmptyConstraints (==$)

(>=*) :: Indexable i t => IndexEnv t i -> Double -> [GeneralConstraint]
(>=*) = nonEmptyConstraints (>=$)

infix 4 ==*
infix 4 >=*

(%-%) :: (Eq i, Indexable i t1, Eq j, Indexable j t2) => IndexEnv t1 i -> IndexEnv t2 j -> LinearFunction
q %-% p = coerceZero q - coerceZero p

infix 5 %-%

-- Sharing Helpers

share :: MonadWS [GeneralConstraint] Anno m => VarEnv -> VarEnv -> m VarEnv
share fvas fvbs = do
    let vs = intersect (keys fvas) (keys fvbs)
    combined <- for vs $ \v -> do
        let Just qa = lookup v fvas
            Just qb = lookup v fvbs
        qc <- reannotate qa
        constrain $ qc - qa - qb ==* 0
        return (v, qc)
    return $ combined ++ foldr delete (fvas ++ fvbs) vs

shareSubtype :: MonadWS [GeneralConstraint] Anno m => VarEnv -> VarEnv -> m VarEnv
shareSubtype fvas fvbs = do
    let vs = intersect (keys fvas) (keys fvbs)
    combined <- for vs $ \v -> do
        let Just qa = lookup v fvas
            Just qb = lookup v fvbs
        qc <- reannotate qa
        constrain $ qc - qa >=* 0
        constrain $ qc - qb >=* 0
        return (v, qc)
    return $ combined ++ foldr delete (fvas ++ fvbs) vs

shareBind :: MonadWS [GeneralConstraint] Anno m => VarEnv -> VarEnv -> m VarEnv
shareBind fvas fvbs = do
    let vs = intersect (keys fvas) (keys fvbs)
    for vs $ \v -> do
        let Just qa = lookup v fvas
            Just qb = lookup v fvbs
        constrain $ qa - qb ==* 0
    return $ foldr delete fvbs vs

to_ctx :: IxEnv -> CIxEnv
to_ctx q = IndexEnv [ixTy q] $ fromList $ map (first $ context . pure) $ elements $ eqns q

-- Reader/Writer/State Helpers

lookupSCP :: MonadReader AnnoState m => Var -> m TypedProg
lookupSCP x = asks (head . filter (isJust . lookup x) . scps)

constrain :: MonadWriter [GeneralConstraint] m => [GeneralConstraint] -> m ()
constrain = tell

costof :: MonadReader AnnoState m => (Cost -> a) -> m a
costof k = asks (k . cost)

degreeof :: MonadReader AnnoState m => m Int
degreeof = asks degree

lookupThisSCP :: MonadReader AnnoState m => Var -> m (Maybe (TypedFun, (CIxEnv, IxEnv)))
lookupThisSCP x = asks (lookup x . comp)

-- The Engine

annoSCP :: MonadRWS AnnoState [GeneralConstraint] Anno m => FunEnv -> m ()
annoSCP = traverse_ annoFun
    where annoFun :: MonadRWS AnnoState [GeneralConstraint] Anno m => (Var, (TypedFun, (CIxEnv, IxEnv))) -> m ()
          annoFun (_, (TypedFun (Arrow pty rty) x e, (pqs, rqs))) = do
              xqs <- rezero pqs
              (fvs, q, q') <- anno e
              k <- degreeof
              shareBind (zip [FVar x] [IndexEnv (head $ ixTy xqs) $ eqns xqs <<< map (\(a,b) -> (b,a)) (concat $ concat $ projectionsDeg k pty)]) fvs
              constrain $ [pqs %-% q ==$ 0]
              constrain $ rqs - q' ==* 0
          annoFun (V "car",  (TypedNative (Arrow [ty_l@(ListTy ty_h)] _) _ _, (pqs, rqs))) = freshIxEnv [ty_h, ty_l] >>= \q -> consShift rqs lz q pqs
          annoFun (V "cdr",  (TypedNative (Arrow [ty_l@(ListTy ty_h)] _) _ _, (pqs, rqs))) = freshIxEnv [ty_h, ty_l] >>= \q -> consShift lz rqs q pqs
          annoFun (V "cons", (TypedNative (Arrow                    _ _) _ _, (pqs, rqs))) = to_ctx <$> pure rqs     >>= \q -> consShift lz lz pqs q
          annoFun (_, (TypedNative (Arrow ty ty') _ _, (pqs, rqs))) = do
              constrain $ [pqs %-% rqs ==$ 0]
          lz :: Monoidal m => m
          lz = Alg.zero
          consShift :: MonadRWS AnnoState [GeneralConstraint] Anno m => IxEnv -> IxEnv -> CIxEnv -> CIxEnv -> m ()
          consShift (IndexEnv _ qs_h) (IndexEnv _ qs_t) (IndexEnv ty_p qs_p) (IndexEnv [ty_l] qs_l) = do
              k <- degreeof
              let limit (i, is) = const (i, is) <$> lookup i qs_p
                  Just shs = sequence $ takeWhile isJust $ map limit $ shift ty_l
                  ss = map (\(i, is) -> (,) <$> lookup i qs_p <*> sequence (filter isJust $ map (flip lookup qs_l) is)) shs
                  q's = buildPoly (repeat qs_p) $ projectionsDeg k ty_p
              constrain [sum ps - q ==$ 0 |
                         Just (q, ps) <- ss]
              constrain [p - sum pcs ==$ 0 |
                         (q_in, q_out) <- zip [qs_h, qs_t] q's, (ix, p) <- elements q_in, pcs <- [mapMaybe (lookup ix) q_out], not $ null pcs]

annoSeq :: MonadRWS AnnoState [GeneralConstraint] Anno m => (Cost -> Double) -> [TypedEx] -> m (VarEnv, VarEnv, IxEnv, IxEnv)
annoSeq k_e es = do
    (fves, qes, qe's) <- unzip3 <$> traverse anno es
    fvs <- foldrM share [] fves
    q' <- freshIxEnv UnitTy
    let q:qs = qes ++ [q']
    k <- costof k_e
    constrain [q_in %-% q_out >=$ k |
               (q_in, q_out) <- zip qe's qs]
    qxs <- traverse rezero qes
    return (fvs, bindvars qxs, q, q')

class Annotate a where
    anno :: MonadRWS AnnoState [GeneralConstraint] Anno m => a -> m (VarEnv, IxEnv, IxEnv)

instance Annotate TypedEx where
    anno (TypedVar ty x) = do
        (q, q') <- freshBounds ty
        k <- costof k_var
        constrain $ q - q' ==* k
        return ([(FVar x, q)], q, q')
    anno (TypedVal ty v) = do
        (q, q') <- freshBounds ty
        k <- costof k_val
        constrain $ q - q' ==* k
        return ([], q, q')
    anno (TypedIf ty ep et ef) = do
        (fvps, qp, qp') <- anno ep
        (fvts, qt, qt') <- anno et
        (fvfs, qf, qf') <- anno ef
        fvs <- share fvps =<< shareSubtype fvts fvfs
        (q, q') <- freshBounds ty
        [kp, kt, kf, kc] <- sequence [costof k_ifp, costof k_ift, costof k_iff, costof k_ifc]
        constrain [q   %-% qp >=$ kp,
                   qp' %-% qt >=$ kt,
                   qp' %-% qf >=$ kf]
        constrain $ qt' - q' >=* kc
        constrain $ qf' - q' >=* kc
        return (fvs, q, q')
    anno (TypedApp _ f es) = do
        fvqes <- traverse anno es
        degree <- degreeof
        let tys = map tyGet es
        (Arrow ty _, (qf, qf')) <- lookupThisSCP f >>= \case
            Just (asc, (qa, qa')) -> do
                cost_free <- costof (== zero)
                let Arrow ty ty' = tyOf asc
                    Right theta = Elab.solve tys (ty ++ [ty'])
                    fun = Elab.instantiate theta asc
                if degree <= 1 || cost_free then
                    return (tyOf fun, (qa, qa'))
                else do
                    scp <- asks comp
                    (qf, qf') <- freshFunBounds fun
                    local (\s -> s {degree = degree - 1, cost = zero}) $ do
                        -- this is cheating for polymorphic mutual recursion; should instantiate tys over the scp somehow
                        cfscp <- traverse (traverse $ \(f, _) -> (,) f <$> freshFunBounds f) $ update f (fun, (qf, qf')) scp
                        let Just (_, (qcf, qcf')) = lookup f cfscp
                        constrain $ qf  - qa  - qcf  ==* 0
                        constrain $ qf' - qa' - qcf' ==* 0
                        local (\s -> s {comp = cfscp}) $ annoSCP cfscp
                    return (tyOf fun, (qf, qf'))
            Nothing -> do
                scp <- lookupSCP f
                let asc = fromJust $ lookup f scp
                    Arrow ty ty' = tyOf asc
                    Right theta = Elab.solve tys (ty ++ [ty'])
                    fun = Elab.instantiate theta asc
                scp' <- traverse (traverse $ \f -> (,) f <$> freshFunBounds f) $ update f fun scp
                local (\cf -> cf {comp = scp'}) $ annoSCP scp'
                return $ first tyOf $ fromJust $ lookup f scp'
        let itys = tail $ inits ty
            pis = map (transpose . last . projectionsDeg degree) itys
        (fvxs, qxs, qxs') <- local (\cf -> cf {cost = zero}) $ unzip3 <$> do
            flip (flip zipWithM (zip es (zip pis itys))) fvqes $ \(e, (pi, ity)) (fv_0, qx_0, qx'_0) -> do
                (fvxs_js, qxs_j, qx's_j) <- unzip3 <$> mapM anno (tail $ map (const e) pi)
                fvxs_j <- foldrM share fv_0 fvxs_js
                return $ (fvxs_j, qx_0:qxs_j, sum $ zipWith ((IndexEnv ity .) . (<<<) . eqns) (qx'_0:qx's_j) pi)
        qt <- rezero qf
        qts <- foldrM ((\ixs envs -> (:envs) <$> ixs) . freshIxEnv) [qt] $ init itys
        q  <- rezero qf'
        q' <- rezero qf'
        k1 <- costof k_ap1
        k2 <- costof k_ap2
        c  <- freshAnno
        constrain [q_in - q_out ==$ 0.0 |
                   (q_in, q_out) <- concat $ zipWith zip (map (map coerceZero) qxs) $ [coerceZero q]:map (values . eqns) qts]
        constrain $ concat [q_in - q_out ==* k1 | (q_in, q_out) <- zip qxs' qts]
        constrain [(last qts %-% qf) - c ==$ k1,
                   c + (qf' %-% q') ==$ k2]
        fvs <- foldrM share [] fvxs
        return (fvs, q, q')
    anno (TypedLet _ bs e) = do
        let (xs, es) = unzip bs
        (fvbs, bvbs, qb, qb') <- annoSeq k_lt1 es
        (fves, qe, qe') <- anno e
        fves' <- shareBind (varclose xs bvbs) fves
        fvs <- share fvbs fves'
        q  <- rezero qe
        q' <- rezero qe'
        k1 <- costof k_lt1
        k2 <- costof k_lt2
        constrain [q %-% qb >=$ k1,
                   qb' %-% qe >=$ 0,
                   qe' %-% q' ==$ k2]
        return (fvs, q, q')

makeEqn :: Int -> CIxEnv -> [GeneralConstraint] -> Eqn
makeEqn k (IndexEnv ty q) cs =
    let objective = sum . map (fromJust . flip lookup q)
        program = flip (GeneralForm Minimize) cs . objective
        progs = reverse $ take (k + 1) $ map program $ index ty
        resource = (\[a] -> a) . fst . coefficients
        indexmap = map (fmap resource) $ elements q
    in (indexmap, progs)

annotate :: Monad m => Int -> [TypedProg] -> m EqnEnv
annotate k p = do
    let checkState = AnnoState {degree = k, scps = p, comp = mempty, cost = constant}
    flip evalStateT 0 $ fmap concat $ for p $ \scp -> do
        (scp', cs) <- flip evalRWT checkState $ do
            scp' <- traverse (traverse $ \f -> (,) f <$> freshFunBounds f) scp
            local (\s -> s {comp = scp'}) $ annoSCP scp'
            return scp'
        for scp' $ traverse $ \(fun, (pqs, _)) -> do
            return $ makeEqn k pqs cs

annotateEx :: Monad m => Int -> [TypedProg] -> TypedEx -> m Eqn
annotateEx k p e = do
    let checkState = AnnoState {degree = k, scps = p, comp = mempty, cost = constant}
    (([], q, q'), cs) <- evalRWST (anno e) checkState 0
    return $ makeEqn 0 (to_ctx q) cs
