{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ratl.Anno (
    annotate,
    annotateEx
) where

import Data.List (transpose, intersect, union, inits)
import Data.Mapping (Mapping(..))
import Data.Maybe (isJust, fromJust, mapMaybe)
import Data.Foldable (traverse_, foldrM)
import Data.Traversable (for)
import Control.Arrow (first, second)
import Control.Monad (zipWithM, mfilter)
import Control.Monad.Except (MonadError(..))
import Control.Monad.State (MonadState, evalStateT, get, put)
import Control.Monad.Reader (MonadReader, runReaderT, asks, local)
import Control.Monad.Writer (MonadWriter, runWriterT, execWriterT, tell)
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
    )
import Language.Ratl.Index (
    Indexable,
    Index,
    ContextIndex,
    poly,
    deg,
    index,
    indexDeg,
    shift,
    projectionsDeg,
    )
import Language.Ratl.Ty (
    Ty(..),
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
type CIxEnv = LinearFunFamily ContextIndex
type VarEnv = [(Var, IxEnv)]
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

data AnnoError = ProjectionError Ty Index

instance Show AnnoError where
    show (ProjectionError t i) = "Index " ++ show i ++ " does not appear to be a projection of type " ++ show t ++ "."

-- IxEnv Helpers

isZero :: Indexable i t => i -> Bool
isZero = (0 ==) . deg

coerceZero :: Indexable i t => LinearFunFamily i -> LinearFunction
coerceZero = fromJust . lookupBy isZero

filterZero :: Indexable i t => LinearFunFamily i -> LinearFunFamily i
filterZero = deleteBy (not . isZero)

updateZero :: Indexable i t => LinearFunction -> LinearFunFamily i -> LinearFunFamily i
updateZero = updateBy isZero

-- Annotation Helpers

freshAnno :: MonadState Anno m => m LinearFunction
freshAnno = do
    q <- get
    put (q + 1)
    return $ sparse [(q, 1)]

freshIxEnv :: (MonadState Anno m, Indexable i t) => Int -> t -> m (LinearFunFamily i)
freshIxEnv k t = fromList <$> traverse (\i -> (,) i <$> freshAnno) (indexDeg k t)

freshBounds :: (MonadReader AnnoState m, MonadState Anno m) => Ty -> m (IxEnv, IxEnv)
freshBounds t = do
    k <- degreeof
    q  <- freshIxEnv k t
    q' <- rezero q
    return (q, q')

freshFunBounds :: MonadState Anno m => Int -> TypedFun -> m (CIxEnv, IxEnv)
freshFunBounds k fun = do
    let Arrow t t' = tyOf fun
    q  <- freshIxEnv k t
    q' <- freshIxEnv k t'
    return (q, q')

rezero :: (MonadState Anno m, Indexable i t) => LinearFunFamily i -> m (LinearFunFamily i)
rezero qs = do
    q_0' <- freshAnno
    return $ updateZero q_0' qs

reannotate :: MonadState Anno m => IxEnv -> m IxEnv
reannotate ixs = fromList <$> traverse (traverse $ const freshAnno) (elements ixs)

-- Constraint Helpers

buildPoly :: [CIxEnv] -> [[[(ContextIndex, Index)]]] -> [[[(Index, LinearFunction)]]]
buildPoly = zipWith $ \qs -> concatMap $ map pure . flip mapMaybe (elements qs) . xlate
    where xlate ixs (ix, lf) = (\ix' -> (ix', lf |* (poly ix / poly ix'))) <$> lookup ix ixs

nonEmptyConstraints c ixs k =
    case (mfilter (not . null) $          lookupBy isZero ixs,
           filter (not . null) $ values $ deleteBy isZero ixs) of
        (Just z, nz) -> (z `c` k):map (`c` 0) nz
        (     _, nz) ->           map (`c` 0) nz

(==*) :: Indexable i t => LinearFunFamily i -> Double -> [GeneralConstraint]
(==*) = nonEmptyConstraints (==$)

(>=*) :: Indexable i t => LinearFunFamily i -> Double -> [GeneralConstraint]
(>=*) = nonEmptyConstraints (>=$)

infix 4 ==*
infix 4 >=*

-- Sharing Helpers

share :: (MonadWriter [GeneralConstraint] m, MonadState Anno m) => VarEnv -> VarEnv -> m VarEnv
share fvas fvbs = do
    let vs = intersect (keys fvas) (keys fvbs)
    combined <- for vs $ \v -> do
        let Just qa = lookup v fvas
            Just qb = lookup v fvbs
        qc <- reannotate qa
        constrain $ qc |-| qa |-| qb ==* 0
        return (v, qc)
    return $ combined ++ foldr delete (fvas ++ fvbs) vs

shareSubtype :: (MonadWriter [GeneralConstraint] m, MonadState Anno m) => VarEnv -> VarEnv -> m VarEnv
shareSubtype fvas fvbs = do
    let vs = intersect (keys fvas) (keys fvbs)
    combined <- for vs $ \v -> do
        let Just qa = lookup v fvas
            Just qb = lookup v fvbs
        qc <- reannotate qa
        constrain $ qc |-| qa >=* 0
        constrain $ qc |-| qb >=* 0
        return (v, qc)
    return $ combined ++ foldr delete (fvas ++ fvbs) vs

shareBind :: (MonadWriter [GeneralConstraint] m, MonadState Anno m) => VarEnv -> VarEnv -> m VarEnv
shareBind fvas fvbs = do
    let vs = intersect (keys fvas) (keys fvbs)
    for vs $ \v -> do
        let Just qa = lookup v fvas
            Just qb = lookup v fvbs
        constrain $ qa |-| qb ==* 0
    return $ foldr delete fvbs vs

to_ctx :: Int -> Ty -> IxEnv -> CIxEnv
to_ctx k ty q = q <<< concat (concat $ projectionsDeg k [ty])

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

annoSCP :: (MonadError AnnoError m, MonadReader AnnoState m, MonadWriter [GeneralConstraint] m, MonadState Anno m) => FunEnv -> m ()
annoSCP = traverse_ (traverse_ annoFE)
    where annoFE :: (MonadError AnnoError m, MonadReader AnnoState m, MonadWriter [GeneralConstraint] m, MonadState Anno m) => (TypedFun, (CIxEnv, IxEnv)) -> m ()
          annoFE (TypedFun (Arrow pty rty) x e, (pqs, rqs)) = do
              xqs <- rezero pqs
              (fvs, q, q') <- anno e
              k <- degreeof
              shareBind (zip [x] [xqs <<< map (\(a,b) -> (b,a)) (concat $ concat $ projectionsDeg k pty)]) fvs
              constrain $ [coerceZero pqs |-| coerceZero q ==$ 0]
              constrain $ rqs |-| q' ==* 0
          annoFE (TypedNative (Arrow [pty@(ListTy pt)]          rt) _ _, (pqs, rqs)) | pt == rt = consShift pty rqs lz lz pqs -- hack for car
          annoFE (TypedNative (Arrow [pty@(ListTy pt)] (ListTy rt)) _ _, (pqs, rqs)) | pt == rt = consShift pty lz rqs lz pqs -- hack for cdr
          annoFE (TypedNative (Arrow [_, ListTy _]  rty@(ListTy _)) _ _, (pqs, rqs))            = consShift rty lz lz pqs =<< (to_ctx <$> degreeof <*> pure rty <*> pure rqs) -- hack for cons
          annoFE (TypedNative (Arrow ty ty') _ _, (pqs, rqs)) = do
              constrain $ [coerceZero pqs |-| coerceZero rqs ==$ 0]
          lz :: LinearFunFamily i
          lz = fromList []
          consShift :: (MonadError AnnoError m, MonadReader AnnoState m, MonadWriter [GeneralConstraint] m, MonadState Anno m) => Ty -> IxEnv -> IxEnv -> CIxEnv -> CIxEnv -> m ()
          consShift ty_l@(ListTy ty_h) qs_h qs_t qs_p qs_l = do
              let ty_p = [ty_h, ty_l]
              k <- degreeof
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
    anno :: (MonadError AnnoError m, MonadReader AnnoState m, MonadWriter [GeneralConstraint] m, MonadState Anno m) => a -> m (VarEnv, IxEnv, IxEnv)

instance Annotate TypedEx where
    anno (TypedVar ty x) = do
        (q, q') <- freshBounds ty
        k <- costof k_var
        constrain $ q |-| q' ==* k
        return ([(x, q)], q, q')
    anno (TypedVal ty v) = do
        (q, q') <- freshBounds ty
        k <- costof k_val
        constrain $ q |-| q' ==* k
        return ([], q, q')
    anno (TypedIf ty ep et ef) = do
        (fvps, qp, qp') <- anno ep
        (fvts, qt, qt') <- anno et
        (fvfs, qf, qf') <- anno ef
        fvs <- share fvps =<< shareSubtype fvts fvfs
        (q, q') <- freshBounds ty
        [kp, kt, kf, kc] <- sequence [costof k_ifp, costof k_ift, costof k_iff, costof k_ifc]
        constrain [coerceZero q   |-| coerceZero qp >=$ kp]
        constrain [coerceZero qp' |-| coerceZero qt >=$ kt]
        constrain [coerceZero qp' |-| coerceZero qf >=$ kf]
        constrain $ qt' |-| q' >=* kc
        constrain $ qf' |-| q' >=* kc
        return (fvs, q, q')
    anno (TypedApp _ f es) = do
        (fves, qs, qs') <- unzip3 <$> traverse anno es
        degree <- degreeof
        let tys = map tyGet es
        (Arrow ty _, (qf, qf')) <- lookupThisSCP f >>= \case
            Just (asc, (qa, qa')) -> do
                cost_free <- costof (== zero)
                let Arrow ty ty' = tyOf asc
                    theta = Elab.solve tys (ty ++ [ty'])
                    fun = Elab.instantiate theta asc
                if degree <= 1 || cost_free then
                    return (tyOf fun, (qa, qa'))
                else do
                    scp <- asks comp
                    (qf, qf') <- freshFunBounds degree fun
                    -- this is cheating for polymorphic mutual recursion; should instantiate tys over the scp somehow
                    cfscp <- traverse (traverse $ \(f, _) -> (,) f <$> freshFunBounds (degree - 1) f) $ update f (fun, (qf, qf')) scp
                    let Just (_, (qcf, qcf')) = lookup f cfscp
                    constrain $ qf  |-| qa  |-| qcf  ==* 0
                    constrain $ qf' |-| qa' |-| qcf' ==* 0
                    local (\cf -> cf {degree = degree - 1, comp = cfscp, cost = zero}) $ annoSCP cfscp
                    return (tyOf fun, (qf, qf'))
            Nothing -> do
                scp <- lookupSCP f
                let asc = fromJust $ lookup f scp
                    Arrow ty ty' = tyOf asc
                    theta = Elab.solve tys (ty ++ [ty'])
                    fun = Elab.instantiate theta asc
                scp' <- traverse (traverse $ \f -> (,) f <$> freshFunBounds degree f) $ update f fun scp
                local (\cf -> cf {comp = scp'}) $ annoSCP scp'
                return $ first tyOf $ fromJust $ lookup f scp'
        let itys = tail $ inits ty
            pis = map (transpose . last . projectionsDeg degree) itys
        (fvxs, qxs, qxs') <- local (\cf -> cf {cost = zero}) $ unzip3 <$> do
            let flippedZipWithM a b c f = zipWithM f a (zip b c)
            let ess = zipWith (map . const) es pis
            flippedZipWithM ess qs qs' $ \es (qx_0, qx'_0) -> do
                (fvxs_js, qxs_j, qx's_j) <- unzip3 <$> mapM anno (tail es)
                fvxs_j <- foldrM share [] fvxs_js
                return $ (fvxs_j, qx_0:qxs_j, qx'_0:qx's_j)
        qt <- rezero qf
        qts <- foldrM ((\ixs envs -> (:envs) <$> ixs) . freshIxEnv degree) [qt] $ init itys
        q  <- rezero qf'
        q' <- rezero qf'
        let qxrs' = map (foldl1 (|+|)) $ zipWith (zipWith (<<<)) qxs' pis
        k1 <- costof k_ap1
        k2 <- costof k_ap2
        c  <- freshAnno
        constrain [q_in |-| q_out ==$ 0.0 |
                   (q_in, q_out) <- concat $ zipWith zip (map (map coerceZero) qxs) $ [coerceZero q]:map values qts]
        constrain $ concat [q_in |-| q_out ==* k1 | (q_in, q_out) <- zip qxrs' qts]
        constrain [(coerceZero $ last qts) |-| coerceZero qf |-| c ==$ k1]
        constrain [c |+| coerceZero qf' |-| coerceZero q' ==$ k2]
        fvs <- foldrM share [] $ fves ++ fvxs
        return (fvs, q, q')
    anno (TypedLet _ ds e) = do
        (xs, (fvds, qs, qs')) <- second unzip3 <$> unzip <$> traverse (traverse anno) ds
        qxs <- traverse rezero qs
        (fves, qe, qe') <- anno e
        fves' <- shareBind (zip xs qxs) fves
        fvs <- foldrM share fves' fvds
        q  <- rezero qe
        q' <- rezero qe'
        k1 <- costof k_lt1
        k2 <- costof k_lt2
        constrain [coerceZero q_in |-| coerceZero q_out >=$ k1 |
                   (q_in, q_out) <- zip (q:qs') (qs ++ [qe])]
        constrain $ filterZero qe' |-| filterZero q' ==* k2
        return (fvs, q, q')

makeEqn :: Int -> CIxEnv -> [GeneralConstraint] -> [Ty] -> Eqn
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
    let checkState = AnnoState {degree = k, scps = p, comp = scp', cost = constant}
    cs <- execWriterT $ runReaderT (annoSCP scp') checkState
    for scp' $ traverse $ \(fun, (pqs, _)) -> do
        let Arrow pty _ = tyOf fun
        return $ makeEqn k pqs cs pty

annotateEx :: MonadError AnnoError m => Int -> [TypedProg] -> TypedEx -> m Eqn
annotateEx k p e = flip evalStateT 0 $ do
    let checkState = AnnoState {degree = k, scps = p, comp = mempty, cost = constant}
    (([], q, q'), cs) <- runWriterT $ runReaderT (anno e) checkState
    return $ makeEqn 0 (to_ctx k (tyGet e) q) cs [tyGet e]
