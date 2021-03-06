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

import Control.Arrow (first, second, (***), (&&&))
import Control.Monad (when, mfilter, replicateM)
import Control.Monad.RWS (MonadRWS, evalRWST)
import Control.Monad.RWS.Extra (MonadRS, evalRWT)
import Control.Monad.Reader (MonadReader, asks, local)
import Control.Monad.State (MonadState, evalStateT, get, put)
import Control.Monad.Writer (MonadWriter, tell)
import Data.Foldable (foldrM, for_)
import Data.Function (on)
import Data.List (sortBy, nub, union, (\\))
import Data.Mapping (Mapping(..), partition, substitute, mapInvert, selectAll, deleteAll, partitionAll, member)
import Data.Maybe (isJust, fromJust)
import Data.Semigroup ((<>))
import Data.Traversable (for)
import Data.Tuple (swap)
import Numeric.Algebra (
    Additive(..),
    Semiring,
    Group(..),
    Monoidal,
    LeftModule(..),
    RightModule(..),
    sum)
import qualified Numeric.Algebra as Alg (zero)
import Numeric.Optimization.Bankroll.LinearFunction (
    LinearFunction,
    sparse,
    coefficients,
    )
import Numeric.Optimization.Bankroll.Program (
    OptimizationDirection(Minimize),
    GeneralConstraint, (==$), (>=$),
    GeneralForm(..),
    )
import Prelude hiding (lookup, sum, negate, (+), (-))

import Language.Ratl.LinearFunFamily (LinearFunFamily)
import Language.Ratl.Index (
    Indexable,
    Index,
    ContextIndex,
    context,
    splitPairs,
    deg,
    index,
    indexDeg,
    zeroIndex,
    transform,
    shift,
    projectDeg,
    shareCoef,
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
    freeVars,
    )
import Language.Ratl.Elab (
    Unifiable(uid),
    unify,
    )
import qualified Language.Ratl.Elab as Elab (
    subst,
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
type VarEnv = ([(LNVar, Ty)], CIxEnv)
type FunEnv = [(Var, (TypedFun, (VarEnv, IxEnv)))]
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

data AnnoConfig = AnnoConfig {
        degree :: Int,
        scps :: [TypedProg],
        comp :: FunEnv,
        cost :: Cost
    }

-- Locally Nameless Representation Helpers

bindvars :: [a] -> [(LNVar, a)]
bindvars =
    let boundvars = (map BVar [0..])
    in \xs -> reverse $ zip boundvars $ reverse xs

varclose :: [Var] -> VarEnv -> VarEnv
varclose = first . (uncurry zip .) . (. unzip) . first . substitute . map swap . bindvars . map FVar

-- IxEnv Helpers

isZero :: Indexable i t => i -> Bool
isZero = (0 ==) . deg

coerceZero :: Indexable i t => IndexEnv t i -> LinearFunction
coerceZero = fromJust . lookupBy isZero . eqns

updateZero :: Indexable i t => LinearFunction -> IndexEnv t i -> IndexEnv t i
updateZero f q = q {eqns = updateBy isZero (Just $ zeroIndex $ ixTy q) f $ eqns q}

zeroEnv :: Indexable i t => t -> LinearFunction -> IndexEnv t i
zeroEnv t c = IndexEnv t $ fromList [(zeroIndex t, c)]

-- Annotation Helpers

freshAnno :: MonadState Anno m => m LinearFunction
freshAnno = do
    q <- get
    put (q + 1)
    return $ sparse [(q, 1)]

freshIxEnv :: (MonadRS AnnoConfig Anno m, Indexable i t) => t -> m (IndexEnv t i)
freshIxEnv t = do
    k <- degreeof
    IndexEnv t <$> fromList <$> traverse (\i -> (,) i <$> freshAnno) (indexDeg k t)

freshFunBounds :: MonadRS AnnoConfig Anno m => TypedFun -> m (VarEnv, IxEnv)
freshFunBounds fun = do
    let Arrow t t' = tyOf fun
    q  <- freshIxEnv t
    q' <- freshIxEnv t'
    return ((bindvars t, q), q')

freshFunEnv :: MonadRS AnnoConfig Anno m => TypedProg -> m FunEnv
freshFunEnv = traverse (traverse $ \f -> (,) f <$> freshFunBounds f)

refreshFunEnv :: MonadRS AnnoConfig Anno m => FunEnv -> m FunEnv
refreshFunEnv = freshFunEnv . map (second fst)

rezero :: (MonadState Anno m, Indexable i t) => IndexEnv t i -> m (IndexEnv t i)
rezero qs = do
    q_0' <- freshAnno
    return $ updateZero q_0' qs

-- Constraint Helpers

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

-- Projection Helpers

both :: (a -> b) -> (a, a) -> (b, b)
both f = f *** f

discardLeft :: MonadReader AnnoConfig m => CIxEnv -> m CIxEnv
discardLeft q@(IndexEnv [] _) = return q
discardLeft (IndexEnv (ty:tys) q) = IndexEnv tys <$> q'
    where q' = fromList . map (first (transform tail)) . elements . flip selectAll q . map (zeroIndex [ty] <>) <$> (indexDeg <$> degreeof <*> pure tys)

discardRight :: MonadReader AnnoConfig m => CIxEnv -> m CIxEnv
discardRight q@(IndexEnv [] _) = return q
discardRight (IndexEnv tys q) = IndexEnv tys' <$> q'
    where q' = fromList . map (first (transform init)) . elements . flip selectAll q . map (<> zeroIndex [ty]) <$> (indexDeg <$> degreeof <*> pure tys')
          (ty, tys') = (last tys, init tys)

augment :: (LNVar, Ty) -> VarEnv -> VarEnv
augment (x, _) q@(gamma, _) | x `member` gamma = q
augment (x, ty) (gamma, IndexEnv tys q) = ((x, ty):gamma, IndexEnv (ty:tys) q')
    where q' = fromList $ map (first (zeroIndex [ty] <>)) $ elements q

augmentMany :: ([LNVar], [Ty]) -> VarEnv -> VarEnv
augmentMany (ys, tys) q = foldr augment q $ zip ys tys

-- Returns a list grouped by _j of mappings from the projection pi_j^gamma to the injection gamma
projectOver :: MonadReader AnnoConfig m => [Ty] -> [Ty] -> m [[(ContextIndex, ContextIndex)]]
projectOver jtys gtys = map (\(ix, ixs) -> map ((ix <>) &&& id) ixs)
                        <$> (projectDeg <$> degreeof <*> pure jtys <*> pure gtys)

-- Returns a list grouped by _j of mappings from the projection to the injection, where the projection
-- is ordered according to the first argument and the injection is ordered according to the second.
projectNames :: MonadReader AnnoConfig m => [(LNVar, Ty)] -> [LNVar] -> m [[(ContextIndex, ContextIndex)]]
projectNames g_y xs = do
    let ((xis, xtys), (yis, ytys)) = both (unzip . values) $ first (flip concatMap xs . flip select) $ partitionAll xs $ zipWith (\n (y, t) -> (y, (n, t))) [1..] g_y
    map (map $ first $ transform $ values . sortBy (compare `on` fst) . zip (yis ++ xis)) <$> projectOver ytys xtys

share :: MonadRWS AnnoConfig [GeneralConstraint] Anno m => VarEnv -> m VarEnv
share (gamma, q) = foldrM shareOne (gamma, q) repeats
    where repeats = nub $ xs \\ nub xs
          xs = keys gamma
          shareOne x (gamma, q) = do
            k <- degreeof
            let ((xty:_), ytys) = both values $ partition x gamma
                gamma' = delete x gamma ++ [(x, xty)]
                restrict :: [(LNVar, Ty)] -> [(ContextIndex, ContextIndex)]
                restrict g = map (id &&& transform (values . delete x . zip (keys g))) $ indexDeg k $ values g
                shrink = (mapInvert (restrict gamma') :: [(ContextIndex, [ContextIndex])]) <<< restrict gamma
            pi_ijs <- concat <$> projectNames gamma [x]
            pi_k <- concat <$> projectOver ytys [xty]
            let qks = do
                    (cix_ijs, cix_ks) <- shrink
                    let Just ix_ijs = lookup cix_ijs pi_ijs
                        lf_ij = maybe Alg.zero id $ lookup cix_ijs $ eqns q
                        cs_k :: [(ContextIndex, Double)]
                        cs_k = shareCoef ix_ijs <<< selectBy (`elem` cix_ks) pi_k
                    return $ fromList $ map (second (lf_ij *.)) cs_k
            q' <- freshIxEnv $ values gamma'
            constrain $ q' - sum (map (IndexEnv (ixTy q')) qks) ==* 0
            return (gamma', q')

pack :: IxEnv -> CIxEnv
pack q = IndexEnv [ixTy q] $ fromList $ map (first $ context . pure) $ elements $ eqns q

expand_ctx :: CIxEnv -> CIxEnv
expand_ctx q = IndexEnv (concatMap unPairTy $ ixTy q) $ fromList $ map (first splitPairs) $ elements $ eqns q
    where unPairTy (PairTy t1 t2) = [t1, t2]
          unPairTy t = [t]

-- Reader/Writer/State Helpers

lookupSCP :: MonadReader AnnoConfig m => Var -> m TypedProg
lookupSCP x = asks (head . filter (isJust . lookup x) . scps)

constrain :: MonadWriter [GeneralConstraint] m => [GeneralConstraint] -> m ()
constrain = tell

costof :: MonadReader AnnoConfig m => (Cost -> a) -> m a
costof k = asks (k . cost)

degreeof :: MonadReader AnnoConfig m => m Int
degreeof = asks degree

lookupThisSCP :: MonadReader AnnoConfig m => Var -> m (Maybe (TypedFun, (VarEnv, IxEnv)))
lookupThisSCP x = asks (lookup x . comp)

-- The Engine

annoSequential :: MonadRWS AnnoConfig [GeneralConstraint] Anno m => [((Cost -> Double, Cost -> Double), TypedEx)] -> VarEnv -> m VarEnv
annoSequential kes q_0 =
    let recurses :: MonadReader AnnoConfig m => TypedEx -> m Bool
        recurses (TypedVar _ _) = return False
        recurses (TypedVal _ _) = return False
        recurses (TypedApp _ f es) = maybe (anyRecurses es) (const $ return True) =<< lookupThisSCP f
        recurses (TypedIf _ ep et ef) = anyRecurses [ep, et, ef]
        recurses (TypedLet _ bs e) = anyRecurses (e:map snd bs)
        anyRecurses :: MonadReader AnnoConfig m => [TypedEx] -> m Bool
        anyRecurses = fmap or . traverse recurses
    in do
    q <- flip (flip foldrM q_0) (bindvars kes) $ \(i, ((ke_i, ke'_i), e_i)) (g_im1, q_im1) -> do
        ((g_i_0, q_i_0), q'_i_0) <- second pack <$> anno e_i
        let g_d_im1 = deleteAll [i] g_im1
            g_i = g_d_im1 ++ g_i_0
            (g_a_im1, q_a_im1) = augment (i, head $ ixTy q'_i_0) (g_im1, q_im1)
        q_i <- freshIxEnv $ values g_i
        ~(pi_g_i_0_0:pi_g_i_j_js) <- map (map swap) <$> projectOver (values g_d_im1) (values g_i_0)
        ~(pi_i_t_i_0:pi_i_t_i_js) <- map (map swap) <$> projectNames g_a_im1 [i]
        k_i  <- costof ke_i
        k'_i <- costof ke'_i
        constrain $ (q_i_0 {eqns = eqns q_i <<< pi_g_i_0_0}) - q_i_0 ==* k_i
        constrain $ q'_i_0 - (q'_i_0 {eqns = eqns q_a_im1 <<< pi_i_t_i_0}) ==* k'_i
        recs <- recurses e_i
        degree <- degreeof
        for (zip pi_g_i_j_js pi_i_t_i_js) $ \(pi_g_i_j_j@((j_i_0, j_i):_), pi_i_t_i_j) -> do
            ((_, q_i_j), q'_i_j) <- local (\s -> s {degree = degree - (deg j_i - deg j_i_0), cost = zero}) $ do
                if not recs || degree < 1 then
                    second pack <$> anno e_i
                else do
                    -- if es contains recursion, reannotate everything in the SCC
                    -- required because it handles each possible world separately
                    scp <- asks comp
                    cfscp <- refreshFunEnv scp
                    local (\s -> s {comp = cfscp}) $ do
                        traverse anno cfscp
                        second pack <$> anno e_i
            constrain $ (q_i_j {eqns = eqns q_i <<< pi_g_i_j_j}) - q_i_j ==* 0
            constrain $ q'_i_j - (q'_i_j {eqns = eqns q_a_im1 <<< pi_i_t_i_j}) ==* 0
        share (g_i, q_i)
    return q

annoParallel :: MonadRWS AnnoConfig [GeneralConstraint] Anno m => [((Cost -> Double, Cost -> Double), TypedEx)] -> m (VarEnv, IxEnv)
annoParallel kes = do
    kqs <- traverse (traverse anno) kes
    let gs = map (fst . fst . snd) kqs
        g = foldl union [] gs
        ty = values g
        ty' = ixTy $ snd $ snd $ head kqs
    pis <- traverse (fmap head . projectNames g) $ map keys gs
    q  <- freshIxEnv $ values g
    q' <- freshIxEnv ty'
    for (zip pis kqs) $ \(pi, ((ke, ke'), ((_, qe), qe'))) -> do
        k  <- costof ke
        k' <- costof ke'
        let qp = IndexEnv ty $ eqns qe <<< pi
        constrain $ q   - qp >=* k
        constrain $ qe' - q' >=* k'
    return ((g, q), q')

class Annotate a where
    anno :: MonadRWS AnnoConfig [GeneralConstraint] Anno m => a -> m (VarEnv, IxEnv)

instance Annotate (Var, (TypedFun, (VarEnv, IxEnv))) where
   anno f@(_, (_, qs)) = annoFun f >> return qs
    where annoFun :: MonadRWS AnnoConfig [GeneralConstraint] Anno m => (Var, (TypedFun, (VarEnv, IxEnv))) -> m ()
          annoFun (_, (TypedFun (Arrow pty rty) xs e, (pqs, rqs))) = do
              (q, q') <- anno e
              let ys = map FVar xs
                  gamma = zip ys pty
                  (gamma_e, qa) = augmentMany (ys, pty) q
              pi <- projectNames gamma $ keys gamma_e
              constrain $ snd pqs - snd (varclose xs (gamma, IndexEnv pty $ eqns qa <<< concat pi)) ==* 0
              constrain $ rqs - q' ==* 0
          annoFun (V "car",   (TypedNative (Arrow [ty_l@(ListTy ty_h)] _) _, ((_, pqs), rqs))) = freshIxEnv [ty_h, ty_l] >>= \qp -> consShift qp pqs >> constrainCar qp (pack rqs)
          annoFun (V "cdr",   (TypedNative (Arrow [ty_l@(ListTy ty_h)] _) _, ((_, pqs), rqs))) = freshIxEnv [ty_h, ty_l] >>= \qp -> consShift qp pqs >> constrainCdr qp (pack rqs)
          annoFun (V "cons",  (TypedNative _                              _, ((_, pqs), rqs))) =                                    consShift pqs (pack rqs)
          annoFun (V "pair",  (TypedNative _                              _, ((_, pqs), rqs))) = constrain $ pqs - expand_ctx (pack rqs) ==* 0
          annoFun (V "fst",   (TypedNative _                              _, ((_, pqs), rqs))) = constrainCar (expand_ctx pqs) (pack rqs)
          annoFun (V "snd",   (TypedNative _                              _, ((_, pqs), rqs))) = constrainCdr (expand_ctx pqs) (pack rqs)
          annoFun (V "error", (TypedNative _                              _, _              )) = return ()
          annoFun (_,         (TypedNative _                              _, ((_, pqs), rqs))) = constrain $ [pqs %-% rqs ==$ 0]
          consShift :: MonadRWS AnnoConfig [GeneralConstraint] Anno m => CIxEnv -> CIxEnv -> m ()
          consShift qp ql = do
              let limit (i, is) = const (i, is) <$> lookup i (eqns qp)
                  Just shs = sequence $ takeWhile isJust $ map limit $ shift $ head $ ixTy ql
                  ss = map (\(i, is) -> (,) <$> lookup i (eqns qp) <*> sequence (filter isJust $ map (flip lookup $ eqns ql) is)) shs
              constrain [sum ps - q ==$ 0 |
                         Just (q, ps) <- ss]
          constrainCar :: MonadRWS AnnoConfig [GeneralConstraint] Anno m => CIxEnv -> CIxEnv -> m ()
          constrainCar qp qh = do
              qph <- discardRight qp
              constrain $ qh - qph ==* 0
          constrainCdr :: MonadRWS AnnoConfig [GeneralConstraint] Anno m => CIxEnv -> CIxEnv -> m ()
          constrainCdr qp qt = do
              qpt <- discardLeft qp
              constrain $ qt - qpt ==* 0

instance Annotate TypedEx where
    anno (TypedVar ty x) = do
        q' <- freshIxEnv ty
        let p = pack q'
        q  <- rezero p
        k <- costof k_var
        constrain $ q - p ==* k
        return (([(FVar x, ty)], q), q')
    anno (TypedVal ty v) = do
        q  <- freshIxEnv []
        q' <- freshIxEnv ty
        k <- costof k_val
        constrain [q %-% q' ==$ k]
        return (([], q), q')
    anno (TypedIf ty ep et ef) = do
        (qc, q') <- annoParallel [((k_ift, k_ifc), et), ((k_iff, k_ifc), ef)]
        q <- annoSequential [((const 0, k_ifp), ep)] qc
        return (q, q')
    anno (TypedApp ty f es) = do
        let tys = map tyGet es
        (p, p', q, q') <- lookupThisSCP f >>= \case
            Just (asc, (p, p')) -> do
                cost_free <- costof (== zero)
                degree <- degreeof
                if degree <= 1 || cost_free then do
                    q <- traverse rezero p
                    q' <- rezero p'
                    c  <- freshAnno
                    return (zeroEnv (ixTy $ snd p) c + (snd p), zeroEnv (ixTy p') c + p', q, q')
                else do
                    scp <- asks comp
                    let Right theta = Elab.solve (Arrow tys ty) $ tyOf asc
                        fun = Elab.subst theta asc
                    (q, q') <- freshFunBounds fun
                    local (\s -> s {degree = degree - 1, cost = zero}) $ do
                        -- this is cheating for polymorphic mutual recursion; should subst tys over the scp somehow
                        cfscp <- refreshFunEnv $ update f (fun, (q, q')) scp
                        local (\s -> s {comp = cfscp}) $ traverse anno cfscp
                        let Just (_, (pcf, pcf')) = lookup f cfscp
                        return (snd p + snd pcf, p' + pcf', q, q')
            Nothing -> do
                scp <- lookupSCP f
                let asc = fromJust $ lookup f scp
                    Right theta = Elab.solve (Arrow tys ty) $ tyOf asc
                    fun = Elab.subst theta asc
                scp' <- freshFunEnv $ update f fun scp
                local (\cf -> cf {comp = scp'}) $ traverse anno scp'
                let Just (_, (p, p')) = lookup f scp'
                q <- traverse rezero p
                q' <- rezero p'
                return (snd p, p', q, q')
        k1 <- costof k_ap1
        k2 <- costof k_ap2
        constrain $ snd q - p ==* k1
        constrain $    p' - q' ==* k2
        q  <- annoSequential (zip (repeat (const 0, k_ap1)) es) q
        return (q, q')
    anno (TypedLet _ bs e) = do
        let (xs, es) = unzip bs
        (qe, qe') <- anno e
        qb <- annoSequential (zip (repeat (const 0, k_lt1)) es) (varclose xs $ augmentMany (map FVar xs, map tyGet es) qe)
        q  <- traverse rezero qb
        q' <- rezero qe'
        k1 <- costof k_lt1
        k2 <- costof k_lt2
        constrain $ snd q - snd qb ==* k1
        constrain $   qe' -     q' ==* k2
        return (q, q')

makeEqn :: Int -> CIxEnv -> [GeneralConstraint] -> Eqn
makeEqn k (IndexEnv ty q) cs =
    let objective = sum . map (fromJust . flip lookup q)
        program = flip (GeneralForm Minimize) cs . objective
        progs = reverse $ take (k + 1) $ map program $ index ty
        resource = (\[a] -> a) . fst . coefficients
        indexmap = map (fmap resource) $ elements q
    in (indexmap, progs)

annotate :: Monad m => Int -> [TypedProg] -> [TypedProg] -> m EqnEnv
annotate k p fs = do
    let checkState = AnnoConfig {degree = k, scps = p, comp = mempty, cost = constant}
    flip evalStateT 0 $ fmap concat $ for fs $ \scp -> do
        (scp', cs) <- flip evalRWT checkState $ do
            scp' <- freshFunEnv scp
            local (\s -> s {comp = scp'}) $ traverse anno scp'
            return scp'
        return $ map (fmap $ \(_, ((_, pqs), _)) -> makeEqn k pqs cs) scp'

annotateEx :: Monad m => Int -> [TypedProg] -> TypedEx -> m Eqn
annotateEx k p e = do
    let checkState = AnnoConfig {degree = k, scps = p, comp = mempty, cost = constant}
    (((_, q), q'), cs) <- flip (flip evalRWST checkState) 0 $ do
        let gamma = map (first FVar) $ freeVars e
        qe <- freshIxEnv $ values gamma
        ((gamma_e, q), q') <- anno e
        pi <- projectNames gamma $ keys gamma_e
        constrain $ qe - (IndexEnv (ixTy qe) $ eqns q <<< concat pi) ==* 0
        return ((gamma, qe), q')
    return $ makeEqn 0 q cs
