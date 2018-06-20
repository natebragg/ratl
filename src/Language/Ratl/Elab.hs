{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ratl.Elab (
    check,
    checkEx,
) where

import Data.List (intersect, intercalate, union, nub, foldl', transpose, tails, unionBy, groupBy, sortBy)
import Data.Function (on)
import Data.Map (foldMapWithKey, traverseWithKey, elems, fromList, toList, mapKeys)
import Data.Map.Monoidal (MonoidalMap(..), singleton, keys)
import Data.Maybe (listToMaybe, isJust, fromJust, mapMaybe)
import Data.Foldable (traverse_)
import Data.Traversable (for)
import Control.Applicative (empty)
import Control.Arrow (first, second, (***), (&&&))
import Control.Monad (when, void, zipWithM)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Except.Extra (unlessJust, unlessJustM)
import Control.Monad.State (MonadState, evalStateT)
import Control.Monad.Reader (MonadReader, runReaderT, withReaderT, mapReaderT, ReaderT, asks)
import Control.Monad.Writer (MonadWriter, runWriterT, execWriterT, mapWriterT, WriterT, tell)

import Data.Clp.Clp (OptimizationDirection(Minimize))
import Data.Clp.LinearFunction (LinearFunction, sparse)
import Data.Clp.Program (
    GeneralConstraint(..),
    GeneralForm(..),
    Objective,
    )
import Language.Ratl.Anno (
    Anno,
    reannotate,
    freshAnno,
    )
import Language.Ratl.Index (
    Index,
    poly,
    index,
    indexDeg,
    zeroIndex,
    shift,
    inject,
    projectionsDeg,
    )
import Language.Ratl.Ty (
    Ty(..),
    varname,
    varnum,
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
    Prog,
    tyOf,
    lookupFun,
    updateFun,
    travFun,
    )
import Language.Ratl.Basis (arity)


type IxEnv a = [(Index, a)]
type TyEnv a = [(Var, (Ty, IxEnv a))]
type FunEnv a = [(Var, (Fun, (IxEnv a, IxEnv a)))]
type SharedTys a = MonoidalMap (IxEnv a) [IxEnv a]
type TyvarEnv = [(String, Ty)]
type Eqn = (IxEnv Anno, [GeneralForm])
type EqnEnv = [(Var, Eqn)]

update :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
update a b [] = [(a,b)]
update a b ((a',_):abs) | a == a' = (a,b):abs
update a b (ab:abs) = ab:update a b abs

delete :: Eq a => a -> [(a, b)] -> [(a, b)]
delete a abs = filter ((a /=) . fst) abs

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

freein :: Ty -> [String]
freein             NatTy = []
freein       (ListTy ty) = freein ty
freein (PairTy (t1, t2)) = freein t1 ++ freein t2
freein         BooleanTy = []
freein            UnitTy = []
freein             SymTy = []
freein         (Tyvar y) = [y]

solve :: TyvarEnv -> (Ty, Ty) -> TyvarEnv
solve theta (ty, ty') = compose (assoc (tysubst theta ty) (tysubst theta ty')) theta
  where assoc       (ListTy ty)        (ListTy ty') = assoc ty ty'
        assoc (PairTy (t1, t2)) (PairTy (t1', t2')) = assoc t1 t1' ++ assoc t2 t2'
        assoc         (Tyvar x)                 ty' = [(x, ty')]
        assoc                 _                   _ = []

compose :: TyvarEnv -> TyvarEnv -> TyvarEnv
compose theta2 theta1 = map (id &&& replace) domain
  where domain  = union (map fst theta2) (map fst theta1)
        replace = tysubst theta2 . varsubst theta1

varsubst :: TyvarEnv -> String -> Ty
varsubst theta x = maybe (Tyvar x) id $ lookup x theta

tysubst :: TyvarEnv -> Ty -> Ty
tysubst theta = subst
  where subst             NatTy = NatTy
        subst       (ListTy ty) = ListTy $ subst ty
        subst (PairTy (t1, t2)) = PairTy (subst t1, subst t2)
        subst         BooleanTy = BooleanTy
        subst            UnitTy = UnitTy
        subst             SymTy = SymTy
        subst         (Tyvar x) = varsubst theta x

class Instantiable t where
    instantiate :: [Ty] -> t -> t

instance Instantiable [Ty] where
    instantiate tys tys' = map (tysubst varenv) tys''
        where varenv = foldl' solve [] $ zip tys'' tys
              tys'' = map (tysubst env) tys'
                where env = zip (map varname $ intersect frees bound) (map (Tyvar . varname) [next_var..])
                      next_var = 1 + (maximum $ union frees bound)
                      frees = nub $ map varnum $ concatMap freein tys
                      bound = nub $ map varnum $ concatMap freein tys'

instance Instantiable Ty where
    instantiate tys ty = head $ instantiate tys $ [ty]

instance Instantiable FunTy where
    instantiate tys (Arrow ty ty') = repair $ instantiate tys $ unpair ty
        where repair [ty, ty'] = Arrow ty ty'
              repair (t1:tys) = let Arrow t2 ty' = repair tys in Arrow (PairTy (t1, t2)) ty'
              unpair (PairTy (t1, t2)) = t1:unpair t2
              unpair t = [t, ty']

instance Instantiable Fun where
    instantiate tys (Fun ty x e) = Fun (instantiate tys ty) x e
    instantiate tys (Native ty a f) = Native (instantiate tys ty) a f

freshIxEnv :: MonadState Anno m => Int -> Ty -> m (IxEnv Anno)
freshIxEnv k t = traverse (reannotate . flip (,) ()) $ indexDeg k t

freshBounds :: (MonadReader CheckE m, MonadState Anno m) => Ty -> m (IxEnv Anno, IxEnv Anno)
freshBounds t = do
    k <- degreeof
    q  <- freshIxEnv k t
    q' <- rezero t q
    return (q, q')

freshFunBounds :: MonadState Anno m => Int -> Fun -> m (IxEnv Anno, IxEnv Anno)
freshFunBounds k fun = do
    let Arrow t t' = tyOf fun
    q  <- freshIxEnv k t
    q' <- freshIxEnv k t'
    return (q, q')

rezero :: MonadState Anno m => Ty -> IxEnv Anno -> m (IxEnv Anno)
rezero ty qs = do
    q_0' <- freshAnno
    return $ update (zeroIndex ty) q_0' qs

injectAnno :: (MonadReader CheckE m, MonadError TypeError m, MonadState Anno m) => Ty -> IxEnv Anno -> m (IxEnv Anno)
injectAnno t qs = do
    qs' <- traverse (\(ix, q) -> flip (,) q <$> inject t ix `unlessJust` throwError (ProjectionError t ix)) qs
    k <- degreeof
    traverse (\ix -> (,) ix <$> lookup ix qs' `unlessJust` freshAnno) $ indexDeg k t

relate :: (LinearFunction -> Double -> GeneralConstraint) -> IxEnv Anno -> [IxEnv Anno] -> [GeneralConstraint]
relate c i is = [sparse (map exchange (Consume p:map Supply ps)) `c` 0.0 | (ix, p) <- i, ps <- [mapMaybe (lookup ix) is], not $ null ps, not $ elem p ps]

equate :: IxEnv Anno -> [IxEnv Anno] -> [GeneralConstraint]
equate = relate Eql

exceed :: IxEnv Anno -> [IxEnv Anno] -> [GeneralConstraint]
exceed = relate Geq

equatePoly :: IxEnv Anno -> [IxEnv (Anno, Double)] -> [GeneralConstraint]
equatePoly i is = [sparse (exchange (Consume p):map (fmap negate) pcs) `Eql` 0.0 | (ix, p) <- i, pcs <- [mapMaybe (lookup ix) is], not $ null pcs, not $ elem p $ map fst pcs]

data CheckE = CheckE {
        env :: TyEnv Anno,
        checkF :: CheckF
    }

data CheckF = CheckF {
        degree :: Int,
        scps :: [Prog],
        comp :: FunEnv Anno,
        cost :: Cost
    }

lookupSCP :: MonadReader CheckE m => Var -> m (Maybe Prog)
lookupSCP x = asks (listToMaybe . filter (isJust . flip lookupFun x) . scps . checkF)

share :: (Monoid a, MonadWriter (a, SharedTys b) m) => SharedTys b -> m ()
share m = tell (mempty, m)

constrain :: (Monoid b, MonadWriter ([GeneralConstraint], b) m) => [GeneralConstraint] -> m ()
constrain c = tell (c, mempty)

constrainShares :: ([GeneralConstraint], SharedTys Anno) -> [GeneralConstraint]
constrainShares = uncurry (++) . second (foldMapWithKey equate . getMonoidalMap)

gamma :: MonadReader CheckE m => Var -> m (Maybe (Ty, IxEnv Anno))
gamma x = asks (lookup x . env)

costof :: MonadReader CheckE m => (Cost -> a) -> m a
costof k = asks (k . cost . checkF)

degreeof :: MonadReader CheckE m => m Int
degreeof = asks (degree . checkF)

lookupThisSCP :: MonadReader CheckE m => Var -> m (Maybe (Fun, (IxEnv Anno, IxEnv Anno)))
lookupThisSCP x = asks (lookup x . comp . checkF)

assoc :: (a, (b, c)) -> ((a, b), c)
assoc (a, (b, c)) = ((a, b), c)

data TypeError = TypeError [(Ty, Ty)]
               | ArityError Int Int
               | NameError Var
               | ProjectionError Ty Index

instance Show TypeError where
    show (TypeError ts) = "Cannot unify unlike types: " ++ intercalate ", " (map (\(t, t') -> show t ++ " and " ++ show t') ts)
    show (ArityError f a) = "Expected " ++ show f ++ " arguments, but got " ++ show a ++ "."
    show (NameError x) = "Name " ++ show x ++ " is not defined."
    show (ProjectionError t i) = "Index " ++ show i ++ " does not appear to be a projection of type " ++ show t ++ "."

zipR :: [a] -> [b] -> ([(a,b)], ([a], [b]))
zipR     []     bs = ([], ([], bs))
zipR     as     [] = ([], (as, []))
zipR (a:as) (b:bs) = first ((a,b) :) $ zipR as bs

objective :: IxEnv Anno -> [Index] -> Objective
objective qs ixs = sparse $ map (flip (,) 1 . fromJust . flip lookup qs) ixs

check :: (MonadError TypeError m) => Int -> [Prog] -> m EqnEnv
check deg_max p = flip evalStateT 0 $ fmap concat $ for p $ \scp -> do
    scp' <- travFun (traverse $ \f -> (,) f <$> freshFunBounds deg_max f) scp
    let checkState = CheckF {degree = deg_max, scps = p, comp = scp', cost = constant}
    cs <- execWriterT $ runReaderT (elabSCP scp') checkState
    for scp' $ traverse $ \(fun, (pqs, _)) -> do
        let Arrow pty _ = tyOf fun
        progs <- for (reverse $ take (deg_max + 1) $ index pty) $ \ixs -> do
            let obj = objective pqs ixs
            return $ GeneralForm Minimize obj cs
        return (pqs, progs)

checkEx :: (MonadError TypeError m) => Int -> [Prog] -> Ex -> m Eqn
checkEx deg_max p e = flip evalStateT 0 $ do
    let checkState = CheckF {degree = deg_max, scps = p, comp = mempty, cost = constant}
    ((ty, (q, q')), css) <- runWriterT $ runReaderT (elab e) $ CheckE [] checkState
    progs <- for [[zeroIndex ty]] $ \ixs -> do
        let obj = objective q ixs
        return $ GeneralForm Minimize obj $ constrainShares css
    return (q, progs)

elabSCP :: (MonadError TypeError m, MonadState Anno m) => FunEnv Anno -> ReaderT CheckF (WriterT [GeneralConstraint] m) ()
elabSCP = traverse_ (traverse_ $ mapReaderT (mapWriterT (fmap $ second $ constrainShares)) . elabFE)
    where elabFE :: (MonadError TypeError m, MonadState Anno m) => (Fun, (IxEnv Anno, IxEnv Anno)) -> ReaderT CheckF (WriterT ([GeneralConstraint], SharedTys Anno) m) ()
          elabFE (Fun (Arrow pty rty) x e, (pqs, rqs)) = do
                    xqs <- rezero pty pqs
                    (ty, (q, q')) <- withReaderT (CheckE $ zip [x] [(pty, xqs)]) $ elab e
                    let ty'' = tysubst (solve [] (ty, rty)) ty
                    when (rty /= ty'') $
                        throwError $ TypeError $ [(rty, ty'')]
                    qsx' <- withReaderT (CheckE []) $ injectAnno ty'' q'
                    let z  = zeroIndex ty
                        Just q_0   = lookup  z q
                        pz = zeroIndex pty
                        Just pqs_0 = lookup pz pqs
                    constrain [sparse (map exchange [Consume pqs_0, Supply q_0]) `Eql` 0.0]
                    constrain $ equate rqs [qsx']
          elabFE (Native (Arrow pty@(ListTy pt) (ListTy rt)) _ _, (pqs, rqs)) | pt == rt = do -- hack for cdr
                    let Just shs = sequence $ takeWhile isJust $ map (\(((_, i), _), (i1, i2)) -> const (i, [i1, i2]) <$> lookup i1 pqs) $ shift pty
                        shmap = map ((head *** nub . concat) . unzip) $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) shs
                        ss = map (\(i, is) -> (,) <$> lookup i rqs <*> sequence (filter isJust $ map (flip lookup pqs) is)) shmap
                    constrain [sparse (map exchange (Supply q:map Consume ps)) `Eql` 0.0 |
                               Just (q, ps) <- ss, not $ q `elem` ps]
          elabFE (Native (Arrow pty@(ListTy pt) rt) _ _, (pqs, rqs)) | pt == rt = do -- hack for car
                    let Just shs = sequence $ takeWhile isJust $ map (\(((ir, _), _), (_, ip)) -> const (ir, ip) <$> lookup ip pqs) $ shift pty
                        z = zeroIndex pty
                        z' = zeroIndex rt
                        shmap = map ((head *** nub) . unzip) $ groupBy ((==) `on` fst) $ sortBy (compare `on` fst) $ (z', z):shs
                        ss = map (first $ \i -> maybe [] id $ (sequence . filter isJust . map (flip lookup pqs)) =<< lookup i shmap) rqs
                    constrain [sparse (map exchange (Supply q:map Consume ps)) `Geq` 0.0 |
                               (ps, q) <- ss, not $ q `elem` ps]
          elabFE (Native (Arrow (PairTy (tyh, ListTy tyt)) rty@(ListTy tyc)) _ _, (pqs, rqs)) = do -- hack for cons
                    let Just shs = sequence $ takeWhile isJust $ map (\((_, i), (i1, i2)) -> const (i, [i1, i2]) <$> lookup i1 rqs) $ shift rty
                        ss = map (\(i, is) -> (,) <$> lookup i pqs <*> sequence (filter isJust $ map (flip lookup rqs) is)) shs
                    constrain [sparse (map exchange (Supply q:map Consume ps)) `Eql` 0.0 |
                               Just (q, ps) <- ss, not $ q `elem` ps]
          elabFE (Native (Arrow ty ty') _ _, (pqs, rqs)) = do
                    let z = zeroIndex ty
                        Just q_0  = lookup z  pqs
                        z' = zeroIndex ty'
                        Just q_0' = lookup z' rqs
                    constrain [sparse [exchange $ Consume q_0, exchange $ Supply q_0'] `Geq` 0.0]

class Elab a where
    elab :: (MonadError TypeError m, MonadState Anno m) => a -> ReaderT CheckE (WriterT ([GeneralConstraint], SharedTys Anno) m) (Ty, (IxEnv Anno, IxEnv Anno))

instance Elab Ex where
    elab (Var x) = do
        (ty, qx) <- unlessJustM (gamma x) $
                throwError $ NameError x
        q  <- traverse reannotate qx
        q' <- rezero ty q
        share $ singleton qx [q]
        let z = zeroIndex ty
            Just q_0  = lookup z q
            Just q_0' = lookup z q'
        k <- costof k_var
        constrain [sparse [exchange $ Consume q_0, exchange $ Supply q_0'] `Geq` k]
        return (ty, (q, q'))
    elab (Val v) = do
        (ty, (q, q')) <- elab v
        k <- costof k_val
        let z = zeroIndex ty
            Just q_0  = lookup z q
            Just q_0' = lookup z q'
        constrain [sparse [exchange $ Consume q_0, exchange $ Supply q_0'] `Geq` k]
        return (ty, (q, q'))
    elab (If ep et ef) = do
        (tyep, (qp, qp')) <- elab ep
        ((tyet, (qt, qt')), (tcs, tss)) <- mapReaderT runWriterT $ elab et
        ((tyef, (qf, qf')), (fcs, fss)) <- mapReaderT runWriterT $ elab ef
        let tys = [tyep, tyet, tyef]
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
        let [typ, tyt, tyf, ty''] = instantiate tys [BooleanTy, Tyvar "a", Tyvar "a", Tyvar "a"]
        let tys' = [typ, tyt, tyf]
        let tys'' = instantiate tys' tys
        qip  <- injectAnno typ qp
        qip' <- injectAnno typ qp'
        qit  <- injectAnno tyt qt
        qit' <- injectAnno tyt qt'
        qif  <- injectAnno tyf qf
        qif' <- injectAnno tyf qf'
        (q, q') <- freshBounds ty''
        let ineqs = filter (uncurry (/=)) (zip tys'' tys')
        when (not $ null ineqs) $
            throwError $ TypeError $ ineqs
        [kp, kt, kf, kc] <- sequence [costof k_ifp, costof k_ift, costof k_iff, costof k_ifc]
        let z = zeroIndex ty''
            Just q_0  = lookup z q
            Just q_0' = lookup z q'
        let zp = zeroIndex typ
            Just qip_0  = lookup zp qip
            Just qip_0' = lookup zp qip'
        let zt = zeroIndex tyt
            Just qit_0  = lookup zt qit
            Just qit_0' = lookup zt qit'
        let zf = zeroIndex tyf
            Just qif_0  = lookup zf qif
            Just qif_0' = lookup zf qif'
        constrain $ exceed (delete zt qit) [q] ++ exceed (delete zf qif) [q]
        constrain [sparse [exchange $ Consume q_0,    exchange $ Supply qip_0] `Geq` kp,
                   sparse [exchange $ Consume qip_0', exchange $ Supply qit_0] `Geq` kt,
                   sparse [exchange $ Consume qip_0', exchange $ Supply qif_0] `Geq` kf,
                   sparse [exchange $ Consume qit_0', exchange $ Supply q_0']  `Geq` kc,
                   sparse [exchange $ Consume qif_0', exchange $ Supply q_0']  `Geq` kc]
        return (ty'', (q, q'))
    elab (App f es) = do
        (tys, (qs, qs')) <- second unzip <$> unzip <$> mapM elab es
        when (arity f /= length es) $
            throwError $ ArityError (arity f) (length es)
        degree <- degreeof
        (Arrow ty ty'', (qf, qf')) <- lookupThisSCP f >>= \case
            Just (asc, (qa, qa')) -> do
                cost_free <- costof (== zero)
                let fun = instantiate tys asc
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
                    constrain =<< withReaderT (\ce -> (checkF ce) {degree = degree - 1, comp = cfscp, cost = zero}) (mapReaderT execWriterT (elabSCP cfscp))
                    return (tyOf fun, (qf, qf'))
            Nothing -> do
                scp <- unlessJustM (lookupSCP f) $
                       throwError $ NameError f
                let fun = instantiate tys $ fromJust $ lookupFun scp f
                scp' <- travFun (traverse $ \f -> (,) f <$> freshFunBounds degree f) $ updateFun scp f fun
                constrain =<< withReaderT (\ce -> (checkF ce) {comp = scp'}) (mapReaderT execWriterT (elabSCP scp'))
                return $ first tyOf $ fromJust $ lookup f scp'
        let unpair (PairTy (t1, t2)) = t1:unpair t2
            unpair t = [t]
            tys' = unpair ty
        let tys'' = instantiate tys' tys
        qxs  <- zipWithM injectAnno tys' qs
        qxs' <- zipWithM injectAnno tys' qs'
        q  <- rezero ty'' qf'
        q' <- rezero ty'' qf'
        let ineqs = filter (uncurry (/=)) (zip tys'' tys')
        when (not $ null ineqs) $
            throwError $ TypeError $ ineqs
        let z = zeroIndex ty
            Just qf_0  = lookup z qf
            z' = zeroIndex ty''
            Just qf_0' = lookup z' qf'
            Just q_0   = lookup z' q
            Just q_0'  = lookup z' q'
            zs = map zeroIndex tys'
            qxs_0  = zipWith ((fromJust .) . lookup) zs qxs
            qxs_0' = zipWith ((fromJust .) . lookup) zs qxs'
            qxf = map (concatMap (map pure . flip mapMaybe qf . xlate)) $ projectionsDeg degree ty
                where xlate ixs (ix, a) = (\ix' -> (ix', (a, poly ix / poly ix'))) <$> lookup ix ixs
        constrain $ concat $ zipWith equatePoly (zipWith delete zs qxs) qxf
        k1 <- costof k_ap1
        k2 <- costof k_ap2
        c  <- freshAnno
        let (qs_0_args, ([q_ap_0], _)) = zipR (q_0:qxs_0') qxs_0
        constrain [sparse [exchange $ Consume q_in, exchange $ Supply q_out] `Geq` k1 |
                   (q_in, q_out) <- qs_0_args]
        constrain [sparse (map exchange [Consume q_ap_0, Supply qf_0, Supply c]) `Eql` k1]
        constrain [sparse (map exchange [Supply q_0', Consume qf_0', Consume c]) `Eql` k2]
        return (ty'', (q, q'))
    elab (Let ds e) = do
        (tyds, (qs, qs')) <- second unzip <$> unzip <$> mapM (fmap assoc . traverse elab) ds
        tyxs <- traverse (\((x, ty), q) -> (,) x <$> (,) ty <$> rezero ty q) $ zip tyds qs
        (ty, (qe, qe')) <- withReaderT (\ce -> ce {env = reverse tyxs ++ env ce}) $ elab e
        q  <- rezero ty qe
        q' <- rezero ty qe'
        k1 <- costof k_lt1
        k2 <- costof k_lt2
        let z  = zeroIndex ty
            zs = map zeroIndex $ map snd tyds
            Just q_0  = lookup z q
            Just q_0' = lookup z q'
            qs_0  = zipWith ((fromJust .) . lookup) zs qs
            qs_0' = zipWith ((fromJust .) . lookup) zs qs'
            Just qe_0  = lookup z qe
            Just qe_0' = lookup z qe'
        constrain [sparse [exchange $ Consume q_in, exchange $ Supply q_out] `Geq` k1 |
                   (q_in, q_out) <- zip (q_0:qs_0') (qs_0 ++ [qe_0])]
        constrain [sparse (map exchange [Supply q_0', Consume qe_0']) `Geq` k2]
        return (ty, (q, q'))

instance Elab Val where
    elab (Nat _) = do
        let ty = NatTy
        (q, q') <- freshBounds ty
        return (ty, (q, q'))
    elab (Boolean _) = do
        let ty = BooleanTy
        (q, q') <- freshBounds ty
        return (ty, (q, q'))
    elab Unit = do
        let ty = UnitTy
        (q, q') <- freshBounds ty
        return (ty, (q, q'))
    elab (Sym _) = do
        let ty = SymTy
        (q, q') <- freshBounds ty
        return (ty, (q, q'))
    elab (List l) = elab l

instance Elab List where
    elab Nil = do
        let ty = ListTy $ Tyvar "a"
        (q, q') <- freshBounds ty
        return (ty, (q, q'))
    elab (Cons v vs) = do
        (vty, _) <- elab v
        (lty, _) <- elab vs
        ty <- case lty of
            ListTy lty' ->
                let lty'' = instantiate [vty] lty'
                    vty'' = instantiate [lty'] vty
                in if lty'' == vty''
                   then return $ ListTy lty''
                   else throwError $ TypeError [(vty'', lty'')]
            t -> throwError $ TypeError [(ListTy vty, t)]
        (q, q') <- freshBounds ty
        return (ty, (q, q'))
