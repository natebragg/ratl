{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ratl.Elab (
    check,
    checkEx,
) where

import Data.List (intersect, intercalate, union, nub, foldl', transpose, tails, unionBy)
import Data.Function (on)
import Data.Map (foldMapWithKey, traverseWithKey, elems, fromList, toList, mapKeys)
import Data.Map.Monoidal (MonoidalMap(..), singleton, keys)
import Data.Maybe (listToMaybe, isJust, fromJust, mapMaybe)
import Control.Applicative (empty)
import Control.Arrow (first, second, (&&&))
import Control.Monad (when, forM, void)
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
    )
import Language.Ratl.Anno (
    Anno,
    reannotate,
    freshAnno,
    )
import Language.Ratl.Index (
    Index,
    deg,
    index,
    indexDeg,
    zeroIndex,
    shift,
    inject,
    extend,
    expand,
    )
import Language.Ratl.Ty (
    Ty(..),
    varname,
    varnum,
    FunTy(..),
    )
import Language.Ratl.Ast (
    List(..),
    Var(..),
    Val(..),
    Fun(..),
    Ex(..),
    Prog,
    tyOf,
    lookupFun,
    updateFun,
    travFun,
    )
import Language.Ratl.Basis (arity)

atoe :: MonadState Anno m => ATy Anno -> m (Ty, (IxEnv Anno, IxEnv Anno))
atoe (ATy (q, ty)) = do
    q' <- rezero ty q
    return (ty, (q, q'))

newtype ATy a = ATy { runaty :: (IxEnv a, Ty) }
    deriving (Eq, Ord)

instance Functor ATy where
    fmap f (ATy (q, t)) = ATy . (,) (fmap (fmap f) q) $ t

instance Foldable ATy where
    foldMap f (ATy (q, t)) = foldMap (foldMap f) q

instance Traversable ATy where
    traverse f (ATy (q, t)) = (ATy .) . (,) <$> traverse (traverse f) q <*> pure t

newtype AFun a = AFun { runafun :: ((IxEnv a, IxEnv a), Fun) }

instance Functor AFun where
    fmap f (AFun ((q, q'), t)) = AFun . (,) (fmap (fmap f) q, fmap (fmap f) q') $ t

instance Foldable AFun where
    foldMap f (AFun ((q, q'), t)) = foldMap (foldMap f) q `mappend` foldMap (foldMap f) q'

instance Traversable AFun where
    traverse f (AFun ((q, q'), t)) = (AFun .) . (,) <$> ((,) <$> traverse (traverse f) q <*> traverse (traverse f) q') <*> pure t

annotate :: MonadState Anno m => Int -> AFun a -> m (AFun Anno)
annotate dm (AFun ((q, q'), t)) = (AFun .) . (,) <$> ((,) <$> reanno q <*> reanno q') <*> pure t
    where reanno = traverse reannotate . filter ((<= dm) . deg . fst)

instance Instantiable (AFun a) where
    instantiate tys (AFun (qs, t)) = AFun . (,) qs $ instantiate tys t

afun :: MonadState Anno m => Int -> Fun -> m (AFun Anno)
afun k fun = do
    let Arrow ts t' = tyOf fun
    q  <- traverse (reannotate . flip (,) ()) $ indexDeg k $ pairify ts
    q' <- traverse (reannotate . flip (,) ()) $ indexDeg k t'
    return $ AFun ((q, q'), fun)

newtype CIxEnv a = CIxEnv {runcix :: IxEnv a}
type IxEnv a = [(Index, a)]
type TyEnv a = [(Var, ATy a)]
type FunEnv a = [(Var, AFun a)]
type SharedTys a = MonoidalMap (ATy a) [ATy a]
type TyvarEnv = [(String, Ty)]
type ProgEnv = [(Var, [GeneralForm])]

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
freein       NatTy = []
freein (ListTy ty) = freein ty
freein   BooleanTy = []
freein      UnitTy = []
freein       SymTy = []
freein   (Tyvar y) = [y]

solve :: TyvarEnv -> (Ty, Ty) -> TyvarEnv
solve theta (ty, ty') = compose (assoc (tysubst theta ty) (tysubst theta ty')) theta
  where assoc (ListTy ty) (ListTy ty') = assoc ty ty'
        assoc   (Tyvar x)          ty' = [(x, ty')]
        assoc           _            _ = []

compose :: TyvarEnv -> TyvarEnv -> TyvarEnv
compose theta2 theta1 = map (id &&& replace) domain
  where domain  = union (map fst theta2) (map fst theta1)
        replace = tysubst theta2 . varsubst theta1

varsubst :: TyvarEnv -> String -> Ty
varsubst theta x = maybe (Tyvar x) id $ lookup x theta

tysubst :: TyvarEnv -> Ty -> Ty
tysubst theta = subst
  where subst       NatTy = NatTy
        subst (ListTy ty) = ListTy $ subst ty
        subst   BooleanTy = BooleanTy
        subst      UnitTy = UnitTy
        subst       SymTy = SymTy
        subst   (Tyvar x) = varsubst theta x

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
    instantiate tys (Arrow tys' ty') = Arrow (init tys''ty'') (last tys''ty'')
        where tys''ty'' = instantiate tys (tys' ++ [ty'])

instance Instantiable Fun where
    instantiate tys (Fun ty x e) = Fun (instantiate tys ty) x e
    instantiate tys (Native ty a f) = Native (instantiate tys ty) a f

pairify [ty] = ty
pairify (ty:tys) = PairTy (ty, pairify tys)

situate :: [Ty] -> [IxEnv a] -> IxEnv a
situate tys ixss = foldl (unionBy ((==) `on` fst)) [] $ map (uncurry place) $ zip ptys ixss
    where place pty ixs = map (first (fromJust . extend (head ptys) . fromJust . expand pty)) ixs
          ptys = map pairify $ tails tys

freshBounds :: (MonadReader CheckE m, MonadState Anno m) => Ty -> m (IxEnv Anno, IxEnv Anno)
freshBounds t = do
    k <- degreeof
    q  <- traverse (reannotate . flip (,) ()) $ indexDeg k t
    q' <- rezero t q
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

class Comparable a where
    relate :: (LinearFunction -> Double -> GeneralConstraint) -> a Anno -> [a Anno] -> [GeneralConstraint]
    equate :: a Anno -> [a Anno] -> [GeneralConstraint]
    exceed :: a Anno -> [a Anno] -> [GeneralConstraint]

    equate = relate Eql
    exceed = relate Geq

instance Comparable CIxEnv where
    relate c i is = [sparse (map exchange (Consume p:map Supply ps)) `c` 0.0 | (ix, p) <- runcix i, ps <- [mapMaybe (lookup ix . runcix) is], not $ null ps, not $ elem p ps]

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
constrainShares = uncurry (++) . second (foldMapWithKey ((. map (CIxEnv . fst . runaty)) . equate . CIxEnv . fst . runaty) . getMonoidalMap)

gamma :: MonadReader CheckE m => Var -> m (Maybe (ATy Anno))
gamma x = asks (lookup x . env)

costof :: MonadReader CheckE m => (Cost -> a) -> m a
costof k = asks (k . cost . checkF)

degreeof :: MonadReader CheckE m => m Int
degreeof = asks (degree . checkF)

lookupThisSCP :: MonadReader CheckE m => Var -> m (Maybe (AFun Anno))
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

check :: (MonadError TypeError m) => Int -> [Prog] -> m ProgEnv
check deg_max p = flip evalStateT 0 $ fmap concat $ forM p $ \scp -> do
    scp' <- travFun (traverse $ afun deg_max) scp
    (los, cs) <- runWriterT $ runReaderT (elabSCP scp') $ CheckF {degree = deg_max, scps = p, comp = scp', cost = constant}
    return $ map (second $ \os -> [GeneralForm Minimize o cs | o <- os]) los

checkEx :: (MonadError TypeError m) => Int -> [Prog] -> Ex -> m GeneralForm
checkEx deg_max p e = flip evalStateT 0 $ do
    ((ty, (q, q')), css) <- runWriterT $ runReaderT (elab e) $ CheckE [] $ CheckF {degree = deg_max, scps = p, comp = mempty, cost = constant}
    let z = zeroIndex ty
        Just q_0 = lookup z q
    return $ GeneralForm Minimize (sparse [(q_0, 1.0)]) $ constrainShares css

elabSCP :: (MonadError TypeError m, MonadState Anno m) => FunEnv Anno -> ReaderT CheckF (WriterT [GeneralConstraint] m) [(Var, [LinearFunction])]
elabSCP = traverse (traverse elabF)
    where elabF :: (MonadError TypeError m, MonadState Anno m) => AFun Anno -> ReaderT CheckF (WriterT [GeneralConstraint] m) [LinearFunction]
          elabF f@(AFun ((pqs, _), fun)) = do
                    mapReaderT (mapWriterT (fmap $ second $ constrainShares)) $ elabFE f
                    deg_max <- asks degree
                    let Arrow ptys _ = tyOf fun
                    return $ reverse $ take (deg_max + 1) $ map (sparse . map (flip (,) 1.0 . fromJust . flip lookup pqs)) $ index $ pairify ptys
          elabFE :: (MonadError TypeError m, MonadState Anno m) => AFun Anno -> ReaderT CheckF (WriterT ([GeneralConstraint], SharedTys Anno) m) ()
          elabFE (AFun ((pqs, rqs), Fun (Arrow [pty] rty) x e)) = do
                    xqs <- rezero pty pqs
                    (ty, (q, q')) <- withReaderT (CheckE $ zip [x] [ATy (xqs, pty)]) $ elab e
                    let ty'' = tysubst (solve [] (ty, rty)) ty
                    when (rty /= ty'') $
                        throwError $ TypeError $ [(rty, ty'')]
                    qsx' <- withReaderT (CheckE []) $ injectAnno ty'' q'
                    let z  = zeroIndex ty
                        Just q_0   = lookup  z q
                        pz = zeroIndex pty
                        Just pqs_0 = lookup pz pqs
                    constrain [sparse (map exchange [Consume pqs_0, Supply q_0]) `Eql` 0.0]
                    constrain $ equate (CIxEnv rqs) [CIxEnv qsx']
          elabFE (AFun ((pqs, rqs), Native (Arrow [pty@(ListTy pt)] (ListTy rt)) _ _)) | pt == rt = do -- hack for cdr
                    let ss = map (\(i, (i1, i2)) -> (,) <$> lookup i rqs <*> sequence (filter isJust [lookup i1 pqs, lookup i2 pqs])) $ shift pty
                    constrain [sparse (map exchange (Supply q:map Consume ps)) `Eql` 0.0 |
                               Just (q, ps) <- takeWhile isJust ss, not $ elem q ps]
          elabFE (AFun ((pqs, rqs), Native (Arrow ptys@[tyh, ListTy tyt] rty@(ListTy tyc)) _ _)) = do -- hack for cons
                    let ss = map (\(i, (i1, i2)) -> (,) <$> lookup (fromJust $ extend (pairify ptys) i) pqs <*> sequence (filter isJust [lookup i1 rqs, lookup i2 rqs])) $ shift rty
                    constrain [sparse (map exchange (Supply q:map Consume ps)) `Eql` 0.0 |
                               Just (q, ps) <- takeWhile isJust ss, not $ elem q ps]
          elabFE (AFun ((pqs, rqs), Native (Arrow tys ty') _ _)) = do
                    let z = zeroIndex $ pairify tys
                        Just qs_0  = lookup z  pqs
                        z' = zeroIndex ty'
                        Just qs_0' = lookup z' rqs
                    constrain [sparse [exchange $ Consume qs_0, exchange $ Supply qs_0'] `Geq` 0.0]

class Elab a where
    elab :: (MonadError TypeError m, MonadState Anno m) => a -> ReaderT CheckE (WriterT ([GeneralConstraint], SharedTys Anno) m) (Ty, (IxEnv Anno, IxEnv Anno))

instance Elab Ex where
    elab (Var x) = do
        ty <- unlessJustM (gamma x) $
                throwError $ NameError x
        ty' <- reannotate ty
        share $ singleton ty [ty']
        (tyi, (q, q')) <- atoe ty'
        let z = zeroIndex tyi
            Just q_0  = lookup z q
            Just q_0' = lookup z q'
        k <- costof k_var
        constrain [sparse [exchange $ Consume q_0, exchange $ Supply q_0'] `Geq` k]
        return (tyi, (q, q'))
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
        sharemap <- traverse (fmap <$> (,) <*> reannotate) $ union (keys tss) (keys fss)
        share $ MonoidalMap $ fromList $ map (second pure) sharemap
        let reannotateShares ss = do
                ss' <- traverseWithKey (flip (fmap . flip (,)) . reannotate) $
                        mapKeys (fromJust . flip lookup sharemap) $ getMonoidalMap ss
                constrain $ concatMap (uncurry exceed . first (CIxEnv . fst . runaty) . second (pure . CIxEnv . fst . runaty . fst)) $ toList ss'
                return $ MonoidalMap $ fromList $ elems ss'
        share =<< reannotateShares tss
        share =<< reannotateShares fss
        let ifty = Arrow [BooleanTy, Tyvar "a", Tyvar "a"] (Tyvar "a")
        let Arrow tys' ty'' = instantiate tys ifty
        let tys'' = instantiate tys' tys
        let [typ, tyt, tyf] = tys'
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
        constrain $ exceed (CIxEnv $ delete zt qit) [CIxEnv q] ++ exceed (CIxEnv $ delete zf qif) [CIxEnv q]
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
        ((qf, qf'), Arrow tys' ty'') <- lookupThisSCP f >>= \case
            Just asc -> do
                cost_free <- costof (== zero)
                if degree <= 1 || cost_free then
                    return $ second (instantiate tys . tyOf) $ runafun asc
                else do
                    scp <- asks (comp . checkF)
                    fun <- annotate degree $ instantiate tys asc
                    -- this is cheating for polymorphic mutual recursion; should instantiate tys over the scp somehow
                    cfscp <- traverse (traverse $ annotate (degree - 1)) $ update f fun scp
                    let Just cffun = lookup f cfscp
                    constrain $ equate (CIxEnv $ fst $ fst $ runafun fun) [CIxEnv $ fst $ fst $ runafun asc, CIxEnv $ fst $ fst $ runafun cffun]
                    constrain $ equate (CIxEnv $ snd $ fst $ runafun fun) [CIxEnv $ snd $ fst $ runafun asc, CIxEnv $ snd $ fst $ runafun cffun]
                    constrain =<< withReaderT (\ce -> (checkF ce) {degree = degree - 1, comp = cfscp, cost = zero}) (mapReaderT execWriterT (elabSCP cfscp))
                    return $ second tyOf $ runafun fun
            Nothing -> do
                scp <- unlessJustM (lookupSCP f) $
                       throwError $ NameError f
                let fun = instantiate tys $ fromJust $ lookupFun scp f
                scp' <- travFun (traverse $ afun degree) $ updateFun scp f fun
                constrain =<< withReaderT (\ce -> (checkF ce) {comp = scp'}) (mapReaderT execWriterT (elabSCP scp'))
                return $ second tyOf $ runafun $ fromJust $ lookup f scp'
        let tys'' = instantiate tys' tys
        qxs  <- traverse (uncurry injectAnno) $ zip tys' qs
        qxs' <- traverse (uncurry injectAnno) $ zip tys' qs'
        q  <- rezero ty'' qf'
        q' <- rezero ty'' qf'
        let ineqs = filter (uncurry (/=)) (zip tys'' tys')
        when (not $ null ineqs) $
            throwError $ TypeError $ ineqs
        let z = zeroIndex $ pairify tys'
            Just qf_0  = lookup z qf
            z' = zeroIndex ty''
            Just qf_0' = lookup z' qf'
            Just q_0   = lookup z' q
            Just q_0'  = lookup z' q'
            zs = map zeroIndex tys'
            qxs_0  = map (fromJust . uncurry lookup) $ zip zs qxs
            qxs_0' = map (fromJust . uncurry lookup) $ zip zs qxs'
            qxf = situate tys' qxs
        constrain $ equate (CIxEnv $ delete z qxf) [CIxEnv qf]
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
        tyxs <- traverse (\((x, ty), q) -> rezero ty q >>= \qx -> return $ (x, ATy (qx, ty))) $ zip tyds qs
        (ty, (qe, qe')) <- withReaderT (\ce -> ce {env = reverse tyxs ++ env ce}) $ elab e
        q  <- rezero ty qe
        q' <- rezero ty qe'
        k1 <- costof k_lt1
        k2 <- costof k_lt2
        let z  = zeroIndex ty
            zs = map zeroIndex $ map snd tyds
            Just q_0  = lookup z q
            Just q_0' = lookup z q'
            qs_0  = map (fromJust . uncurry lookup) $ zip zs qs
            qs_0' = map (fromJust . uncurry lookup) $ zip zs qs'
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
        let ty = (ListTy $ Tyvar "a")
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
