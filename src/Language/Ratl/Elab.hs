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
    Annotatory(..),
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
    eqTy,
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

newtype ETy a = ETy { runety :: ((IxEnv a, IxEnv a), Ty a) }
    deriving (Eq, Ord)

instance Functor ETy where
    fmap f (ETy ((q, q'), t)) = ETy . (,) (fmap (fmap f) q, fmap (fmap f) q') $ fmap f t

instance Foldable ETy where
    foldMap f (ETy ((q, q'), t)) = foldMap (foldMap f) q `mappend` foldMap (foldMap f) q' `mappend` foldMap f t

instance Traversable ETy where
    traverse f (ETy ((q, q'), t)) = (ETy .) . (,) <$> ((,) <$> traverse (traverse f) q <*> traverse (traverse f) q') <*> traverse f t

instance Annotatory ETy where
    annotate dm (ETy ((q, q'), t)) = (ETy .) . (,) <$> ((,) <$> traverse reannotate q <*> traverse reannotate q') <*> annotate dm t

atoe :: MonadState Anno m => ATy Anno -> m (ETy Anno)
atoe (ATy (q, ty)) = do
    q' <- rezero ty q
    return $ ETy ((q, q'), ty)

newtype ATy a = ATy { runaty :: (IxEnv a, Ty a) }
    deriving (Eq, Ord)

instance Functor ATy where
    fmap f (ATy (q, t)) = ATy . (,) (fmap (fmap f) q) $ fmap f t

instance Foldable ATy where
    foldMap f (ATy (q, t)) = foldMap (foldMap f) q `mappend` foldMap f t

instance Traversable ATy where
    traverse f (ATy (q, t)) = (ATy .) . (,) <$> traverse (traverse f) q <*> traverse f t

instance Annotatory ATy where
    annotate dm (ATy (q, t)) = (ATy .) . (,) <$> traverse reannotate q <*> annotate dm t

aty :: (MonadReader CheckE m, MonadState Anno m) => Ty Anno -> m (ATy Anno)
aty t = do
    k <- degreeof
    q  <- traverse (reannotate . flip (,) ()) $ indexDeg k t
    return $ ATy (q, t)

newtype AFun a = AFun { runafun :: ((IxEnv a, IxEnv a), Fun a) }

instance Functor AFun where
    fmap f (AFun ((q, q'), t)) = AFun . (,) (fmap (fmap f) q, fmap (fmap f) q') $ fmap f t

instance Foldable AFun where
    foldMap f (AFun ((q, q'), t)) = foldMap (foldMap f) q `mappend` foldMap (foldMap f) q' `mappend` foldMap f t

instance Traversable AFun where
    traverse f (AFun ((q, q'), t)) = (AFun .) . (,) <$> ((,) <$> traverse (traverse f) q <*> traverse (traverse f) q') <*> traverse f t

instance Annotatory AFun where
    annotate dm (AFun ((q, q'), t)) = (AFun .) . (,) <$> ((,) <$> reanno q <*> reanno q') <*> annotate dm t
        where reanno = traverse reannotate . filter ((<= dm) . deg . fst)

instance Instantiable AFun where
    instantiate tys (AFun (qs, t)) = AFun . (,) qs $ instantiate tys t

afun :: MonadState Anno m => Int -> Fun a -> m (AFun Anno)
afun k fun = do
    let Arrow _ ts t' = tyOf fun
    q  <- traverse (reannotate . flip (,) ()) $ indexDeg k $ pairify ts
    q' <- traverse (reannotate . flip (,) ()) $ indexDeg k t'
    fun' <- annotate k fun
    return $ AFun ((q, q'), fun')
atyOf = tyOf . snd . runafun

newtype CIxEnv a = CIxEnv {runcix :: IxEnv a}
type IxEnv a = [(Index, a)]
type TyEnv a = [(Var, ATy a)]
type FunEnv a = [(Var, AFun a)]
type SharedTys a = MonoidalMap (ATy a) [ATy a]
type TyvarEnv a = [(String, Ty a)]
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

freein :: Ty a -> [String]
freein         NatTy = []
freein (ListTy _ ty) = freein ty
freein     BooleanTy = []
freein        UnitTy = []
freein         SymTy = []
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
        subst      BooleanTy = BooleanTy
        subst         UnitTy = UnitTy
        subst          SymTy = SymTy
        subst      (Tyvar x) = varsubst theta x

class Instantiable f where
    instantiate :: [Ty a] -> f a -> f a

newtype TyList a = TyList { unTyList :: [Ty a] }

instance Instantiable TyList where
    instantiate tys (TyList tys') = TyList $ map (tysubst varenv) tys''
        where varenv = foldl' solve [] $ zip tys'' tys
              tys'' = map (tysubst env) tys'
                where env = zip (map varname $ intersect frees bound) (map (Tyvar . varname) [next_var..])
                      next_var = 1 + (maximum $ union frees bound)
                      frees = nub $ map varnum $ concatMap freein tys
                      bound = nub $ map varnum $ concatMap freein tys'

instance Instantiable Ty where
    instantiate tys ty = head $ unTyList $ instantiate tys $ TyList [ty]

instance Instantiable FunTy where
    instantiate tys (Arrow q tys' ty') = Arrow q (init tys''ty'') (last tys''ty'')
        where tys''ty'' = unTyList $ instantiate tys (TyList $ tys' ++ [ty'])

instance Instantiable Fun where
    instantiate tys (Fun ty x e) = Fun (instantiate tys ty) x e
    instantiate tys (Native ty a f) = Native (instantiate tys ty) a f

pairify [ty] = ty
pairify (ty:tys) = PairTy (ty, pairify tys)

situate :: [Ty a] -> [IxEnv a] -> IxEnv a
situate tys ixss = foldl (unionBy ((==) `on` fst)) [] $ map (uncurry place) $ zip ptys ixss
    where place pty ixs = map (first (fromJust . extend (head ptys) . fromJust . expand pty)) ixs
          ptys = map pairify $ tails tys

rezero :: MonadState Anno m => Ty a -> IxEnv Anno -> m (IxEnv Anno)
rezero ty qs = do
    q_0' <- freshAnno
    return $ update (zeroIndex ty) q_0' qs

injectAnno :: (MonadReader CheckE m, MonadError TypeError m, MonadState Anno m) => Ty Anno -> IxEnv Anno -> m (IxEnv Anno)
injectAnno t qs = do
    qs' <- traverse (\(ix, q) -> flip (,) q <$> inject t ix `unlessJust` throwError (ProjectionError t ix)) qs
    k <- degreeof
    traverse (\ix -> (,) ix <$> lookup ix qs' `unlessJust` freshAnno) $ indexDeg k t

objective :: FunTy Anno -> Int -> LinearFunction
objective fty degree = sparse $ objF fty
    where payIf d = if degree == d then 1.0 else 0.0
          objF (Arrow (q, _) tys _) = (q, payIf 0):concatMap objTy tys
          objTy (ListTy ps _) = map (second payIf) $ zip ps [1..]
          objTy            _  = []

ashift :: [a] -> [[a]]
ashift ps = [p_1] ++ transpose [ps, p_ik]
    where (p_1, p_ik) = splitAt 1 ps

class Comparable a where
    relate :: (LinearFunction -> Double -> GeneralConstraint) -> a Anno -> [a Anno] -> [GeneralConstraint]
    equate :: a Anno -> [a Anno] -> [GeneralConstraint]
    exceed :: a Anno -> [a Anno] -> [GeneralConstraint]

    equate = relate Eql
    exceed = relate Geq

instance Comparable CIxEnv where
    relate c i is = [sparse (map exchange (Consume p:map Supply ps)) `c` 0.0 | (ix, p) <- runcix i, ps <- [mapMaybe (lookup ix . runcix) is], not $ null ps, not $ elem p ps]

instance Comparable Ty where
    relate c t ts = [sparse (map exchange (Consume p:map Supply qs)) `c` 0.0 | (p, qs) <- zip (qsOf t) $ transpose $ map qsOf ts, not $ elem p qs]
        where qsOf (ListTy qs _) = qs
              qsOf            _  = []

instance Comparable FunTy where
    relate c a as = [sparse (map exchange (Consume p:map Supply qs)) `c` 0.0 |
                        (p, qs) <- [(qOf a, map qOf as)], not $ elem p qs] ++
                    (concatMap (uncurry (relate c)) $
                              zip (psOf a) (transpose $ map psOf as)) ++
                    [sparse (map exchange (Consume p:map Supply qs)) `c` 0.0 |
                        (p, qs) <- [(q'Of a, map q'Of as)], not $ elem p qs] ++
                    (relate c (rOf a) (map rOf as))
        where qOf  (Arrow (q,  _)   _   _) = q
              q'Of (Arrow (_, q')   _   _) = q'
              psOf (Arrow       _ tys   _) = tys
              rOf  (Arrow       _   _ ty') = ty'

data CheckE = CheckE {
        env :: TyEnv Anno,
        checkF :: CheckF
    }

data CheckF = CheckF {
        degree :: Int,
        scps :: [Prog ()],
        comp :: FunEnv Anno,
        cost :: Cost
    }

lookupSCP :: MonadReader CheckE m => Var -> m (Maybe (Prog ()))
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

annoMax :: (Annotatory a, MonadState Anno m, MonadReader CheckE m) => a b -> m (a Anno)
annoMax a = degreeof >>= flip annotate a

lookupThisSCP :: MonadReader CheckE m => Var -> m (Maybe (AFun Anno))
lookupThisSCP x = asks (lookup x . comp . checkF)

assoc :: (a, (b, c)) -> ((a, b), c)
assoc (a, (b, c)) = ((a, b), c)

data TypeError = TypeError [(Ty Anno, Ty Anno)]
               | ArityError Int Int
               | NameError Var
               | ProjectionError (Ty Anno) Index

instance Show TypeError where
    show (TypeError ts) = "Cannot unify unlike types: " ++ intercalate ", " (map (\(t, t') -> show t ++ " and " ++ show t') ts)
    show (ArityError f a) = "Expected " ++ show f ++ " arguments, but got " ++ show a ++ "."
    show (NameError x) = "Name " ++ show x ++ " is not defined."
    show (ProjectionError t i) = "Index " ++ show i ++ " does not appear to be a projection of type " ++ show t ++ "."

zipR :: [a] -> [b] -> ([(a,b)], ([a], [b]))
zipR     []     bs = ([], ([], bs))
zipR     as     [] = ([], (as, []))
zipR (a:as) (b:bs) = first ((a,b) :) $ zipR as bs

check :: (MonadError TypeError m) => Int -> [Prog ()] -> m ProgEnv
check deg_max p = flip evalStateT 0 $ fmap concat $ forM p $ \scp -> do
    scp' <- travFun (traverse $ afun deg_max) scp
    (los, cs) <- runWriterT $ runReaderT (elabSCP scp') $ CheckF {degree = deg_max, scps = p, comp = scp', cost = constant}
    return $ map (second $ \os -> [GeneralForm Minimize o cs | o <- os]) los

checkEx :: (MonadError TypeError m) => Int -> [Prog ()] -> Ex -> m GeneralForm
checkEx deg_max p e = flip evalStateT 0 $ do
    ((ty, (q, q')), css) <- runWriterT $ runReaderT (elab e) $ CheckE [] $ CheckF {degree = deg_max, scps = p, comp = mempty, cost = constant}
    return $ GeneralForm Minimize (sparse [(q, 1.0)]) $ constrainShares css

elabSCP :: (MonadError TypeError m, MonadState Anno m) => FunEnv Anno -> ReaderT CheckF (WriterT [GeneralConstraint] m) [(Var, [LinearFunction])]
elabSCP = traverse (traverse elabF)
    where elabF :: (MonadError TypeError m, MonadState Anno m) => AFun Anno -> ReaderT CheckF (WriterT [GeneralConstraint] m) [LinearFunction]
          elabF f@(AFun ((pqs, _), fun)) = do
                    mapReaderT (mapWriterT (fmap $ second $ constrainShares)) $ elabFE f
                    deg_max <- asks degree
                    let Arrow _ ptys _ = tyOf fun
                    return $ reverse $ take (deg_max + 1) $ map (sparse . map (flip (,) 1.0 . fromJust . flip lookup pqs)) $ index $ pairify ptys
          elabFE :: (MonadError TypeError m, MonadState Anno m) => AFun Anno -> ReaderT CheckF (WriterT ([GeneralConstraint], SharedTys Anno) m) ()
          elabFE (AFun ((pqs, rqs), Fun (Arrow (qf, qf') [pty] rty) x e)) = do
                    xqs <- rezero pty pqs
                    (ETy ((qs, qs'), ty), (q, q')) <- withReaderT (CheckE $ zip [x] [ATy (xqs, pty)]) $ elab e
                    let ty'' = tysubst (solve [] (ty, rty)) ty
                    when (not $ eqTy rty ty'') $
                        throwError $ TypeError $ [(rty, ty'')]
                    qsx' <- withReaderT (CheckE []) $ injectAnno ty'' qs'
                    let z  = zeroIndex ty
                        Just qs_0  = lookup  z qs
                        pz = zeroIndex pty
                        Just pqs_0 = lookup pz pqs
                    constrain [sparse (map exchange [Consume pqs_0, Supply qs_0]) `Eql` 0.0]
                    constrain $ equate (CIxEnv rqs) [CIxEnv qsx']
          elabFE (AFun ((pqs, rqs), Native (Arrow (qf, qf') [pty@(ListTy ps pt)] (ListTy rs rt)) _ _)) | pt == rt = do -- hack for cdr
                    let ss = map (\(i, (i1, i2)) -> (,) <$> lookup i rqs <*> sequence (filter isJust [lookup i1 pqs, lookup i2 pqs])) $ shift pty
                    constrain [sparse (map exchange (Supply q:map Consume ps)) `Eql` 0.0 |
                               Just (q, ps) <- takeWhile isJust ss, not $ elem q ps]
          elabFE (AFun ((pqs, rqs), Native (Arrow (qf, qf') ptys@[tyh, ListTy rs tyt] rty@(ListTy ps tyc)) _ _)) = do -- hack for cons
                    let ss = map (\(i, (i1, i2)) -> (,) <$> lookup (fromJust $ extend (pairify ptys) i) pqs <*> sequence (filter isJust [lookup i1 rqs, lookup i2 rqs])) $ shift rty
                    constrain [sparse (map exchange (Supply q:map Consume ps)) `Eql` 0.0 |
                               Just (q, ps) <- takeWhile isJust ss, not $ elem q ps]
          elabFE (AFun ((pqs, rqs), Native (Arrow (qf, qf') tys ty') _ _)) = do
                    let z = zeroIndex $ pairify tys
                        Just qs_0  = lookup z  pqs
                        z' = zeroIndex ty'
                        Just qs_0' = lookup z' rqs
                    constrain [sparse [exchange $ Consume qs_0, exchange $ Supply qs_0'] `Geq` 0.0]

class Elab a where
    elab :: (MonadError TypeError m, MonadState Anno m) => a -> ReaderT CheckE (WriterT ([GeneralConstraint], SharedTys Anno) m) (ETy Anno, (Anno, Anno))

instance Elab Ex where
    elab (Var x) = do
        ty <- unlessJustM (gamma x) $
                throwError $ NameError x
        q  <- freshAnno
        ty' <- reannotate ty
        q' <- freshAnno
        share $ singleton ty [ty']
        ety'@(ETy ((qi, qi'), tyi)) <- atoe ty'
        let z = zeroIndex tyi
            Just qi_0  = lookup z qi
            Just qi_0' = lookup z qi'
        k <- costof k_var
        constrain [sparse [exchange $ Consume qi_0, exchange $ Supply qi_0'] `Geq` k]
        return (ety', (q, q'))
    elab (Val v) = do
        (ety'@(ETy ((qi, qi'), ty)), (q, q')) <- elab v
        k <- costof k_val
        let z = zeroIndex ty
            Just qi_0  = lookup z qi
            Just qi_0' = lookup z qi'
        constrain [sparse [exchange $ Consume qi_0, exchange $ Supply qi_0'] `Geq` k]
        return (ety', (q, q'))
    elab (If ep et ef) = do
        (ETy ((qsp, qsp'), tyep), (qip, qip')) <- elab ep
        ((ETy ((qst, qst'), tyet), (qit, qit')), (tcs, tss)) <- mapReaderT runWriterT $ elab et
        ((ETy ((qsf, qsf'), tyef), (qif, qif')), (fcs, fss)) <- mapReaderT runWriterT $ elab ef
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
        let ifty = Arrow ((), ()) [BooleanTy, Tyvar "a", Tyvar "a"] (Tyvar "a")
        Arrow (q, q') tys' ty'' <- annoMax $ instantiate (map void tys) ifty
        let tys'' = unTyList $ instantiate tys' $ TyList tys
        let [typ, tyt, tyf] = tys'
        qxp  <- injectAnno typ qsp
        qxp' <- injectAnno typ qsp'
        qxt  <- injectAnno tyt qst
        qxt' <- injectAnno tyt qst'
        qxf  <- injectAnno tyf qsf
        qxf' <- injectAnno tyf qsf'
        ety'@(ETy ((qix, qix'), _)) <- atoe =<< aty ty''
        let ineqs = filter (not . uncurry eqTy) (zip tys'' tys')
        when (not $ null ineqs) $
            throwError $ TypeError $ ineqs
        [kp, kt, kf, kc] <- sequence [costof k_ifp, costof k_ift, costof k_iff, costof k_ifc]
        let z = zeroIndex ty''
            Just q_0  = lookup z qix
            Just q_0' = lookup z qix'
        let zp = zeroIndex typ
            Just qxp_0  = lookup zp qxp
            Just qxp_0' = lookup zp qxp'
        let zt = zeroIndex tyt
            Just qxt_0  = lookup zt qxt
            Just qxt_0' = lookup zt qxt'
        let zf = zeroIndex tyf
            Just qxf_0  = lookup zf qxf
            Just qxf_0' = lookup zf qxf'
        constrain $ exceed (CIxEnv $ delete zt qxt) [CIxEnv qix] ++ exceed (CIxEnv $ delete zf qxf) [CIxEnv qix]
        constrain [sparse [exchange $ Consume q_0,    exchange $ Supply qxp_0] `Geq` kp,
                   sparse [exchange $ Consume qxp_0', exchange $ Supply qxt_0] `Geq` kt,
                   sparse [exchange $ Consume qxp_0', exchange $ Supply qxf_0] `Geq` kf,
                   sparse [exchange $ Consume qxt_0', exchange $ Supply q_0']  `Geq` kc,
                   sparse [exchange $ Consume qxf_0', exchange $ Supply q_0']  `Geq` kc]
        return (ety', (q, q'))
    elab (App f es) = do
        (etys, (qs, q's)) <- second unzip <$> unzip <$> mapM elab es
        let ((qis, qis'), tys) = first unzip $ unzip $ map runety etys
        when (arity f /= length es) $
            throwError $ ArityError (arity f) (length es)
        degree <- degreeof
        ((qif, qif'), Arrow (qf, qf') tys' ty'') <- lookupThisSCP f >>= \case
            Just asc -> do
                cost_free <- costof (== zero)
                if degree <= 1 || cost_free then
                    return $ second (instantiate tys . tyOf) $ runafun asc
                else do
                    scp <- asks (comp . checkF)
                    fun <- annoMax $ instantiate tys asc
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
                let fun = instantiate (map void tys) $ fromJust $ lookupFun scp f
                scp' <- travFun (traverse $ afun degree) $ updateFun scp f fun
                constrain =<< withReaderT (\ce -> (checkF ce) {comp = scp'}) (mapReaderT execWriterT (elabSCP scp'))
                return $ second tyOf $ runafun $ fromJust $ lookup f scp'
        q  <- freshAnno
        q' <- freshAnno
        let tys'' = unTyList $ instantiate tys' $ TyList tys
        qxs  <- traverse (uncurry injectAnno) $ zip tys' qis
        qxs' <- traverse (uncurry injectAnno) $ zip tys' qis'
        qix  <- rezero ty'' qif'
        qix' <- rezero ty'' qif'
        let ety' = ETy ((qix, qix'), ty'')
        let ineqs = filter (not . uncurry eqTy) (zip tys'' tys')
        when (not $ null ineqs) $
            throwError $ TypeError $ ineqs
        let z = zeroIndex $ pairify tys'
            Just qf_0  = lookup z qif
            z' = zeroIndex ty''
            Just qf_0' = lookup z' qif'
            Just q_0   = lookup z' qix
            Just q_0'  = lookup z' qix'
            zs = map zeroIndex tys'
            qxs_0  = map (fromJust . uncurry lookup) $ zip zs qxs
            qxs'_0 = map (fromJust . uncurry lookup) $ zip zs qxs'
            qxf = situate tys' qxs
        constrain $ equate (CIxEnv $ delete z qxf) [CIxEnv qif]
        k1 <- costof k_ap1
        k2 <- costof k_ap2
        c  <- freshAnno
        cx <- freshAnno
        let (qs_args, ([q_ap], _)) = zipR (q:q's) qs
        let (qs_0_args, ([q_ap_0], _)) = zipR (q_0:qxs'_0) qxs_0
        constrain [sparse [exchange $ Consume q_in, exchange $ Supply q_out] `Geq` k1 |
                   (q_in, q_out) <- qs_0_args]
        constrain [sparse (map exchange [Consume q_ap_0, Supply qf_0, Supply cx]) `Eql` k1]
        constrain [sparse (map exchange [Supply q_0', Consume qf_0', Consume cx]) `Eql` k2]
        return (ety', (q, q'))
    elab (Let ds e) = do
        (tyds, (qs, q's)) <- second unzip <$> unzip <$> mapM (fmap assoc . traverse elab) ds
        tyxs <- traverse (traverse $ \(ETy ((q, q'), ty)) -> rezero ty q >>= \qx -> return $ ATy (qx, ty)) tyds
        (ETy ((qex, qex'), ty), (qe, qe')) <- withReaderT (\ce -> ce {env = reverse tyxs ++ env ce}) $ elab e
        q  <- freshAnno
        q' <- freshAnno
        qix  <- rezero ty qex
        qix' <- rezero ty qex'
        let Just q_0  = lookup (zeroIndex ty) qix
            Just q_0' = lookup (zeroIndex ty) qix'
        k1 <- costof k_lt1
        k2 <- costof k_lt2
        let ((qds, qds'), tys) = first unzip $ unzip $ map (runety . snd) tyds
            zs = map zeroIndex tys
            qds_0  = map (fromJust . uncurry lookup) $ zip zs qds
            qds'_0 = map (fromJust . uncurry lookup) $ zip zs qds'
            z = zeroIndex ty
            Just qe_0  = lookup z qex
            Just qe_0' = lookup z qex'
        constrain [sparse [exchange $ Consume q_in, exchange $ Supply q_out] `Geq` k1 |
                   (q_in, q_out) <- zip (q_0:qds'_0) (qds_0 ++ [qe_0])]
        constrain [sparse (map exchange [Supply q_0', Consume qe_0']) `Geq` k2]
        return (ETy ((qix, qix'), ty), (q, q'))

instance Elab Val where
    elab (Nat _) = do
        q <- freshAnno
        q' <- freshAnno
        ety' <- atoe =<< aty NatTy
        return (ety', (q, q'))
    elab (Boolean _) = do
        q <- freshAnno
        q' <- freshAnno
        ety' <- atoe =<< aty BooleanTy
        return (ety', (q, q'))
    elab Unit = do
        q <- freshAnno
        q' <- freshAnno
        ety' <- atoe =<< aty UnitTy
        return (ety', (q, q'))
    elab (Sym _) = do
        q <- freshAnno
        q' <- freshAnno
        ety' <- atoe =<< aty SymTy
        return (ety', (q, q'))
    elab (List l) = elab l

instance Elab List where
    elab Nil = do
        q <- freshAnno
        ty <- annoMax $ ListTy [] $ Tyvar "a"
        q' <- freshAnno
        ety' <- atoe =<< aty ty
        return (ety', (q, q'))
    elab (Cons v vs) = do
        (ETy (_, vty), _) <- elab v
        (ETy (_, lty), (q, q')) <- elab vs
        ty <- case lty of
            ListTy ps lty' ->
                let lty'' = instantiate [vty] lty'
                    vty'' = instantiate [lty'] vty
                in if eqTy lty'' vty''
                   then return $ ListTy ps lty''
                   else throwError $ TypeError [(vty'', lty'')]
            t -> throwError $ TypeError [(ListTy [] vty, t)]
        ety' <- atoe =<< aty ty
        return (ety', (q, q'))
