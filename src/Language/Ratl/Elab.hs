{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ratl.Elab (
    check,
    checkEx,
) where

import Data.List (intersect, intercalate, union, nub, foldl', transpose)
import Data.Map (foldMapWithKey, traverseWithKey, elems, fromList, toList, mapKeys)
import Data.Map.Monoidal (MonoidalMap(..), singleton, keys)
import Data.Maybe (listToMaybe, isJust, fromJust)
import Control.Applicative (empty)
import Control.Arrow (first, second, (&&&))
import Control.Monad (when, forM, void)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Except.Extra (unlessJustM)
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

type TyEnv a = [(Var, Ty a)]
type SharedTys a = MonoidalMap (Ty a) [Ty a]
type TyvarEnv a = [(String, Ty a)]
type ProgEnv = [(Var, [GeneralForm])]

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

objective :: FunTy Anno -> Int -> LinearFunction
objective fty degree = sparse $ objF fty
    where payIf d = if degree == d then 1.0 else 0.0
          objF (Arrow (q, _) tys _) = (q, payIf 0):concatMap objTy tys
          objTy (ListTy ps _) = map (second payIf) $ zip ps [1..]
          objTy            _  = []

shift :: [a] -> [[a]]
shift ps = [p_1] ++ transpose [ps, p_ik]
    where (p_1, p_ik) = splitAt 1 ps

class Comparable a where
    relate :: (LinearFunction -> Double -> GeneralConstraint) -> a Anno -> [a Anno] -> [GeneralConstraint]
    equate :: a Anno -> [a Anno] -> [GeneralConstraint]
    exceed :: a Anno -> [a Anno] -> [GeneralConstraint]

    equate = relate Eql
    exceed = relate Geq

instance Comparable Ty where
    relate c t ts = [sparse (map exchange (Consume p:map Supply qs)) `c` 0.0 | (p, qs) <- zip (qsOf t) $ transpose $ map qsOf ts, not $ elem p qs]
        where qsOf (ListTy qs _) = qs
              qsOf            _  = []

instance Comparable FunTy where
    relate c a as = (concatMap (uncurry (relate c)) $ zip (psOf a) (transpose $ map psOf as) ++ [(rOf a, map rOf as)]) ++
                    [sparse (map exchange (Consume p:map Supply qs)) `c` 0.0 | (p, qs) <- [(qOf a, map qOf as), (q'Of a, map q'Of as)], not $ elem p qs]
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
        comp :: Prog Anno,
        cost :: Cost
    }

lookupSCP :: MonadReader CheckE m => Var -> m (Maybe (Prog ()))
lookupSCP x = asks (listToMaybe . filter (isJust . flip lookupFun x) . scps . checkF)

share :: (Monoid a, MonadWriter (a, SharedTys b) m) => SharedTys b -> m ()
share m = tell (mempty, m)

constrain :: (Monoid b, MonadWriter ([GeneralConstraint], b) m) => [GeneralConstraint] -> m ()
constrain c = tell (c, mempty)

constrainShares :: ([GeneralConstraint], SharedTys Anno) -> [GeneralConstraint]
constrainShares = uncurry (++) . second (foldMapWithKey equate . getMonoidalMap)

gamma :: MonadReader CheckE m => Var -> m (Maybe (Ty Anno))
gamma x = asks (lookup x . env)

costof :: MonadReader CheckE m => (Cost -> a) -> m a
costof k = asks (k . cost . checkF)

degreeof :: MonadReader CheckE m => m Int
degreeof = asks (degree . checkF)

annoMax :: (Annotatory a, MonadState Anno m, MonadReader CheckE m) => a b -> m (a Anno)
annoMax a = degreeof >>= flip annotate a

lookupThisSCP :: MonadReader CheckE m => Var -> m (Maybe (Fun Anno))
lookupThisSCP x = asks (flip lookupFun x . comp . checkF)

assoc :: (a, (b, c)) -> ((a, b), c)
assoc (a, (b, c)) = ((a, b), c)

data TypeError = TypeError [(Ty Anno, Ty Anno)]
               | ArityError Int Int
               | NameError Var

instance Show TypeError where
    show (TypeError ts) = "Cannot unify unlike types: " ++ intercalate ", " (map (\(t, t') -> show t ++ " and " ++ show t') ts)
    show (ArityError f a) = "Expected " ++ show f ++ " arguments, but got " ++ show a ++ "."
    show (NameError x) = "Name " ++ show x ++ " is not defined."

zipR :: [a] -> [b] -> ([(a,b)], ([a], [b]))
zipR     []     bs = ([], ([], bs))
zipR     as     [] = ([], (as, []))
zipR (a:as) (b:bs) = first ((a,b) :) $ zipR as bs

check :: (MonadError TypeError m) => Int -> [Prog ()] -> m ProgEnv
check deg_max p = flip evalStateT 0 $ fmap concat $ forM p $ \scp -> do
    scp' <- annotate deg_max scp
    (los, cs) <- runWriterT $ runReaderT (elabSCP scp') $ CheckF {degree = deg_max, scps = p, comp = scp', cost = constant}
    return $ map (second $ \os -> [GeneralForm Minimize o cs | o <- os]) los

checkEx :: (MonadError TypeError m) => Int -> [Prog ()] -> Ex -> m GeneralForm
checkEx deg_max p e = flip evalStateT 0 $ do
    ((ty, (q, q')), css) <- runWriterT $ runReaderT (elab e) $ CheckE [] $ CheckF {degree = deg_max, scps = p, comp = mempty, cost = constant}
    return $ GeneralForm Minimize (sparse [(q, 1.0)]) (constrainShares css)

elabSCP :: (MonadError TypeError m, MonadState Anno m) => Prog Anno -> ReaderT CheckF (WriterT [GeneralConstraint] m) [(Var, [LinearFunction])]
elabSCP = travFun (traverse elabF)
    where elabF :: (MonadError TypeError m, MonadState Anno m) => Fun Anno -> ReaderT CheckF (WriterT [GeneralConstraint] m) [LinearFunction]
          elabF f = do
                    mapReaderT (mapWriterT (fmap $ second $ constrainShares)) $ elabFE f
                    deg_max <- asks degree
                    return $ map (objective (tyOf f)) [deg_max,deg_max-1..0]
          elabFE :: (MonadError TypeError m, MonadState Anno m) => Fun Anno -> ReaderT CheckF (WriterT ([GeneralConstraint], SharedTys Anno) m) ()
          elabFE (Fun (Arrow (qf, qf') tys ty') x e) = do
                    (ty, (q, q')) <- withReaderT (CheckE (zip [x] tys)) $ elab e
                    let ty'' = tysubst (solve [] (ty, ty')) ty
                    when (not $ eqTy ty' ty'') $
                        throwError $ TypeError $ [(ty', ty'')]
                    constrain $ equate ty' [ty'']
                    constrain [sparse (map exchange [Consume qf, Supply q]) `Eql` 0.0]
                    constrain [sparse (map exchange [Supply qf', Consume q']) `Eql` 0.0]
          elabFE (Native (Arrow (qf, qf') [ListTy ps _] (ListTy rs _)) _ _) = -- hack for cdr
                    constrain [sparse (map exchange (Supply r:map Consume sps)) `Eql` 0.0 |
                               (r, sps) <- zip (qf':rs) (tail $ shift (qf:ps)), not $ elem r sps]
          elabFE (Native (Arrow (qf, qf') [tyh, ListTy rs tyt] (ListTy ps tyc)) _ _) = do -- hack for cons
                    constrain [sparse (map exchange (Supply r:map Consume sps)) `Eql` 0.0 |
                               (r, sps) <- zip (qf:rs) (tail $ shift (qf':ps)), not $ elem r sps]
                    constrain $ tyh `exceed` [tyc] ++ tyt `exceed` [tyc]
          elabFE (Native (Arrow (qf, qf') _ _) _ _) =
                    constrain [sparse [exchange $ Consume qf,   exchange $ Supply qf'] `Geq` 0.0]

class Elab a where
    elab :: (MonadError TypeError m, MonadState Anno m) => a -> ReaderT CheckE (WriterT ([GeneralConstraint], SharedTys Anno) m) (Ty Anno, (Anno, Anno))

instance Elab Ex where
    elab (Var x) = do
        ty <- unlessJustM (gamma x) $
                throwError $ NameError x
        ty' <- reannotate ty
        share $ singleton ty [ty']
        q  <- freshAnno
        q' <- freshAnno
        k <- costof k_var
        constrain [sparse [exchange $ Consume q, exchange $ Supply q'] `Geq` k]
        return (ty', (q, q'))
    elab (Val v) = do
        (ty, (q, q')) <- elab v
        k <- costof k_val
        constrain [sparse [exchange $ Consume q, exchange $ Supply q'] `Geq` k]
        return (ty, (q, q'))
    elab (If ep et ef) = do
        (tyep, (qip, qip')) <- elab ep
        ((tyet, (qit, qit')), (tcs, tss)) <- mapReaderT runWriterT $ elab et
        ((tyef, (qif, qif')), (fcs, fss)) <- mapReaderT runWriterT $ elab ef
        let tys = [tyep, tyet, tyef]
        constrain tcs
        constrain fcs
        sharemap <- traverse (fmap <$> (,) <*> reannotate) $ union (keys tss) (keys fss)
        share $ MonoidalMap $ fromList $ map (second pure) sharemap
        let reannotateShares ss = do
                ss' <- traverseWithKey (flip (fmap . flip (,)) . reannotate) $
                        mapKeys (fromJust . flip lookup sharemap) $ getMonoidalMap ss
                constrain $ concatMap (uncurry exceed . second (pure . fst)) $ toList ss'
                return $ MonoidalMap $ fromList $ elems ss'
        share =<< reannotateShares tss
        share =<< reannotateShares fss
        let ifty = Arrow ((), ()) [BooleanTy, Tyvar "a", Tyvar "a"] (Tyvar "a")
        Arrow (q, q') tys' ty'' <- annoMax $ instantiate (map void tys) ifty
        let tys'' = unTyList $ instantiate tys' $ TyList tys
        let [typ, tyt, tyf] = tys'
        let ineqs = filter (not . uncurry eqTy) (zip tys'' tys')
        when (not $ null ineqs) $
            throwError $ TypeError $ ineqs
        constrain $ concatMap (uncurry equate) $ zip tys'' $ map (:[]) tys'
        [kp, kt, kf, kc] <- sequence [costof k_ifp, costof k_ift, costof k_iff, costof k_ifc]
        constrain $ exceed tyt [ty''] ++ exceed tyf [ty'']
        constrain [sparse [exchange $ Consume q,    exchange $ Supply qip] `Geq` kp,
                   sparse [exchange $ Consume qip', exchange $ Supply qit] `Geq` kt,
                   sparse [exchange $ Consume qip', exchange $ Supply qif] `Geq` kf,
                   sparse [exchange $ Consume qit', exchange $ Supply q']  `Geq` kc,
                   sparse [exchange $ Consume qif', exchange $ Supply q']  `Geq` kc]
        return (ty'', (q, q'))
    elab (App f es) = do
        (tys, (qs, q's)) <- second unzip <$> unzip <$> mapM elab es
        when (arity f /= length es) $
            throwError $ ArityError (arity f) (length es)
        Arrow (qf, qf') tys' ty'' <- lookupThisSCP f >>= \case
            Just asc -> do
                degree <- degreeof
                cost_free <- costof (== zero)
                if degree <= 1 || cost_free then
                    return $ instantiate tys $ tyOf asc
                else do
                    scp <- asks (comp . checkF)
                    fun <- annoMax $ instantiate tys asc
                    -- this is cheating for polymorphic mutual recursion; should instantiate tys over the scp somehow
                    cfscp <- annotate (degree - 1) $ updateFun scp f fun
                    let Just cffun = lookupFun cfscp f
                    constrain $ equate (tyOf fun) [tyOf asc, tyOf cffun]
                    constrain =<< withReaderT (\ce -> (checkF ce) {degree = degree - 1, comp = cfscp, cost = zero}) (mapReaderT execWriterT (elabSCP cfscp))
                    return $ tyOf fun
            Nothing -> do
                scp <- unlessJustM (lookupSCP f) $
                       throwError $ NameError f
                let fun = instantiate (map void tys) $ fromJust $ lookupFun scp f
                scp' <- annoMax $ updateFun scp f fun
                constrain =<< withReaderT (\ce -> (checkF ce) {comp = scp'}) (mapReaderT execWriterT (elabSCP scp'))
                return $ tyOf $ fromJust $ lookupFun scp' f
        q  <- freshAnno
        q' <- freshAnno
        let tys'' = unTyList $ instantiate tys' $ TyList tys
        let ineqs = filter (not . uncurry eqTy) (zip tys'' tys')
        when (not $ null ineqs) $
            throwError $ TypeError $ ineqs
        constrain $ concatMap (uncurry equate) $ zip tys'' $ map (:[]) tys'
        k1 <- costof k_ap1
        k2 <- costof k_ap2
        c  <- freshAnno
        let (qs_args, ([q_ap], _)) = zipR (q:q's) qs
        constrain [sparse [exchange $ Consume q_in, exchange $ Supply q_out] `Geq` k1 |
                   (q_in, q_out) <- qs_args]
        constrain [sparse (map exchange [Consume q_ap, Supply qf, Supply c]) `Eql` k1]
        constrain [sparse (map exchange [Supply q', Consume qf', Consume c]) `Eql` k2]
        return (ty'', (q, q'))
    elab (Let ds e) = do
        (tyds, (qs, q's)) <- second unzip <$> unzip <$> mapM (fmap assoc . traverse elab) ds
        (ty, (qe, qe')) <- withReaderT (\ce -> ce {env = (reverse tyds) ++ env ce}) $ elab e
        q  <- freshAnno
        q' <- freshAnno
        k1 <- costof k_lt1
        k2 <- costof k_lt2
        constrain [sparse [exchange $ Consume q_in, exchange $ Supply q_out] `Geq` k1 |
                   (q_in, q_out) <- zip (q:q's) (qs ++ [qe])]
        constrain [sparse (map exchange [Supply q', Consume qe']) `Geq` k2]
        return (ty, (q, q'))

instance Elab Val where
    elab (Nat _) = do
        q <- freshAnno
        q' <- freshAnno
        return (NatTy, (q, q'))
    elab (Boolean _) = do
        q <- freshAnno
        q' <- freshAnno
        return (BooleanTy, (q, q'))
    elab Unit = do
        q <- freshAnno
        q' <- freshAnno
        return (UnitTy, (q, q'))
    elab (Sym _) = do
        q <- freshAnno
        q' <- freshAnno
        return (SymTy, (q, q'))
    elab (List l) = elab l

instance Elab List where
    elab Nil = do
        q <- freshAnno
        q' <- freshAnno
        ty <- annoMax $ ListTy [] $ Tyvar "a"
        return (ty, (q, q'))
    elab (Cons v vs) = do
        (vty, _) <- elab v
        (lty, (q, q')) <- elab vs
        ty <- case lty of
            ListTy ps lty' ->
                let lty'' = instantiate [vty] lty'
                    vty'' = instantiate [lty'] vty
                in if eqTy lty'' vty''
                   then return $ ListTy ps lty''
                   else throwError $ TypeError [(vty'', lty'')]
            t -> throwError $ TypeError [(ListTy [] vty, t)]
        return (ty, (q, q'))
