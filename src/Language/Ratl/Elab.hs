{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Language.Ratl.Elab (
    check,
) where

import Data.List (intersect, union, nub, foldl', transpose)
import Data.Map (foldMapWithKey, traverseWithKey, elems, fromList, toList, mapKeys)
import Data.Map.Monoidal (MonoidalMap(..), singleton, keys)
import Data.Maybe (listToMaybe, isJust, fromJust)
import Control.Applicative (empty)
import Control.Arrow (first, second, (&&&))
import Control.Monad (guard, forM, MonadPlus(..), void)
import Control.Monad.State (MonadState)
import Control.Monad.Reader (MonadReader, runReaderT, withReaderT, mapReaderT, ReaderT, asks)
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
    tyOf,
    lookupFun,
    updateFun,
    mapFun,
    travFun,
    connects,
    scSubprograms,
    )

type TyEnv a = [(Var, Ty a)]
type SharedTys a = MonoidalMap (Ty a) [Ty a]
type TyvarEnv a = [(String, Ty a)]
type ProgEnv = [(Var, [GeneralForm])]

data Cost = Cost { k_var, k_val, k_ap1, k_ap2, k_ifp, k_ift, k_iff, k_ifc :: Double }
    deriving Eq

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

zero = Cost {
        k_var = 0.0,
        k_val = 0.0,
        k_ap1 = 0.0,
        k_ap2 = 0.0,
        k_ifp = 0.0,
        k_ift = 0.0,
        k_iff = 0.0,
        k_ifc = 0.0
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

instance Instantiable FunTy where
    instantiate tys (Arrow q tys' ty') = Arrow q (init tys''ty'') (last tys''ty'')
        where tys''ty'' = unTyList $ instantiate tys (TyList $ tys' ++ [ty'])

instance Instantiable Fun where
    instantiate tys (Fun ty x e) = Fun (instantiate tys ty) x e
    instantiate tys (Native ty a f) = Native (instantiate tys ty) a f

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

zipR :: [a] -> [b] -> ([(a,b)], ([a], [b]))
zipR     []     bs = ([], ([], bs))
zipR     as     [] = ([], (as, []))
zipR (a:as) (b:bs) = first ((a,b) :) $ zipR as bs

check :: (MonadPlus m, MonadState Anno m) => Int -> Prog () -> m ProgEnv
check deg_max p_ = programs
    where p = callgraph p_
          scps = scSubprograms p
          lookupSCP :: Var -> Maybe (Prog ())
          lookupSCP x = listToMaybe $ filter (isJust . flip lookupFun x) scps
          share m = tell (mempty, m)
          constrain c = tell (c, mempty)
          gamma x = asks (lookup x . env)
          costof k = asks (k . cost . checkF)
          degreeof :: MonadReader CheckE m => m Int
          degreeof = asks (degree . checkF)
          annoMax a = degreeof >>= flip annotate a
          lookupThisSCP x = asks (flip lookupFun x . comp . checkF)
          programs = fmap concat $ forM scps $ \scp -> do
                    scp' <- annotate deg_max scp
                    (los, cs) <- runWriterT $ runReaderT (elabSCP scp') $ CheckF {degree = deg_max, comp = scp', cost = constant}
                    return $ map (second $ \os -> [GeneralForm Minimize o cs | o <- os]) los
          elabSCP :: (MonadPlus m, MonadState Anno m) => Prog Anno -> ReaderT CheckF (WriterT [GeneralConstraint] m) [(Var, [LinearFunction])]
          elabSCP = travFun (traverse elabF)
          elabF :: (MonadPlus m, MonadState Anno m) => Fun Anno -> ReaderT CheckF (WriterT [GeneralConstraint] m) [LinearFunction]
          elabF f = do
                    mapReaderT (mapWriterT (fmap $ second $ uncurry (++) . second (foldMapWithKey equate . getMonoidalMap))) $ elabFE f
                    return $ map (objective (tyOf f)) [deg_max,deg_max-1..0]
          elabFE :: (MonadPlus m, MonadState Anno m) => Fun Anno -> ReaderT CheckF (WriterT ([GeneralConstraint], SharedTys Anno) m) ()
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
          elabE :: (MonadPlus m, MonadState Anno m) => Ex -> ReaderT CheckE (WriterT ([GeneralConstraint], SharedTys Anno) m) (Ty Anno, (Anno, Anno))
          elabE (Var x)    = do ty <- hoist =<< gamma x
                                ty' <- reannotate ty
                                share $ singleton ty [ty']
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
          elabE (If ep et ef) = do
                                (tyep, (qip, qip')) <- elabE ep
                                ((tyet, (qit, qit')), (tcs, tss)) <- mapReaderT runWriterT $ elabE et
                                ((tyef, (qif, qif')), (fcs, fss)) <- mapReaderT runWriterT $ elabE ef
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
                                let ifty = Arrow ((), ()) [Tyvar "a", Tyvar "b", Tyvar "b"] (Tyvar "b")
                                Arrow (q, q') tys' ty'' <- annoMax $ instantiate (map void tys) ifty
                                let tys'' = unTyList $ instantiate tys' $ TyList tys
                                let [typ, tyt, tyf] = tys'
                                guard $ all (uncurry eqTy) (zip tys'' tys')
                                constrain $ concatMap (uncurry equate) $ zip tys'' $ map (:[]) tys'
                                [kp, kt, kf, kc] <- sequence [costof k_ifp, costof k_ift, costof k_iff, costof k_ifc]
                                constrain $ exceed tyt [ty''] ++ exceed tyf [ty'']
                                constrain [Sparse [exchange $ Consume q,    exchange $ Supply qip] `Geq` kp,
                                           Sparse [exchange $ Consume qip', exchange $ Supply qit] `Geq` kt,
                                           Sparse [exchange $ Consume qip', exchange $ Supply qif] `Geq` kf,
                                           Sparse [exchange $ Consume qit', exchange $ Supply q']  `Geq` kc,
                                           Sparse [exchange $ Consume qif', exchange $ Supply q']  `Geq` kc]
                                return (ty'', (q, q'))
          elabE (App f es) = do (tys, (qs, q's)) <- second unzip <$> unzip <$> mapM elabE es
                                Arrow (qf, qf') tys' ty'' <- lookupThisSCP f >>= \case
                                    Just asc -> do
                                        degree <- degreeof
                                        cost_free <- costof (== zero)
                                        if degree <= 1 || cost_free then
                                            return $ instantiate tys $ tyOf asc
                                        else do
                                            scp <- asks (comp . checkF)
                                            fun <- annoMax $ instantiate tys asc
                                            cffun <- annotate (degree - 1) asc
                                            constrain $ equate (tyOf fun) [tyOf asc, tyOf cffun]
                                            let scp' = updateFun scp f cffun
                                            withReaderT (const $ CheckF {degree = degree - 1, comp = scp', cost = zero}) (elabFE cffun)
                                            return $ tyOf fun
                                    Nothing -> do
                                        scp <- hoist (lookupSCP f)
                                        fun <- instantiate (map void tys) <$> hoist (lookupFun scp f)
                                        scp' <- annoMax $ updateFun scp f fun
                                        constrain =<< withReaderT (\ce -> (checkF ce) {comp = scp'}) (mapReaderT execWriterT (elabSCP scp'))
                                        tyOf <$> hoist (lookupFun scp' f)
                                q  <- freshAnno
                                q' <- freshAnno
                                let tys'' = unTyList $ instantiate tys' $ TyList tys
                                guard $ all (uncurry eqTy) (zip tys'' tys')
                                constrain $ concatMap (uncurry equate) $ zip tys'' $ map (:[]) tys'
                                k1 <- costof k_ap1
                                k2 <- costof k_ap2
                                c  <- freshAnno
                                let (qs_args, ([q_ap], _)) = zipR (q:q's) qs
                                constrain [Sparse [exchange $ Consume q_in, exchange $ Supply q_out] `Geq` k1 |
                                           (q_in, q_out) <- qs_args]
                                constrain [Sparse (map exchange [Consume q_ap, Supply qf, Supply c]) `Eql` k1]
                                constrain [Sparse (map exchange [Supply q', Consume qf', Consume c]) `Eql` k2]
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
