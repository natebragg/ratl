{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Ratl.Elab (
    Env(..),
    elaborate,
    elab,
    instantiate,
) where

import Data.Foldable (traverse_)
import Data.List (intersect, intercalate, union, nub, foldl')
import Control.Arrow (second, (&&&))
import Control.Monad (when, void)
import Control.Monad.Except (MonadError(..))
import Control.Monad.Except.Extra (unlessJustM)
import Control.Monad.Reader (runReaderT, withReaderT, ReaderT, asks)

import Language.Ratl.Ty (
    Ty(..),
    varname,
    varnum,
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
    mapFun,
    travFun,
    )
import Language.Ratl.Basis (arity)

type TyEnv = [(Var, Ty)]
type FunTyEnv = [(Var, FunTy)]
type TyvarEnv = [(String, Ty)]

data Env = Env {
        gamma :: TyEnv,
        phi :: FunTyEnv
    }

data TypeError = TypeError [(Ty, Ty)]
               | ArityError Int Int
               | NameError Var

instance Show TypeError where
    show (TypeError ts) = "Cannot unify unlike types: " ++ intercalate ", " (map (\(t, t') -> show t ++ " and " ++ show t') ts)
    show (ArityError f a) = "Expected " ++ show f ++ " arguments, but got " ++ show a ++ "."
    show (NameError x) = "Name " ++ show x ++ " is not defined."

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
    instantiate tys (Arrow ty ty') = repair $ instantiate tys $ unpair ty ++ [ty']
        where repair [ty, ty'] = Arrow ty ty'
              repair (t1:tys) = let Arrow t2 ty' = repair tys in Arrow (PairTy (t1, t2)) ty'

instance Instantiable Fun where
    instantiate tys (Fun ty x e) = Fun (instantiate tys ty) x e
    instantiate tys (Native ty a f) = Native (instantiate tys ty) a f

instance Instantiable [ExTy] where
    instantiate = zipWith (instantiate . pure)

instance Instantiable ExTy where
    instantiate tys (VarTy ty x) = VarTy (instantiate tys ty) x
    instantiate tys (ValTy ty v) = ValTy (instantiate tys ty) v
    instantiate tys (IfTy ty ep et ef) = IfTy (instantiate tys ty) (instantiate tys ep) (instantiate tys et) (instantiate tys ef)
    instantiate tys (AppTy ty f es) = AppTy (instantiate tys ty) f (map (instantiate tys) es)
    instantiate tys (LetTy ty ds e) = LetTy (instantiate tys ty) (map (fmap (instantiate tys)) ds) (instantiate tys e)

class Elab a where
    type Type a :: *
    elab :: MonadError TypeError m => a -> ReaderT Env m (Type a)

instance Elab Prog where
    type Type Prog = ()
    elab = void . travFun (traverse_ elab)

instance Elab Fun where
    type Type Fun = FunTy
    elab (Fun fty@(Arrow pty rty) x e) = do
        ety <- withReaderT (\ce -> ce {gamma = zip [x] (unpair pty)}) $ elab e
        let ety' = instantiate [rty] ety
            ty'' = tyGet ety'
        when (rty /= ty'') $
            throwError $ TypeError $ [(rty, ty'')]
        return fty
    elab (Native fty _ _) = return fty

instance Elab Ex where
    type Type Ex = ExTy
    elab (Var x) = do
        ty <- unlessJustM (asks $ lookup x . gamma) $
            throwError $ NameError x
        return $ VarTy ty x
    elab (Val v) = do
        ty <- elab v
        return $ ValTy ty v
    elab (If ep et ef) = do
        etys <- traverse elab [ep, et, ef]
        let tys = map tyGet etys
            tys' = instantiate tys [BooleanTy, Tyvar "a", Tyvar "a"]
            etys'@[etyp', etyt', etyf'] = instantiate tys' etys
            tys'' = map tyGet etys'
            ineqs = filter (uncurry (/=)) (zip tys'' tys')
        when (not $ null ineqs) $
            throwError $ TypeError $ ineqs
        return $ IfTy (last tys') etyp' etyt' etyf'
    elab (App f es) = do
        etys <- traverse elab es
        let tys = map tyGet etys
        when (arity f /= length es) $
            throwError $ ArityError (arity f) (length es)
        Arrow ty ty'' <- unlessJustM (asks $ fmap (instantiate tys) . lookup f . phi) $
            throwError $ NameError f
        let tys' = unpair ty
            etys' = instantiate tys' etys
            tys'' = map tyGet etys'
            ineqs = filter (uncurry (/=)) (zip tys'' tys')
        when (not $ null ineqs) $
            throwError $ TypeError $ ineqs
        return $ AppTy ty'' f etys'
    elab (Let ds e) = do
        etyds <- traverse (traverse elab) ds
        let tyds = map (fmap tyGet) etyds
        ety <- withReaderT (\ce -> ce {gamma = reverse tyds ++ gamma ce}) $ elab e
        let ty = tyGet ety
        return $ LetTy ty etyds ety

instance Elab Val where
    type Type Val = Ty
    elab (Nat _)     = return NatTy
    elab (Boolean _) = return BooleanTy
    elab Unit        = return UnitTy
    elab (Sym _)     = return SymTy
    elab (List l)    = elab l

instance Elab List where
    type Type List = Ty
    elab Nil = return $ ListTy $ Tyvar "a"
    elab (Cons v vs) = do
        vty <- elab v
        lty <- elab vs
        ty <- case lty of
            ListTy lty' ->
                let lty'' = instantiate [vty] lty'
                    vty'' = instantiate [lty'] vty
                in if lty'' == vty''
                   then return $ ListTy lty''
                   else throwError $ TypeError [(vty'', lty'')]
            t -> throwError $ TypeError [(ListTy vty, t)]
        return ty

elaborate :: (Elab a, MonadError TypeError m) => Prog -> a -> m (Type a)
elaborate p a = runReaderT (elab a) (Env {phi = mapFun (second tyOf) p, gamma = []})
