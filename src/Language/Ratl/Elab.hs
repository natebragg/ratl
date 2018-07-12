{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Ratl.Elab (
    Env(..),
    elaborate,
    elab,
    instantiate,
    solve,
) where

import Data.Function (on)
import Data.List (intersect, intercalate, union, unionBy, nub, foldl')
import Control.Arrow (second)
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
    TypedFun(..),
    Ex(..),
    ExTy(..),
    Prog,
    TypedProg,
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

solve :: [Ty] -> [Ty] -> TyvarEnv
solve tys tys' = compose varenv renames
    where varenv = foldl' build [] $ zip tys'' tys
          tys'' = instantiate renames tys'
          renames = zip (map varname $ intersect frees bound) (map (Tyvar . varname) [next_var..])
          next_var = 1 + (maximum $ union frees bound)
          frees = nub $ map varnum $ concatMap freein tys
          bound = nub $ map varnum $ concatMap freein tys'
          compose theta2 theta1 = unionBy ((==) `on` fst) (instantiate theta2 theta1) theta2
          build theta (ty, ty') = compose (assoc (instantiate theta ty) (instantiate theta ty')) theta
          assoc       (ListTy ty)        (ListTy ty') = assoc ty ty'
          assoc (PairTy (t1, t2)) (PairTy (t1', t2')) = assoc t1 t1' ++ assoc t2 t2'
          assoc         (Tyvar x)                 ty' = [(x, ty')]
          assoc                 _                   _ = []

class Instantiable t where
    instantiate :: TyvarEnv -> t -> t

instance (Functor f, Instantiable a) => Instantiable (f a) where
    instantiate theta = fmap (instantiate theta)

instance Instantiable Ty where
    instantiate theta = subst
      where subst             NatTy = NatTy
            subst       (ListTy ty) = ListTy $ subst ty
            subst (PairTy (t1, t2)) = PairTy (subst t1, subst t2)
            subst         BooleanTy = BooleanTy
            subst            UnitTy = UnitTy
            subst             SymTy = SymTy
            subst         (Tyvar x) = varsubst x
            varsubst x = maybe (Tyvar x) id $ lookup x theta

instance Instantiable FunTy where
    instantiate theta (Arrow ty ty') = Arrow (instantiate theta ty) (instantiate theta ty')

instance Instantiable Fun where
    instantiate theta (Fun ty x e) = Fun (instantiate theta ty) x e
    instantiate theta (Native ty a f) = Native (instantiate theta ty) a f

instance Instantiable TypedFun where
    instantiate theta (TypedFun ty x e) = TypedFun (instantiate theta ty) x (instantiate theta e)
    instantiate theta (TypedNative ty a f) = TypedNative (instantiate theta ty) a f

instance Instantiable ExTy where
    instantiate theta (VarTy ty x) = VarTy (instantiate theta ty) x
    instantiate theta (ValTy ty v) = ValTy (instantiate theta ty) v
    instantiate theta (IfTy ty ep et ef) = IfTy (instantiate theta ty) (instantiate theta ep) (instantiate theta et) (instantiate theta ef)
    instantiate theta (AppTy ty f es) = AppTy (instantiate theta ty) f (map (instantiate theta) es)
    instantiate theta (LetTy ty ds e) = LetTy (instantiate theta ty) (map (fmap (instantiate theta)) ds) (instantiate theta e)

class Elab a where
    type Type a :: *
    elab :: MonadError TypeError m => a -> ReaderT Env m (Type a)

instance Elab Prog where
    type Type Prog = TypedProg
    elab = travFun (traverse elab)

instance Elab Fun where
    type Type Fun = TypedFun
    elab (Fun fty@(Arrow pty rty) x e) = do
        ety <- withReaderT (\ce -> ce {gamma = zip [x] (unpair pty)}) $ elab e
        let theta = solve [rty] [tyGet ety]
            ety' = instantiate theta ety
            ty'' = tyGet ety'
        when (rty /= ty'') $
            throwError $ TypeError $ [(rty, ty'')]
        return $ TypedFun fty x ety'
    elab (Native fty a f) =
        return $ TypedNative fty a f

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
        let ifty   = [BooleanTy, Tyvar "a", Tyvar "a"]
            theta1 = solve (map tyGet etys) ifty
            tys    = instantiate theta1 ifty
            theta2 = solve tys (map tyGet etys)
            etys'@[etyp', etyt', etyf'] = instantiate theta2 etys
            tys'   = map tyGet etys'
            ineqs  = filter (uncurry (/=)) (zip tys' tys)
        when (not $ null ineqs) $
            throwError $ TypeError $ ineqs
        return $ IfTy (last tys') etyp' etyt' etyf'
    elab (App f es) = do
        etys <- traverse elab es
        when (arity f /= length es) $
            throwError $ ArityError (arity f) (length es)
        Arrow ty ty' <- unlessJustM (asks $ lookup f . phi) $
            throwError $ NameError f
        let appty  = unpair ty
            theta1 = solve (map tyGet etys) (appty ++ [ty'])
            tys    = instantiate theta1 appty
            ty''   = instantiate theta1 ty'
            theta2 = solve tys (map tyGet etys)
            etys'  = instantiate theta2 etys
            tys'   = map tyGet etys'
            ineqs  = filter (uncurry (/=)) (zip tys' tys)
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
                let theta1 = solve [vty] [lty']
                    lty''  = instantiate theta1 lty'
                    theta2 = solve [lty'] [vty]
                    vty''  = instantiate theta2 vty
                in if lty'' == vty''
                   then return $ ListTy lty''
                   else throwError $ TypeError [(vty'', lty'')]
            t -> throwError $ TypeError [(ListTy vty, t)]
        return ty

elaborate :: (Elab a, MonadError TypeError m) => Prog -> a -> m (Type a)
elaborate p a = runReaderT (elab a) (Env {phi = mapFun (second tyOf) p, gamma = []})
