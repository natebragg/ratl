{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Language.Ratl.Ast (
    NativeError(..),
    Var(..),
    Fun(Fun, Native),
    TypedFun(TypedFun, TypedNative),
    Ex(Var, Val, App, If, Let),
    ExTy(VarTy, ValTy, AppTy, IfTy, LetTy),
    Prog,
    TypedProg,
    tyOf,
    tyGet,
    makeProg,
    lookupFun,
    updateFun,
    mapFun,
    travFun,
    mapProg,
    travProg,
    connects,
    scSubprograms,
) where

import Control.Arrow ((***))
import Control.Monad.Except (Except)
import Data.Graph.Inductive.Graph (mkGraph, insEdges)
import Data.Graph.Inductive.PatriciaTree (Gr(..))
import Data.Graph.Inductive.Extra (
    makeEdgesWhere,
    scSubgraphs,
    OverNodes(..),
    )
import Data.Foldable (find, toList)
import Data.Fix (Fix(..))
import Language.Ratl.Ty (
    FunTy,
    Ty,
    )
import Language.Ratl.Val (Val)


data Var = V String
    deriving (Eq)

instance Show Var where
    show (V x) = x

data NativeError = EmptyError
                 | DivideByZeroError

instance Show NativeError where
    show EmptyError = "Tried to access contents of empty list."
    show DivideByZeroError = "Tried to divide by zero."

class Typed t where
    tyOf :: t -> FunTy

data FunRep e where
    FunRep :: FunTy -> Var -> e -> FunRep e
    NativeRep :: FunTy -> Int -> ([Val] -> Except NativeError Val) -> FunRep e

instance Show (FunRep e) where
    show _ = "(define ...)"

newtype Fun = F { unfun :: FunRep Ex }

{-# COMPLETE Fun, Native #-}

pattern Fun ty x e = F (FunRep ty x e)
pattern Native ty a f = F (NativeRep ty a f)

instance Show Fun where
    show = show . unfun

instance Typed Fun where
    tyOf (Fun ty _ _) = ty
    tyOf (Native ty _ _) = ty

newtype TypedFun = TypedF { untypedfun :: FunRep ExTy }

{-# COMPLETE TypedFun, TypedNative #-}

pattern TypedFun ty x e = TypedF (FunRep ty x e)
pattern TypedNative ty a f = TypedF (NativeRep ty a f)

instance Show TypedFun where
    show = show . untypedfun

instance Typed TypedFun where
    tyOf (TypedFun ty _ _) = ty
    tyOf (TypedNative ty _ _) = ty

data ExRep e = VarRep Var
             | ValRep Val
             | AppRep Var [e]
             | IfRep e e e
             | LetRep [(Var, e)] e
    deriving Eq

instance Show e => Show (ExRep e) where
    show (VarRep x) = show x
    show (ValRep v) = show v
    show (AppRep x es) = "(" ++ unwords (show x:map show es) ++ ")"
    show (IfRep ep et ef) = "(if " ++ show ep ++ " " ++ show et ++ " " ++ show ef ++ ")"
    show (LetRep ds e) = "(let (" ++ unwords (map showdec ds) ++ ") " ++ show e ++ ")"
            where showdec (x, e) = "(" ++ show x ++ " " ++ show e ++ ")"

newtype Ex = Ex { unex :: Fix ExRep }
    deriving Eq

{-# COMPLETE Var, Val, App, If, Let #-}

pattern Var x = Ex (Fix (VarRep x))
pattern Val v = Ex (Fix (ValRep v))
pattern App x es <- ((\case
            ex@(Ex (Fix (AppRep _ es))) -> (ex, map Ex es)
            ex -> (ex, undefined)) -> (Ex (Fix (AppRep x _)), es))
    where App x es = Ex (Fix (AppRep x (map unex es)))
pattern If ep et ef <- ((\case
            ex@(Ex (Fix (IfRep ep et ef))) -> (ex, Ex ep, Ex et, Ex ef)
            ex -> (ex, undefined, undefined, undefined)) -> (Ex (Fix (IfRep _ _ _)), ep, et, ef))
    where If ep et ef = Ex (Fix (IfRep (unex ep) (unex et) (unex ef)))
pattern Let ds e <- ((\case
            ex@(Ex (Fix (LetRep ds e))) -> (ex, fmap (fmap Ex) ds, Ex e)
            ex -> (ex, undefined, undefined)) -> (Ex (Fix (LetRep _ _)), ds, e))
    where Let ds e = Ex (Fix (LetRep (fmap (fmap unex) ds) (unex e)))

instance Show Ex where
    show = show . unex

newtype TyRep a = TyRep { untyrep :: (Ty, ExRep a) }

instance Show a => Show (TyRep a) where
    show = show . snd . untyrep

newtype ExTy = ExTy { unexty :: Fix TyRep }

{-# COMPLETE VarTy, ValTy, AppTy, IfTy, LetTy #-}

pattern VarTy ty x = ExTy (Fix (TyRep (ty, VarRep x)))
pattern ValTy ty v = ExTy (Fix (TyRep (ty, ValRep v)))
pattern AppTy ty x es <- ((\case
            ex@(ExTy (Fix (TyRep (_, AppRep _ es)))) -> (ex, map ExTy es)
            ex -> (ex, undefined)) -> (ExTy (Fix (TyRep (ty, AppRep x _))), es))
    where AppTy ty x es = ExTy (Fix (TyRep (ty, AppRep x (map unexty es))))
pattern IfTy ty ep et ef <- ((\case
            ex@(ExTy (Fix (TyRep (_, IfRep ep et ef)))) -> (ex, ExTy ep, ExTy et, ExTy ef)
            ex -> (ex, undefined, undefined, undefined)) -> (ExTy (Fix (TyRep (ty, IfRep _ _ _))), ep, et, ef))
    where IfTy ty ep et ef = ExTy (Fix (TyRep (ty, IfRep (unexty ep) (unexty et) (unexty ef))))
pattern LetTy ty ds e <- ((\case
            ex@(ExTy (Fix (TyRep (_, LetRep ds e)))) -> (ex, map (fmap ExTy) ds, ExTy e)
            ex -> (ex, undefined, undefined)) -> (ExTy (Fix (TyRep (ty, LetRep _ _))), ds, e))
    where LetTy ty ds e = ExTy (Fix (TyRep (ty, LetRep (map (fmap unexty) ds) (unexty e))))

instance Show ExTy where
    show = show . unexty

tyGet :: ExTy -> Ty
tyGet = fst . untyrep . unfix . unexty

newtype Prog = Prog {getProg :: Gr (Var, Fun) ()}
    deriving (Monoid)

makeProg :: [(Var, Fun)] -> Prog
makeProg = Prog . flip mkGraph [] . zip [1..]

lookupFun :: Prog -> Var -> Maybe Fun
lookupFun = lookup . OverNodes . getProg
    where lookup t key = fmap snd $ find ((key ==) . fst) t

updateFun :: Prog -> Var -> Fun -> Prog
updateFun p x f = case lookupFun p x of
    Just _ -> mapProg (\(y, g) -> (y, if x == y then f else g)) p
    Nothing -> p `mappend` makeProg [(x, f)]

mapFun :: ((Var, Fun) -> b) -> Prog -> [b]
mapFun f (Prog fs) = foldMap ((:[]) . f) $ OverNodes fs

mapProg :: ((Var, Fun) -> (Var, Fun)) -> Prog -> Prog
mapProg f (Prog fs) = Prog $ getOverNodes $ fmap f $ OverNodes fs

travFun :: Applicative f => ((Var, Fun) -> f b) -> Prog -> f [b]
travFun f (Prog fs) = toList <$> traverse f (OverNodes fs)

travProg :: Applicative f => ((Var, Fun) -> f (Var, Fun)) -> Prog -> f Prog
travProg f (Prog fs) = Prog <$> (getOverNodes <$> traverse f (OverNodes fs))

connects :: [(Var, Var)] -> Prog -> Prog
connects vs (Prog fs) = Prog $ insEdges (concatMap firstEdge vs) fs
    where firstEdge = take 1 . makeEdgesWhere fs () . (named *** named)
          named = (. fst) . (==)

scSubprograms :: Prog -> [Prog]
scSubprograms = map Prog . scSubgraphs . getProg

type TypedProg = [(Var, TypedFun)]
