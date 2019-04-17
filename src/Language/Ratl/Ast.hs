{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}

module Language.Ratl.Ast (
    RuntimeError(..),
    Var(..),
    Fun(Fun, Native),
    TypedFun(TypedFun, TypedNative),
    Ex(Var, Val, App, If, Let),
    TypedEx(TypedVar, TypedVal, TypedApp, TypedIf, TypedLet),
    Prog,
    TypedProg,
    tyOf,
    tyPut,
    tyGet,
    tySet,
    freeVars,
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
import Data.List (nub)
import Data.Mapping (deleteAll)
import Language.Ratl.Ty (
    FunTy,
    Ty,
    )
import Language.Ratl.Val (Val)


data Var = V String
    deriving (Eq, Ord)

instance Show Var where
    show (V x) = x

data RuntimeError = EmptyError
                  | DivideByZeroError
                  | NameError Var
                  | UserError Val

instance Show RuntimeError where
    show EmptyError = "Tried to access contents of empty list."
    show DivideByZeroError = "Tried to divide by zero."
    show (NameError x) = "Name " ++ show x ++ " is not defined."
    show (UserError v) = "Error: " ++ show v

class Typed t where
    tyOf :: t -> FunTy
    tyPut :: FunTy -> t -> t

data FunRep e where
    FunRep :: FunTy -> [Var] -> e -> FunRep e
    NativeRep :: FunTy -> ([Val] -> Except RuntimeError Val) -> FunRep e

instance Show (FunRep e) where
    show _ = "(define ...)"

newtype Fun = F { unfun :: FunRep Ex }

{-# COMPLETE Fun, Native #-}

pattern Fun ty xs e = F (FunRep ty xs e)
pattern Native ty f = F (NativeRep ty f)

instance Show Fun where
    show = show . unfun

instance Typed Fun where
    tyOf (Fun ty _ _) = ty
    tyOf (Native ty _) = ty

    tyPut ty (Fun _ xs e) = Fun ty xs e
    tyPut ty (Native _ f) = Native ty f

newtype TypedFun = TypedF { untypedfun :: FunRep TypedEx }

{-# COMPLETE TypedFun, TypedNative #-}

pattern TypedFun ty xs e = TypedF (FunRep ty xs e)
pattern TypedNative ty f = TypedF (NativeRep ty f)

instance Show TypedFun where
    show = show . untypedfun

instance Typed TypedFun where
    tyOf (TypedFun ty _ _) = ty
    tyOf (TypedNative ty _) = ty

    tyPut ty (TypedFun _ xs e) = TypedFun ty xs e
    tyPut ty (TypedNative _ f) = TypedNative ty f

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
    show (LetRep bs e) = "(let (" ++ unwords (map showbind bs) ++ ") " ++ show e ++ ")"
            where showbind (x, e) = "(" ++ show x ++ " " ++ show e ++ ")"

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
pattern Let bs e <- ((\case
            ex@(Ex (Fix (LetRep bs e))) -> (ex, fmap (fmap Ex) bs, Ex e)
            ex -> (ex, undefined, undefined)) -> (Ex (Fix (LetRep _ _)), bs, e))
    where Let bs e = Ex (Fix (LetRep (fmap (fmap unex) bs) (unex e)))

instance Show Ex where
    show = show . unex

newtype TyRep a = TyRep { untyrep :: (Ty, ExRep a) }

instance Show a => Show (TyRep a) where
    show = show . snd . untyrep

newtype TypedEx = TypedEx { untypedex :: Fix TyRep }

{-# COMPLETE TypedVar, TypedVal, TypedApp, TypedIf, TypedLet #-}

pattern TypedVar ty x = TypedEx (Fix (TyRep (ty, VarRep x)))
pattern TypedVal ty v = TypedEx (Fix (TyRep (ty, ValRep v)))
pattern TypedApp ty x es <- ((\case
            ex@(TypedEx (Fix (TyRep (_, AppRep _ es)))) -> (ex, map TypedEx es)
            ex -> (ex, undefined)) -> (TypedEx (Fix (TyRep (ty, AppRep x _))), es))
    where TypedApp ty x es = TypedEx (Fix (TyRep (ty, AppRep x (map untypedex es))))
pattern TypedIf ty ep et ef <- ((\case
            ex@(TypedEx (Fix (TyRep (_, IfRep ep et ef)))) -> (ex, TypedEx ep, TypedEx et, TypedEx ef)
            ex -> (ex, undefined, undefined, undefined)) -> (TypedEx (Fix (TyRep (ty, IfRep _ _ _))), ep, et, ef))
    where TypedIf ty ep et ef = TypedEx (Fix (TyRep (ty, IfRep (untypedex ep) (untypedex et) (untypedex ef))))
pattern TypedLet ty bs e <- ((\case
            ex@(TypedEx (Fix (TyRep (_, LetRep bs e)))) -> (ex, map (fmap TypedEx) bs, TypedEx e)
            ex -> (ex, undefined, undefined)) -> (TypedEx (Fix (TyRep (ty, LetRep _ _))), bs, e))
    where TypedLet ty bs e = TypedEx (Fix (TyRep (ty, LetRep (map (fmap untypedex) bs) (untypedex e))))

instance Show TypedEx where
    show = show . untypedex

tyGet :: TypedEx -> Ty
tyGet = fst . untyrep . unfix . untypedex

tySet :: Ty -> TypedEx -> TypedEx
tySet ty = TypedEx . Fix . TyRep . (,) ty . snd . untyrep . unfix . untypedex

freeVars :: TypedEx -> [(Var, Ty)]
freeVars (TypedVar ty x)      = [(x, ty)]
freeVars (TypedVal _ _)       = []
freeVars (TypedApp _ _ es)    = nub $ concatMap freeVars es
freeVars (TypedIf _ ep et ef) = nub $ concatMap freeVars [ep, et, ef]
freeVars (TypedLet _ bs e)    = nub $ let (xs, es) = unzip bs in concatMap freeVars es ++ (deleteAll xs $ freeVars e)

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
