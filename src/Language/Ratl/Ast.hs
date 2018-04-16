{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Language.Ratl.Ast (
    Embeddable(..),
    NativeError(..),
    List(..),
    Sym(..),
    Var(..),
    Val(..),
    Fun(..),
    Ex(..),
    Prog,
    tyOf,
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

import Language.Ratl.Ty (FunTy)
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

projectionBug v = error $ "Tried to project to wrong type, which should be impossible: " ++ show v

class Embeddable a where
    embed :: a -> Val
    project :: Val -> a

newtype Nat = N Int
    deriving (Eq)

instance Embeddable Int where
    embed = Nat . N . max 0
    project (Nat (N n)) = n
    project v = projectionBug v

instance Show Nat where
    show (N n) = show n

data List = Nil | Cons Val List
    deriving (Eq)

instance Embeddable a => Embeddable [a] where
    embed = List . go
        where go [] = Nil
              go (v:vs) = Cons (embed v) $ go vs

    project (List a) = go a
        where go Nil = []
              go (Cons v vs) = project v:go vs
    project v = projectionBug v

instance Show List where
    show l = beg ++ go "" l ++ end
        where (beg, sep, end) = ("(", " ", ")")
              go _ Nil = ""
              go c (Cons v vs) = c ++ show v ++ go sep vs

newtype Boolean = B Bool
    deriving (Eq)

instance Embeddable Bool where
    embed = Boolean . B
    project (Boolean (B b)) = b
    project v = projectionBug v

instance Show Boolean where
    show (B True) = "#t"
    show (B False) = "#f"

instance Embeddable () where
    embed () = Unit
    project Unit = ()
    project v = projectionBug v

newtype Sym = S String
    deriving (Eq)

instance Show Sym where
    show (S x) = x

instance Embeddable Sym where
    embed = Sym
    project (Sym x) = x
    project v = projectionBug v

data Val = List List
         | Nat Nat
         | Boolean Boolean
         | Unit
         | Sym Sym
    deriving (Eq)

instance Embeddable Val where
    embed = id
    project = id

instance Show Val where
    show (List xs) = show xs
    show (Nat n) = show n
    show (Boolean b) = show b
    show (Sym s) = show s
    show Unit = show ()

data Var = V String
    deriving (Eq)

instance Show Var where
    show (V x) = x

data NativeError = EmptyError
                 | DivideByZeroError

instance Show NativeError where
    show EmptyError = "Tried to access contents of empty list."
    show DivideByZeroError = "Tried to divide by zero."

data Fun where
    Fun :: FunTy -> Var -> Ex -> Fun
    Native :: FunTy -> Int -> ([Val] -> Except NativeError Val) -> Fun

tyOf :: Fun -> FunTy
tyOf (Fun ty _ _) = ty
tyOf (Native ty _ _) = ty

instance Show Fun where
    show _ = "(define ...)"

data Ex = Var Var
        | Val Val
        | App Var [Ex]
        | If Ex Ex Ex
        | Let [(Var, Ex)] Ex
    deriving (Show, Eq)

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
