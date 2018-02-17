{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Ratl.Ast (
    Embeddable(..),
    Nat(..),
    List(..),
    Boolean(..),
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
import Data.Graph.Inductive.Graph (mkGraph, insEdges)
import Data.Graph.Inductive.PatriciaTree (Gr(..))
import Data.Graph.Inductive.Extra (
    makeEdgesWhere,
    scSubgraphs,
    OverNodes(..),
    )
import Data.Foldable (find, toList)

class Embeddable a where
    type HostType a :: *
    embed :: HostType a -> a
    project :: a -> HostType a

data Nat = N Int
    deriving (Eq)

instance Embeddable Nat where
    type HostType Nat = Int
    embed = N . max 0

    project (N n) = n

instance Show Nat where
    show n = show $ project n

data List = Nil | Cons Val List
    deriving (Eq)

instance Embeddable List where
    type HostType List = [Val]
    embed [] = Nil
    embed (v:vs) = Cons v (embed vs)

    project Nil = []
    project (Cons v vs) = v:project vs

instance Show List where
    show l = show $ project l

newtype Boolean = B Bool
    deriving (Eq)

instance Embeddable Boolean where
    type HostType Boolean = Bool
    embed = B
    project (B b) = b

instance Show Boolean where
    show (B True) = "#t"
    show (B False) = "#f"

data Var = V String
    deriving (Eq)

instance Show Var where
    show (V x) = x

data Val = List List
         | Nat Nat
         | Boolean Boolean
    deriving (Eq)

instance Show Val where
    show (List xs) = show xs
    show (Nat n) = show n
    show (Boolean b) = show b

data Fun a = Fun (FunTy a) Var Ex
           | Native (FunTy a) Int ([Val] -> Val)

tyOf :: Fun a -> FunTy a
tyOf (Fun ty _ _) = ty
tyOf (Native ty _ _) = ty

instance Show (Fun a) where
    show _ = "(define ...)"

instance Functor Fun where
    fmap f (Fun ty x e) = Fun (fmap f ty) x e
    fmap f (Native ty a g) = Native (fmap f ty) a g

instance Foldable Fun where
    foldMap f (Fun ty _ _) = foldMap f ty
    foldMap f (Native ty _ _) = foldMap f ty

instance Traversable Fun where
    traverse f (Fun ty x e) = Fun <$> traverse f ty <*> pure x <*> pure e
    traverse f (Native ty a g) = Native <$> traverse f ty <*> pure a <*> pure g

data Ex = Var Var | Val Val | App Var [Ex] | If Ex Ex Ex
    deriving (Show, Eq)

newtype Prog a = Prog {getProg :: Gr (Var, Fun a) ()}
    deriving (Monoid)

instance Functor Prog where
    fmap f (Prog fs) = Prog $ getOverNodes $ fmap (fmap (fmap f)) $ OverNodes fs

instance Foldable Prog where
    foldMap f (Prog fs) = foldMap (foldMap (foldMap f)) $ OverNodes fs

instance Traversable Prog where
    traverse f (Prog fs) = Prog <$> (getOverNodes <$> traverse (traverse (traverse f)) (OverNodes fs))

makeProg :: [(Var, Fun a)] -> Prog a
makeProg = Prog . flip mkGraph [] . zip [1..]

lookupFun :: Prog a -> Var -> Maybe (Fun a)
lookupFun = lookup . OverNodes . getProg
    where lookup t key = fmap snd $ find ((key ==) . fst) t

updateFun :: Prog a -> Var -> Fun a -> Prog a
updateFun p x f = case lookupFun p x of
    Just _ -> mapProg (\(y, g) -> (y, if x == y then f else g)) p
    Nothing -> p `mappend` makeProg [(x, f)]

mapFun :: ((Var, Fun a) -> b) -> Prog a -> [b]
mapFun f (Prog fs) = foldMap ((:[]) . f) $ OverNodes fs

mapProg :: ((Var, Fun a) -> (Var, Fun b)) -> Prog a -> Prog b
mapProg f (Prog fs) = Prog $ getOverNodes $ fmap f $ OverNodes fs

travFun :: Applicative f => ((Var, Fun a) -> f b) -> Prog a -> f [b]
travFun f (Prog fs) = toList <$> traverse f (OverNodes fs)

travProg :: Applicative f => ((Var, Fun a) -> f (Var, Fun b)) -> Prog a -> f (Prog b)
travProg f (Prog fs) = Prog <$> (getOverNodes <$> traverse f (OverNodes fs))

connects :: [(Var, Var)] -> Prog a -> Prog a
connects vs (Prog fs) = Prog $ insEdges (concatMap firstEdge vs) fs
    where firstEdge = take 1 . makeEdgesWhere fs () . (named *** named)
          named = (. fst) . (==)

scSubprograms :: Prog a -> [Prog a]
scSubprograms = map Prog . scSubgraphs . getProg
