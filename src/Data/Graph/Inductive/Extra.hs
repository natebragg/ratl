{-# LANGUAGE GADTs #-}

module Data.Graph.Inductive.Extra (
    nodesWhere,
    makeEdgesWhere,
    scSubgraphs,
    OverNodes(..),
    OverEdges(..),
) where

import Control.Arrow (first, second)
import Data.Maybe (fromJust)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.DFS (scc)
import Data.Graph.Inductive.Graph (
    DynGraph,
    Context,
    Node,
    LEdge,
    empty,
    ufold,
    gmap,
    (&),
    newNodes,
    noNodes,
    nodes,
    labfilter,
    subgraph
    )

nodesWhere :: DynGraph gr => (a -> Bool) -> gr a b -> [Node]
nodesWhere = (nodes .) . labfilter

makeEdgesWhere :: DynGraph gr => gr a b -> b -> (a -> Bool, a -> Bool) -> [LEdge b]
makeEdgesWhere g b (f1, f2) = (,,) <$> nodesWhere f1 g <*> nodesWhere f2 g <*> pure b

scSubgraphs :: DynGraph gr => gr a b -> [gr a b]
scSubgraphs = map . (flip subgraph) <*> scc

instance Semigroup (Gr a b) where
    g1 <> g2 = ufold ((&) . inc) g1 g2
        where inc (p, v, l, s) = (renumberAdj p, renumber v, l, renumberAdj s)
              renums = zip (nodes g2) $ newNodes (noNodes g2) g1
              renumber = fromJust . flip lookup renums
              renumberAdj = map (second renumber)

instance Monoid (Gr a b) where
    mempty = empty

newtype ByNode b a =
    ByNode { getByNode :: Context a b }

instance Functor (ByNode b) where
    fmap f (ByNode (p, v, l, s)) = ByNode (p, v, f l, s)

instance Foldable (ByNode b) where
    foldMap f (ByNode (_, _, l, _)) = f l

instance Traversable (ByNode b) where
    traverse f (ByNode (p, v, l, s)) = ByNode <$> ((,,,) p v <$> f l <*> pure s)

data OverNodes gr b a where
    OverNodes :: DynGraph gr => { getOverNodes :: gr a b } -> OverNodes gr b a

instance Functor (OverNodes gr b) where
    fmap f (OverNodes g) = OverNodes $ gmap (getByNode . fmap f . ByNode) g

instance Foldable (OverNodes gr b) where
    foldMap f (OverNodes g) = ufold (mappend . foldMap f . ByNode) mempty g

instance Traversable (OverNodes gr b) where
    traverse f (OverNodes g) = OverNodes <$> utrav f g
        where utrav :: (DynGraph gr, Applicative f) => (a -> f c) -> gr a b -> f (gr c b)
              utrav f g = ufold (\c a -> (&) <$> (getByNode <$> traverse f (ByNode c)) <*> a) (pure empty) g

newtype ByEdge a b =
    ByEdge { getByEdge :: Context a b }

instance Functor (ByEdge a) where
    fmap f (ByEdge (p, v, l, s)) = ByEdge (fmap (first f) p, v, l, fmap (first f) s)

instance Foldable (ByEdge a) where
    foldMap f (ByEdge (p, _, _, s)) = foldMap (f . fst) p `mappend` foldMap (f . fst) s

instance Traversable (ByEdge a) where
    traverse f (ByEdge (p, v, l, s)) = ByEdge <$> ((,,,) <$> travAdj p <*> pure v <*> pure l <*> travAdj s)
        where travAdj = traverse (\(l,w) -> (,) <$> f l <*> pure w)

data OverEdges gr a b where
    OverEdges :: DynGraph gr => { getOverEdges :: gr a b } -> OverEdges gr a b

instance Functor (OverEdges gr a) where
    fmap f (OverEdges g) = OverEdges $ gmap (getByEdge . fmap f . ByEdge) g

instance Foldable (OverEdges gr a) where
    foldMap f (OverEdges g) = ufold (mappend . foldMap f . ByEdge) mempty g

instance Traversable (OverEdges gr a) where
    traverse f (OverEdges g) = OverEdges <$> utrav f g
        where utrav :: (DynGraph gr, Applicative f) => (b -> f c) -> gr a b -> f (gr a c)
              utrav f g = ufold (\c a -> (&) <$> (getByEdge <$> traverse f (ByEdge c)) <*> a) (pure empty) g
