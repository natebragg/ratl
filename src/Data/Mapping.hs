{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Mapping (
    Mapping(..),
    mapAppend,
    mapConcat,
    (<?<),
) where

import Data.Maybe (listToMaybe)
import Data.Semigroup (Semigroup(..))
import Prelude hiding (lookup)

class Mapping m k v | m -> k v where
    lookupBy :: (k -> Bool) -> m -> Maybe v
    lookupBy = ((listToMaybe . values) .) . selectBy
    updateBy :: (k -> Bool) -> Maybe k -> v -> m -> m
    deleteBy :: (k -> Bool) -> m -> m
    deleteBy = selectBy . (not .)
    selectBy :: (k -> Bool) -> m -> m
    selectBy = deleteBy . (not .)

    fromList :: [(k, v)] -> m
    elements :: m -> [(k, v)]
    keys :: m -> [k]
    keys = map fst . elements
    values :: m -> [v]
    values = map snd . elements

    (<<<) :: (Eq v, Mapping m2 v v2, Mapping m3 k v2) => m2 -> m -> m3
    m2 <<< m = fromList [(k, v2) | (k, v) <- elements m, Just v2 <- [lookup v m2]]

    lookup :: Eq k => k -> m -> Maybe v
    lookup = lookupBy . (==)
    update :: Eq k => k -> v -> m -> m
    update k = updateBy (k ==) (Just k)
    delete :: Eq k => k -> m -> m
    delete = deleteBy . (==)
    select :: Eq k => k -> m -> m
    select = selectBy . (==)

mapAppend :: (Semigroup v, Eq k, Mapping m k v) => m -> m -> m
mapAppend as bs = foldl combine as $ elements bs
    where combine as (k, v) = update k (maybe v (<> v) (lookup k as)) as

mapConcat :: (Semigroup v, Eq k, Mapping m k v) => [m] -> m
mapConcat = foldr mapAppend $ fromList []

(<?<) :: (Eq k, Mapping m k v, Mapping m2 k k) => m -> m2 -> m
m <?< m2 = fromList [(maybe k id $ lookup k m2, v) | (k, v) <- elements m]

instance Mapping [(k, v)] k v where
    lookupBy f = go
        where go [] = Nothing
              go ((k,v):_) | f k = Just v
              go (_:kvs) = go kvs
    updateBy f k v = go k
        where go k [] = maybe [] (\k -> [(k, v)]) k
              go _ ((k,_):kvs) | f k = (k,v):go Nothing kvs
              go k (kv:kvs) = kv:go k kvs
    deleteBy f = go
        where go [] = []
              go ((k,_):kvs) | f k = go kvs
              go (kv:kvs) = kv:go kvs

    fromList = id
    elements = id
