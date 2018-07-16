{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Mapping (
    Mapping(..),
) where

import Prelude hiding (lookup)

class Mapping m k v | m -> k v where
    lookupBy :: (k -> Bool) -> m -> Maybe v
    updateBy :: (k -> Bool) -> v -> m -> m
    deleteBy :: (k -> Bool) -> m -> m      

    fromList :: [(k, v)] -> m
    keys :: m -> [k]
    values :: m -> [v]

    lookup :: Eq k => k -> m -> Maybe v
    lookup = lookupBy . (==)
    update :: Eq k => k -> v -> m -> m
    update = updateBy . (==)
    delete :: Eq k => k -> m -> m
    delete = deleteBy . (==)

instance Mapping [(k, v)] k v where
    lookupBy f = go
        where go [] = Nothing
              go ((k,v):_) | f k = Just v
              go (_:kvs) = go kvs
    updateBy f v = go
        where go [] = []
              go ((k,_):kvs) | f k = (k,v):kvs
              go (kv:kvs) = kv:go kvs
    deleteBy f = go
        where go [] = []
              go ((k,_):kvs) | f k = kvs
              go (kv:kvs) = kv:go kvs

    fromList = id
    keys = map fst
    values = map snd
