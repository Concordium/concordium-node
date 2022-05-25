{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |Cached references.
module Concordium.GlobalState.Persistent.CachedRef where

import Data.Proxy

import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.Cache
import Concordium.Types.HashableTo

data CachedRef c a
    = -- |Value stored on disk, and possibly cached
      CRRef {crRef :: !(BlobRef a)}
    | -- |Value stored only in memory
      CRMem {crMem :: !a}

instance
    ( MonadCache r c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a
    ) =>
    Reference m (CachedRef c) a
    where
    refFlush r@(CRRef ref) = return (r, ref)
    refFlush (CRMem val) = do
        (r, val') <- storeUpdateRef val
        _ <- putCachedValue (Proxy @c) r val'
        return (CRRef r, r)
    refCache r@(CRRef ref) =
        lookupCachedValue (Proxy @c) ref >>= \case
            Nothing -> do
                val <- loadRef ref
                val' <- putCachedValue (Proxy @c) ref val
                return (val', r)
            Just val ->
                return (val, r)
    refCache r@(CRMem val) = return (val, r)

    refLoad (CRRef ref) =
        lookupCachedValue (Proxy @c) ref >>= \case
            Nothing -> loadRef ref
            Just val -> return val
    refLoad (CRMem val) = return val
    refMake val = return (CRMem val)
    refUncache r@(CRRef _) = return r
    refUncache (CRMem val) = do
        (r, _) <- storeUpdateRef val
        return (CRRef r)

instance
    ( MonadCache r c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a
    ) =>
    BlobStorable m (CachedRef c a)
    where
    store c = store . snd =<< refFlush c
    storeUpdate c = do
        (c', ref) <- refFlush c
        (,c') <$> store ref
    load = fmap CRRef <$> load

instance
    ( MonadCache r c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a,
      MHashableTo m h a
    ) =>
    MHashableTo m h (CachedRef c a)
    where
    getHashM ref = getHashM =<< refLoad ref
