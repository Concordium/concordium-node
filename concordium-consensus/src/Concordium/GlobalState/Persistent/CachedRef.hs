{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |Cached references.
module Concordium.GlobalState.Persistent.CachedRef where

import Control.Monad.IO.Class
import Data.IORef
import Data.Proxy

import qualified Concordium.Crypto.SHA256 as H
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.Cache
import Concordium.Types.HashableTo

data CachedRef c a
    = -- |Value stored on disk, and possibly cached
      CRRef {crRef :: !(BlobRef a)}
    | -- |Value stored only in memory
      CRMem {crMem :: !a}

instance
    ( MonadCache c m,
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
            Nothing -> do
              val <- loadRef ref
              putCachedValue (Proxy @c) ref val
            Just val -> return val
    refLoad (CRMem val) = return val
    refMake val = return (CRMem val)
    refUncache r@(CRRef _) = return r
    refUncache (CRMem val) = do
        (r, _) <- storeUpdateRef val
        return (CRRef r)

instance
    ( MonadCache c m,
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
    ( MonadCache c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a,
      MHashableTo m h a
    ) =>
    MHashableTo m h (CachedRef c a)
    where
    getHashM ref = getHashM =<< refLoad ref

instance Show a => Show (CachedRef c a) where
    show (CRRef r) = show r
    show (CRMem v) = show v

instance (MonadCache c m, BlobStorable m a, Cache c, CacheKey c ~ BlobRef a, CacheValue c ~ a, Cacheable m a) => Cacheable m (CachedRef c a) where
    cache cr@CRRef{..} = do
        val <-
            lookupCachedValue (Proxy @c) crRef >>= \case
                Nothing -> loadRef crRef
                Just val -> return val
        _ <- putCachedValue (Proxy @c) crRef =<< cache val
        return cr
    cache CRMem{..} = CRMem <$> cache crMem

-- |A 'CachedRef', possibly accompanied by a hash.
data HashedCachedRef' h c a = HashedCachedRef
    { cachedRef :: !(CachedRef c a),
      cachedRefHash :: !(IORef (Maybe h))
    }

type HashedCachedRef = HashedCachedRef' H.Hash

-- |'refFlush', 'refCache', 'refLoad' and 'refUncache' do not touch the hash.
-- 'refMake' will compute the hash optimistically.
-- TODO: Determine if this is actually what we want. It may be better to postpone the hashing,
-- since the hash may not actually be used.
instance
    ( MonadCache c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a,
      MHashableTo m h a
    ) =>
    Reference m (HashedCachedRef' h c) a
    where
    refFlush ref = do
        (cr, r) <- refFlush $ cachedRef ref
        return (HashedCachedRef cr (cachedRefHash ref), r)

    refCache ref = do
        (r, cr) <- refCache $ cachedRef ref
        return (r, HashedCachedRef cr $ cachedRefHash ref)

    refLoad ref = refLoad $ cachedRef ref

    refMake val = do
        h <- getHashM val
        href <- liftIO $ newIORef (Just h)
        return $ HashedCachedRef (CRMem val) href

    refUncache ref = do
        cr <- refUncache (cachedRef ref)
        return $ HashedCachedRef cr (cachedRefHash ref)

instance
    ( MonadCache c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a
    ) =>
    BlobStorable m (HashedCachedRef' h c a)
    where
    store c = store . snd =<< refFlush (cachedRef c)

    storeUpdate c = do
        (r, v') <- storeUpdate (cachedRef c)
        return (r, HashedCachedRef v' (cachedRefHash c))

    load = do
        mCachedRef <- load
        return $ do
            cachedRef <- mCachedRef
            cachedRefHash <- liftIO $ newIORef Nothing
            return HashedCachedRef{..}

instance
    ( MonadCache c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a,
      MHashableTo m h a,
      MonadIO m
    ) =>
    MHashableTo m h (HashedCachedRef' h c a)
    where
    getHashM ref =
        liftIO (readIORef (cachedRefHash ref)) >>= \case
            Nothing -> do
                !hsh <- getHashM =<< refLoad ref
                liftIO $ writeIORef (cachedRefHash ref) (Just hsh)
                return hsh
            Just hsh -> return hsh

instance Show a => Show (HashedCachedRef c a) where
    show ref = show (cachedRef ref)

instance
    ( MonadCache c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a,
      Cacheable m a
    ) =>
    Cacheable m (HashedCachedRef' h c a)
    where
    cache HashedCachedRef{..} = do
        ref' <- cache cachedRef
        return HashedCachedRef{cachedRef = ref', ..}

-- |A 'CachedRef' with a hash that is eagerly computed.
data EagerlyHashedCachedRef' h c a = EagerlyHashedCachedRef
    { ehCachedRef :: !(CachedRef c a),
      ehHash :: !h
    }
    deriving Show

type EagerlyHashedCachedRef = EagerlyHashedCachedRef' H.Hash

instance HashableTo h (EagerlyHashedCachedRef' h c a) where
    getHash = ehHash

instance (Monad m) => MHashableTo m h (EagerlyHashedCachedRef' h c a)

instance
    ( MonadCache c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a,
      MHashableTo m h a
    ) =>
    Reference m (EagerlyHashedCachedRef' h c) a
    where
    refFlush ref = do
        (cr, r) <- refFlush $ ehCachedRef ref
        return (ref{ehCachedRef = cr}, r)

    refCache ref = do
        (r, cr) <- refCache $ ehCachedRef ref
        return (r, ref{ehCachedRef = cr})

    refLoad ref = refLoad $ ehCachedRef ref

    refMake val = do
        h <- getHashM val
        return $ EagerlyHashedCachedRef (CRMem val) h

    refUncache ref = do
        cr <- refUncache (ehCachedRef ref)
        return $! ref{ehCachedRef = cr}

instance
    ( MonadCache c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a,
      MHashableTo m h a
    ) =>
    BlobStorable m (EagerlyHashedCachedRef' h c a)
    where
    store c = store . snd =<< refFlush (ehCachedRef c)

    storeUpdate c = do
        (r, v') <- storeUpdate (ehCachedRef c)
        return (r, c{ehCachedRef = v'})

    load = do
        mCachedRef <- load
        return $ do
            ehCachedRef <- mCachedRef
            ehHash <- getHashM ehCachedRef
            return EagerlyHashedCachedRef{..}

instance
    ( MonadCache c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a,
      Cacheable m a
    ) =>
    Cacheable m (EagerlyHashedCachedRef' h c a)
    where
    cache EagerlyHashedCachedRef{..} = do
        ref' <- cache ehCachedRef
        return EagerlyHashedCachedRef{ehCachedRef = ref', ..}
