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

-- * 'CachedRef'

-- |A value that is either stored on disk as a 'BlobRef' or in memory only.
data MaybeMem a
    = -- |A value stored on disk as a 'BlobRef'
      Disk !(BlobRef a)
    | -- |A value held directly in memory
      Mem !a

-- |A reference that is backed by a cache.
-- Before it is first written to disk, the value is stored in memory.
-- Once it is first written to disk, only the reference is directly maintained, but the value
-- is stored in the cache.
--
-- The use of an 'IORef' is to allow for the possibility of references being duplicated
-- between block states, which can happen if finalization is lagging the head of the chain.
-- The IORef is shared among all copies of the reference, which ensures that it is not unnecessarily
-- held in memory and is not written to disk in duplicate.
newtype CachedRef c a = CachedRef {crIORef :: IORef (MaybeMem a)}

instance
    ( MonadCache c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a
    ) =>
    Reference m (CachedRef c) a
    where
    refFlush cr@(CachedRef ioref) = do
        mbr <- liftIO $ readIORef ioref
        case mbr of
            Disk br -> return (cr, br)
            Mem val -> do
                (!r, !val') <- {-# SCC "FLUSHCACHE" #-} storeUpdateRef val
                liftIO $ writeIORef ioref (Disk r)
                _ <- putCachedValue (Proxy @c) r val'
                return (cr, r)
    refCache r@(CachedRef ioref) =
        liftIO (readIORef ioref) >>= \case
            Mem v -> return (v, r)
            Disk ref -> do
                val <- {-# SCC "LOADCACHE" #-} loadRef ref
                !val' <- putCachedValue (Proxy @c) ref val
                return (val', r)
    refLoad (CachedRef ioref) =
        liftIO (readIORef ioref) >>= \case
            Mem val -> return val
            Disk ref ->
                lookupCachedValue (Proxy @c) ref >>= \case
                    Nothing -> do
                        val <- {-# SCC "LOADCACHE" #-} loadRef ref
                        putCachedValue (Proxy @c) ref val
                    Just val -> return val
    refMake val = do
        ioref <- liftIO $ newIORef (Mem val)
        return (CachedRef ioref)
    refUncache cr@(CachedRef ioref) =
        liftIO (readIORef ioref) >>= \case
            Mem val -> do
                (r, _) <- storeUpdateRef val
                liftIO $ writeIORef ioref (Disk r)
                return cr
            Disk _ -> return cr
    {-# INLINE refFlush #-}
    {-# INLINE refCache #-}
    {-# INLINE refLoad #-}
    {-# INLINE refMake #-}
    {-# INLINE refUncache #-}

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
    load = do
        mref <- load
        return $ do
            ref <- mref
            ioref <- liftIO $ newIORef (Disk ref)
            return (CachedRef ioref)
    {-# INLINE store #-}
    {-# INLINE storeUpdate #-}
    {-# INLINE load #-}
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
    show _ = "<CachedRef>"

-- |We do nothing to cache a 'CachedRef'. Since 'cache' is generally used to cache the entire
-- global state, it is generally undesirable to load every 'CachedRef' into the cache, as this
-- can result in evictions and wasted effort if the chache size is insufficient.
instance (Applicative m) => Cacheable m (CachedRef c a) where
    cache = pure

-- * 'EagerlyHashedCachedRef'

-- |A 'CachedRef' with a hash that is eagerly computed.
data EagerlyHashedCachedRef' h c a = EagerlyHashedCachedRef
    { ehCachedRef :: !(CachedRef c a),
      ehHash :: !h
    }
    deriving (Show)

-- |A 'CachedRef' with a hash that is eagerly computed.
type EagerlyHashedCachedRef = EagerlyHashedCachedRef' H.Hash

instance HashableTo h (EagerlyHashedCachedRef' h c a) where
    getHash = ehHash
    {-# INLINE getHash #-}

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
        return (EagerlyHashedCachedRef{ehCachedRef = cr, ehHash = ehHash ref}, r)

    refCache ref = do
        (r, cr) <- refCache $ ehCachedRef ref
        return (r, EagerlyHashedCachedRef{ehCachedRef = cr, ehHash = ehHash ref})

    refLoad ref = refLoad $ ehCachedRef ref

    refMake val = do
        h <- getHashM val
        cref <- refMake val
        return $ EagerlyHashedCachedRef cref h

    refUncache ref = do
        cr <- refUncache (ehCachedRef ref)
        return EagerlyHashedCachedRef{ehCachedRef = cr, ehHash = ehHash ref}

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
        return (r, EagerlyHashedCachedRef v' (ehHash c))

    load = do
        mCachedRef <- load
        return $ do
            ehCachedRef <- mCachedRef
            ehHash <- getHashM ehCachedRef
            return EagerlyHashedCachedRef{..}

-- |See implementation for 'CachedRef'.
instance
    ( Applicative m
    ) =>
    Cacheable m (EagerlyHashedCachedRef' h c a)
    where
    cache = pure
