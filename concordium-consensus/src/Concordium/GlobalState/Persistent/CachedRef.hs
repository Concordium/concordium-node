{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |Cached references.
module Concordium.GlobalState.Persistent.CachedRef where

import Control.Monad
import Control.Monad.Trans
import Data.IORef
import Data.Proxy

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types.HashableTo

import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.Cache


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
                (!r, !val') <- storeUpdateRef val
                liftIO $ writeIORef ioref $! Disk r
                _ <- putCachedValue (Proxy @c) r val'
                return (cr, r)
    refCache r@(CachedRef ioref) =
        liftIO (readIORef ioref) >>= \case
            Mem v -> return (v, r)
            Disk ref -> do
                val <- loadRef ref
                !val' <- putCachedValue (Proxy @c) ref val
                return (val', r)
    refLoad (CachedRef ioref) =
        liftIO (readIORef ioref) >>= \case
            Mem val -> return val
            Disk ref ->
                lookupCachedValue (Proxy @c) ref >>= \case
                    Nothing -> do
                        val <- loadRef ref
                        putCachedValue (Proxy @c) ref val
                    Just val -> return val
    refMake val = do
        ioref <- liftIO $ newIORef $! Mem val
        return (CachedRef ioref)
    refUncache cr@(CachedRef ioref) =
        liftIO (readIORef ioref) >>= \case
            Mem val -> do
                (r, _) <- storeUpdateRef val
                liftIO $ writeIORef ioref $! Disk r
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
            ioref <- liftIO $ newIORef $! Disk ref
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
-- can result in evictions and wasted effort if the cache size is insufficient.
instance (Applicative m) => Cacheable m (CachedRef c a) where
    cache = pure

-- * 'LazilyHashedCachedRef'

-- |A 'CachedRef' with a hash that is computed when first demanded (via 'getHashM'), or when the
-- reference is cached (via 'refCache' or 'cache').
data LazilyHashedCachedRef' h c a = LazilyHashedCachedRef
    { lhCachedRef :: !(CachedRef c a),
      lhHash :: !(IORef (Nullable h))
    }

type LazilyHashedCachedRef = LazilyHashedCachedRef' H.Hash

instance Show (LazilyHashedCachedRef' h c a) where
    show _ = "<LazilyHashedCachedRef>"

instance
    ( MonadCache c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a,
      MHashableTo m h a
    ) =>
    MHashableTo m h (LazilyHashedCachedRef' h c a)
    where
    getHashM LazilyHashedCachedRef{..} =
        liftIO (readIORef lhHash) >>= \case
            Some h -> return h
            Null -> do
                h <- getHashM lhCachedRef
                liftIO $ writeIORef lhHash $! Some h
                return h

instance
    ( MonadCache c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a,
      MHashableTo m h a
    ) =>
    Reference m (LazilyHashedCachedRef' h c) a
    where
    refFlush ref = do
        (cr, r) <- refFlush $ lhCachedRef ref
        return (LazilyHashedCachedRef{lhCachedRef = cr, lhHash = lhHash ref}, r)

    refCache ref = do
        (r, cr) <- refCache $ lhCachedRef ref
        liftIO (readIORef (lhHash ref)) >>= \case
           Null -> do
                h <- getHashM r
                liftIO (writeIORef (lhHash ref) $! Some h)
           Some _ -> return ()
        return (r, LazilyHashedCachedRef{lhCachedRef = cr, lhHash = lhHash ref})

    refLoad ref = refLoad $ lhCachedRef ref

    refMake val = do
        h <- liftIO $ newIORef Null
        cref <- refMake val
        return $ LazilyHashedCachedRef cref h

    refUncache ref = do
        cr <- refUncache (lhCachedRef ref)
        return LazilyHashedCachedRef{lhCachedRef = cr, lhHash = lhHash ref}

-- |Construct a 'LazilyHashedCachedRef'' given the value and hash.
makeLazilyHashedCachedRef :: (MonadIO m) => a -> h -> m (LazilyHashedCachedRef' h c a)
makeLazilyHashedCachedRef val hsh = liftIO $ do
    lhCachedRef <- CachedRef <$> (newIORef $! Mem val)
    lhHash <- newIORef $! Some hsh
    return LazilyHashedCachedRef{..}

instance
    ( MonadCache c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a
    ) =>
    BlobStorable m (LazilyHashedCachedRef' h c a)
    where
    store c = store . snd =<< refFlush (lhCachedRef c)

    storeUpdate c = do
        (r, v') <- storeUpdate (lhCachedRef c)
        return (r, LazilyHashedCachedRef v' (lhHash c))

    load = do
        mCachedRef <- load
        return $ do
            lhCachedRef <- mCachedRef
            lhHash <- liftIO $ newIORef Null
            return LazilyHashedCachedRef{..}

instance
    ( MonadCache c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a,
      MHashableTo m h a
    ) =>
    Cacheable m (LazilyHashedCachedRef' h c a)
    where
    cache r@LazilyHashedCachedRef{..} = do
        mhsh <- liftIO (readIORef lhHash)
        when (isNull mhsh) $ do
            val <- refLoad lhCachedRef
            hsh <- getHashM val
            liftIO $ writeIORef lhHash $! Some hsh
        return r

-- * 'EagerlyHashedCachedRef'

-- |A 'CachedRef' with a hash that is always computed. In particular, this means that 'load'ing
-- the reference will also load the referenced data (consequently caching it) in order to
-- compute the hash.
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

-- |Construct an 'EagerlyHashedCachedRef'' given the value and hash.
makeEagerlyHashedCachedRef :: (MonadIO m) => a -> h -> m (EagerlyHashedCachedRef' h c a)
makeEagerlyHashedCachedRef val ehHash = do
    ehCachedRef <- liftIO $ CachedRef <$> (newIORef $! Mem val)
    return EagerlyHashedCachedRef{..}

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

-- * 'HashedCachedRef'

data MaybeHashedCachedRef h c a = HCRMem !a | HCRMemHashed !a !h | HCRDisk !(HashedCachedRef' h c a)

-- |A 'CachedRef' with a hash that is computed when first demanded (via 'getHashM'), or when the
-- reference is cached (via 'refCache' or 'cache').
data HashedCachedRef' h c a
    = HCRUnflushed { hcrUnflushed :: !(IORef (MaybeHashedCachedRef h c a))}
    | HCRFlushed {
        hcrBlob :: !(BlobRef a),
        hcrHash :: !h
    }

-- |Migrate a 'HashedCachedRef'. This
migrateHashedCachedRef' ::
    forall h c c' a b t m.
    ( Cache c
    , Cache c'
    , MonadCache c m
    , MonadCache c' (t m)
    , BlobStorable m a
    , BlobStorable (t m) b
    , MonadTrans t
    , MHashableTo m h a
    , CacheValue c ~ a
    , CacheKey c ~ BlobRef a
    , MHashableTo (t m) h b
    , CacheValue c' ~ b
    , CacheKey c' ~ BlobRef b
    ) =>
    (a -> t m b) ->
    HashedCachedRef' h c a ->
    t m (HashedCachedRef' h c' b)
migrateHashedCachedRef' f hcr = do
    !v <- f =<< lift (refLoad hcr)
    -- compute the hash now that the value is available
    (!newRef, _) <- refFlush =<< makeHashedCachedRef v =<< getHashM v
    !_ <- lift (refUncache hcr)
    return newRef

type HashedCachedRef = HashedCachedRef' H.Hash

instance Show (HashedCachedRef' h c a) where
    show _ = "<HashedCachedRef>"

instance
    ( MonadCache c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a,
      MHashableTo m h a
    ) =>
    MHashableTo m h (HashedCachedRef' h c a)
    where
    getHashM HCRUnflushed{..} = liftIO (readIORef hcrUnflushed) >>= \case
        HCRMem a -> getHashM a
        HCRMemHashed _ h -> return h
        HCRDisk d -> getHashM d
    getHashM HCRFlushed{..} = return hcrHash

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
    refFlush HCRUnflushed{..} = liftIO (readIORef hcrUnflushed) >>= \case
        HCRMem val -> do
            (!hcrBlob, !val') <- storeUpdateRef val
            _ <- putCachedValue (Proxy @c) hcrBlob val'
            hcrHash <- getHashM val'
            let !newHCR = HCRFlushed{..}
            liftIO $ writeIORef hcrUnflushed $! HCRDisk newHCR
            return (newHCR, hcrBlob)
        HCRMemHashed val hcrHash -> do
            (!hcrBlob, !val') <- storeUpdateRef val
            _ <- putCachedValue (Proxy @c) hcrBlob val'
            let !newHCR = HCRFlushed{..}
            liftIO $ writeIORef hcrUnflushed $! HCRDisk newHCR
            return (newHCR, hcrBlob)
        HCRDisk newHCR -> refFlush newHCR
    refFlush hcr@HCRFlushed{..} = return (hcr, hcrBlob)

    refCache hcr@HCRUnflushed{..} = liftIO (readIORef hcrUnflushed) >>= \case
        HCRMem val -> return (val, hcr)
        HCRMemHashed val _ -> return (val, hcr)
        HCRDisk d -> refCache d
    refCache hcr@HCRFlushed{..} = do
        val <- loadRef hcrBlob
        !val' <- putCachedValue (Proxy @c) hcrBlob val
        return (val', hcr)
    
    refMake val = liftIO $ do
        hcrUnflushed <- newIORef $! HCRMem val
        return $! HCRUnflushed{..}
    
    refLoad HCRUnflushed{..} = liftIO (readIORef hcrUnflushed) >>= \case
        HCRMem val -> return val
        HCRMemHashed val _ -> return val
        HCRDisk r -> refLoad r
    refLoad HCRFlushed{..} = lookupCachedValue (Proxy @c) hcrBlob >>= \case
        Nothing -> do
            val <- loadRef hcrBlob
            putCachedValue (Proxy @c) hcrBlob val
        Just val -> return val

    refUncache = fmap fst . refFlush
    {-# INLINE refFlush #-}
    {-# INLINE refCache #-}
    {-# INLINE refLoad #-}
    {-# INLINE refMake #-}
    {-# INLINE refUncache #-}


-- |Construct a 'HashedCachedRef'' given the value and hash.
makeHashedCachedRef :: (MonadIO m) => a -> h -> m (HashedCachedRef' h c a)
makeHashedCachedRef val hsh = liftIO $
    HCRUnflushed <$!> (newIORef $! HCRMemHashed val hsh)

instance
    ( MonadCache c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a,
      MHashableTo m h a
    ) =>
    BlobStorable m (HashedCachedRef' h c a)
    where
    store hcr = store . snd =<< refFlush hcr

    storeUpdate hcr = do
        (!hcr', !ref) <- refFlush hcr
        (, hcr') <$> store ref

    load = do
        mref <- load
        return $ do
            hcrBlob <- mref
            val <- lookupCachedValue (Proxy @c) hcrBlob >>= \case
                Nothing -> do
                    val <- loadRef hcrBlob
                    putCachedValue (Proxy @c) hcrBlob val
                Just val -> return val
            hcrHash <- getHashM val
            return HCRFlushed{..}

-- |Caching a 'HashedCachedRef' does nothing on the principle that it is generally undesirable to
-- load every 'HashedCachedRef' into the cache at load time.
instance (Applicative m) => Cacheable m (HashedCachedRef c a) where
    cache = pure

instance 
    ( MonadCache c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a
    ) => Cacheable1 m (HashedCachedRef' h c a) a where
    liftCache csh hcr@HCRUnflushed{..} = liftIO (readIORef hcrUnflushed) >>= \case
        HCRMem val -> do
            val' <- csh val
            liftIO . writeIORef hcrUnflushed . HCRMem $! val'
            return hcr
        HCRMemHashed val hsh -> do
            val' <- csh val
            liftIO . writeIORef hcrUnflushed $! HCRMemHashed val' hsh
            return hcr
        HCRDisk d -> liftCache csh d
    liftCache csh hcr@HCRFlushed{..} = do
        _ <- lookupCachedValue (Proxy @c) hcrBlob >>= \case
            Nothing -> putCachedValue (Proxy @c) hcrBlob =<< csh =<< loadRef hcrBlob
            Just val -> putCachedValue (Proxy @c) hcrBlob =<< csh val
        return hcr
