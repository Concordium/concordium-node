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
import qualified Concordium.Crypto.SHA256 as H

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

instance Show a => Show (CachedRef c a) where
  show ref = show (cachedRef ref) ++ maybe "" (\x -> " with hash: " ++ show x) (cachedRefHash ref)    

-- |A `CachedRef` accompanied with a `Maybe Hash`.
data HashedCachedRef' h c a =
  HashedCachedRef
  {
    cachedRef :: !(CachedRef c a),
    cachedRefHash :: !(Maybe h)
  }

type HashedCachedRef = HashedCachedRef' H.Hash

instance
    ( MonadCache r c m,
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
      return $ HashedCachedRef (CRMem val) $ Just h
      
    refUncache ref = do
      cr <- refUncache (cachedRef ref)
      return $ HashedCachedRef cr (cachedRefHash ref)

instance
    ( MonadCache r c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a
    ) =>
    BlobStorable m (HashedCachedRef c a)
    where
    store c = store . snd =<< refFlush (cachedRef c)
    
    storeUpdate c = do
      (r, v') <- storeUpdate (cachedRef c)
      return (r, HashedCachedRef v' (cachedRefHash c))
      
    load = load

instance
    ( MonadCache r c m,
      BlobStorable m a,
      Cache c,
      CacheKey c ~ BlobRef a,
      CacheValue c ~ a,
      MHashableTo m h a
    ) =>
    MHashableTo m h (HashedCachedRef' h c a)
    where
    getHashM ref = maybe (getHashM =<< refLoad ref) return (cachedRefHash ref)

instance Show a => Show (HashedCachedRef c a) where
  show ref = show (cachedRef ref) ++ maybe "" (\x -> " with hash: " ++ show x) (cachedRefHash ref)
