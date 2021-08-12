{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Concordium.GlobalState.Persistent.Bakers where

import Control.Exception
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import Data.Serialize

import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Basic.BlockState.Bakers as Basic
import qualified Concordium.Types.Accounts as BaseAccounts
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types
import Concordium.Utils.Serialization

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.Types.HashableTo
import Concordium.Utils.Serialization.Put

-- |A list of 'BakerInfo's, ordered by increasing 'BakerId'.
newtype BakerInfos = BakerInfos (Vec.Vector (BufferedRef BaseAccounts.BakerInfo)) deriving (Show)

instance MonadBlobStore m => BlobStorable m BakerInfos where
    storeUpdate (BakerInfos v) = do
      v' <- mapM storeUpdate v
      let pv = do
              putLength (Vec.length v')
              mapM_ fst v'
      return (pv, BakerInfos (snd <$> v'))
    store bi = fst <$> storeUpdate bi
    load = do
      len <- getLength
      v <- Vec.replicateM len load
      return $ BakerInfos <$> sequence v

instance MonadBlobStore m => MHashableTo m H.Hash BakerInfos where
    getHashM (BakerInfos v) = do
      v' <- mapM loadBufferedRef v
      return $ H.hashLazy $ runPutLazy $ mapM_ put v'

instance (MonadBlobStore m) => Cacheable m BakerInfos where
    cache (BakerInfos v) = BakerInfos <$> mapM cache v

-- |A list of stakes for bakers.
newtype BakerStakes = BakerStakes (Vec.Vector Amount) deriving (Show)

instance HashableTo H.Hash BakerStakes where
    getHash (BakerStakes v) = H.hashLazy $ runPutLazy $ mapM_ put v
instance Monad m => MHashableTo m H.Hash BakerStakes
instance Serialize BakerStakes where
    put (BakerStakes v) = putLength (Vec.length v) >> mapM_ put v
    get = do
        len <- getLength
        BakerStakes <$> Vec.replicateM len get
instance MonadBlobStore m => BlobStorable m BakerStakes
instance (Applicative m) => Cacheable m BakerStakes

-- |The set of bakers that are eligible to bake in a particular epoch.
--
-- The hashing scheme separately hashes the baker info and baker stakes.
data PersistentEpochBakers = PersistentEpochBakers {
    _bakerInfos :: !(HashedBufferedRef BakerInfos),
    _bakerStakes :: !(HashedBufferedRef BakerStakes),
    _bakerTotalStake :: !Amount
} deriving (Show)

makeLenses ''PersistentEpochBakers

-- |Serialize 'PersistentEpochBakers' in V0 format.
putEpochBakersV0 :: (MonadBlobStore m, MonadPut m) => PersistentEpochBakers -> m ()
putEpochBakersV0 peb = do
        BakerInfos bi <- refLoad (peb ^. bakerInfos)
        bInfos <- mapM refLoad bi
        BakerStakes bStakes <- refLoad (peb ^. bakerStakes)
        assert (Vec.length bInfos == Vec.length bStakes) $
            liftPut $ putLength (Vec.length bInfos)
        mapM_ sPut bInfos
        mapM_ sPut bStakes

instance MonadBlobStore m => MHashableTo m H.Hash PersistentEpochBakers where
    getHashM PersistentEpochBakers{..} = do
      hbkrInfos <- getHashM _bakerInfos
      hbkrStakes <- getHashM _bakerStakes
      return $ H.hashOfHashes hbkrInfos hbkrStakes

instance MonadBlobStore m => BlobStorable m PersistentEpochBakers where
    storeUpdate PersistentEpochBakers{..} = do
        (pBkrInfos, newBkrInfos) <- storeUpdate _bakerInfos
        (pBkrStakes, newBkrStakes) <- storeUpdate _bakerStakes
        let pBkrs = do
                pBkrInfos
                pBkrStakes
                put _bakerTotalStake
        return (pBkrs, PersistentEpochBakers{_bakerInfos = newBkrInfos, _bakerStakes = newBkrStakes,..})
    store eb = fst <$> storeUpdate eb
    load = do
        mBkrInfos <- load
        mBkrStakes <- load
        _bakerTotalStake <- get
        return $ do
          _bakerInfos <- mBkrInfos
          _bakerStakes <- mBkrStakes
          return PersistentEpochBakers{..}

instance MonadBlobStore m => Cacheable m PersistentEpochBakers where
    cache peb = do
        cBkrInfos <- cache (_bakerInfos peb)
        cBkrStakes <- cache (_bakerStakes peb)
        return peb {_bakerInfos = cBkrInfos, _bakerStakes = cBkrStakes}

-- |Derive a 'FullBakers' from a 'PersistentEpochBakers'.
epochToFullBakers :: (MonadBlobStore m) => PersistentEpochBakers -> m FullBakers
epochToFullBakers PersistentEpochBakers{..} = do
    BakerInfos infoRefs <- refLoad _bakerInfos
    infos <- mapM refLoad infoRefs
    BakerStakes stakes <- refLoad _bakerStakes
    return FullBakers{
            fullBakerInfos = Vec.zipWith FullBakerInfo infos stakes,
            bakerTotalStake = _bakerTotalStake
        }

-- |Derive a 'PersistentEpochBakers' from a 'Basic.EpochBakers'.
makePersistentEpochBakers :: (MonadBlobStore m) => Basic.EpochBakers -> m PersistentEpochBakers
makePersistentEpochBakers ebs = do
    _bakerInfos <- refMake =<< BakerInfos <$> mapM refMake (Basic._bakerInfos ebs)
    _bakerStakes <- refMake $ BakerStakes (Basic._bakerStakes ebs)
    let _bakerTotalStake = Basic._bakerTotalStake ebs
    return PersistentEpochBakers{..}

data PersistentActiveBakers = PersistentActiveBakers {
    _activeBakers ::  !(Trie.TrieN (BufferedBlobbed BlobRef) BakerId ()),
    _aggregationKeys :: !(Trie.TrieN (BufferedBlobbed BlobRef) BakerAggregationVerifyKey ())
} deriving (Show)

makeLenses ''PersistentActiveBakers

instance MonadBlobStore m => BlobStorable m PersistentActiveBakers where
    storeUpdate PersistentActiveBakers{..} = do
        (pActiveBakers, newActiveBakers) <- storeUpdate _activeBakers
        (pAggregationKeys, newAggregationKeys) <- storeUpdate _aggregationKeys
        let pPAB = pActiveBakers >> pAggregationKeys
        let newPAB = PersistentActiveBakers{
          _activeBakers = newActiveBakers,
          _aggregationKeys = newAggregationKeys
        }
        return (pPAB, newPAB)
    store pab = fst <$> storeUpdate pab
    load = do
        mActiveBakers <- load
        mAggregationKeys <- load
        return $ do
            _activeBakers <- mActiveBakers
            _aggregationKeys <- mAggregationKeys
            return PersistentActiveBakers{..}

instance Applicative m => Cacheable m PersistentActiveBakers

makePersistentActiveBakers :: (MonadBlobStore m) => Basic.ActiveBakers -> m PersistentActiveBakers
makePersistentActiveBakers ab = do
    _activeBakers <- Trie.fromList $ (, ()) <$> Set.toList (Basic._activeBakers ab)
    _aggregationKeys <- Trie.fromList $ (, ()) <$> Set.toList (Basic._aggregationKeys ab)
    return PersistentActiveBakers{..}
