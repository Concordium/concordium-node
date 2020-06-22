{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Concordium.GlobalState.Persistent.Bakers where

import Control.Monad.IO.Class
import Data.Serialize
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform

import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Basic.BlockState.Bakers as Basic
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.Types
import Concordium.Utils

type Trie = Trie.TrieN (BufferedBlobbed BlobRef)

-- |Representation of the set of bakers on the chain.
data PersistentBakers = PersistentBakers {
    -- |Baker information, indexed by 'BakerId'
    _bakerInfoMap :: !(Trie BakerId BakerInfo),
    -- |Baker stake, indexed by 'BakerId'
    _bakerStakeMap :: !(Trie BakerId Amount),
    -- |The baker ids indexed by the keys
    _bakersByKey :: !(Map.Map BakerSignVerifyKey BakerId),
    -- |The total stake delegated to all bakers
    _bakerTotalStake :: !Amount,
    -- |The next 'BakerId' to use for a new baker.
    -- 'BakerId's should not be reused when bakers are removed.
    _nextBakerId :: !BakerId,
    -- |Aggregation keys in use
    _aggregationKeys :: !(Set.Set BakerAggregationVerifyKey)
} deriving (Show)

makeLenses ''PersistentBakers

instance (MonadBlobStore m BlobRef, MonadIO m) => BlobStorable m BlobRef PersistentBakers where
    storeUpdate p PersistentBakers{..} = do
        (pInfoMap, bInfoMap) <- storeUpdate p _bakerInfoMap
        (pStakeMap, bStakeMap) <- storeUpdate p _bakerStakeMap
        let newBakers = PersistentBakers {
                _bakerInfoMap = bInfoMap,
                _bakerStakeMap = bStakeMap,
                ..
            }
        let putBs = do
                    pInfoMap
                    pStakeMap
                    put _nextBakerId
        return (putBs, newBakers)
    store p a = fst <$> storeUpdate p a
    load p = do
        mInfoMap <- load p
        mStakeMap <- load p
        mNextBakerId <- load p
        return $ do
            _bakerInfoMap <- mInfoMap
            _bakerStakeMap <- mStakeMap
            _nextBakerId <- mNextBakerId
            -- TODO (MRA) instead of using Trie.toMap below it might be more efficient to compute _bakersByKey, _aggregationKeys, and
            -- _bakerTotalStake using the Trie API
            let deriv bid BakerInfo{..} (m, aks) = (m & at' _bakerSignatureVerifyKey ?~ bid,
                                                    Set.insert _bakerAggregationVerifyKey aks)
            (_bakersByKey, _aggregationKeys) <- Map.foldrWithKey deriv (Map.empty, Set.empty) <$> Trie.toMap _bakerInfoMap
            _bakerTotalStake <- sum <$> Trie.toMap _bakerStakeMap
            return PersistentBakers {..}

-- |Convert a (non-persistent) 'Transient.Accounts' to a (persistent) 'Accounts'.
-- The new object is not yet stored on disk.
makePersistentBakers :: MonadIO m => Basic.Bakers -> m PersistentBakers
makePersistentBakers Basic.Bakers{..} = do
    _bakerInfoMap <- Trie.fromList $ Map.toList $ _bakerInfo <$> _bakerMap
    _bakerStakeMap <- Trie.fromList $ Map.toList $ _bakerStake <$> _bakerMap
    return PersistentBakers{..}

createBaker :: (MonadBlobStore m BlobRef, MonadIO m) => BakerInfo -> PersistentBakers -> m (Either BakerError (BakerId, PersistentBakers))
createBaker (BakerInfo _bakerElectionVerifyKey _bakerSignatureVerifyKey _bakerAggregationVerifyKey _bakerAccount) bkrs = do
    let bInfoMap = bkrs ^. bakerInfoMap
    case bkrs ^. bakersByKey . at' _bakerSignatureVerifyKey of
        Nothing -> -- key does not yet exist
            if Set.member _bakerAggregationVerifyKey (bkrs ^. aggregationKeys) then
              return $ Left DuplicateAggregationKey
            else do -- aggregation keys is not already in use, so we insert baker
              newBakerInfoMap <- Trie.insert bid BakerInfo{..} bInfoMap
              newBakerStakeMap <- Trie.insert bid _bakerStake (bkrs ^. bakerStakeMap)
              return $ Right (bid, bkrs
                         & bakerInfoMap .~ newBakerInfoMap
                         & bakerStakeMap .~ newBakerStakeMap
                         & bakersByKey . at' _bakerSignatureVerifyKey ?~ bid
                         & nextBakerId .~ bid + 1
                         & aggregationKeys %~ Set.insert _bakerAggregationVerifyKey)
                where
                  _bakerStake = 0
                  bid = _nextBakerId bkrs
        Just _ -> return $ Left DuplicateSignKey

-- |Update a given baker. If this would lead to duplicate baker signing keys
-- return 'Nothing'. If the baker with the given id does not exist this function
-- returns the original 'Bakers' object.
updateBaker :: (MonadBlobStore m BlobRef, MonadIO m) => Basic.BakerUpdate -> PersistentBakers -> m (Maybe PersistentBakers)
updateBaker !Basic.BakerUpdate{..} !bakers = do
  let bInfoMap = bakers ^. bakerInfoMap
  Trie.lookup _buId bInfoMap >>= \case
    Nothing -> return $ Just bakers
    Just binfo -> do
      let bacc = _buAccount ^. non (binfo ^. bakerAccount)
      case _buSignKey of
           Nothing -> do -- don't update the sign key, no clash possible
             newBakerInfoMap <- Trie.insert _buId (binfo & bakerAccount .~ bacc) bInfoMap
             return $ Just $ bakers & bakerInfoMap .~ newBakerInfoMap
           Just newSignKey ->
             -- new signing key, make sure it is new
             case bakers ^. bakersByKey . at' newSignKey of
               Just _ -> -- existing signing key
                 return Nothing
               Nothing -> do -- fresh signing key
                 newBakerInfoMap <- Trie.insert _buId (binfo & bakerSignatureVerifyKey .~ newSignKey) bInfoMap
                 return $ Just (bakers
                       & bakerInfoMap .~ newBakerInfoMap
                       & bakersByKey . at' (binfo ^. bakerSignatureVerifyKey) .~ Nothing -- remove old identification
                       & bakersByKey . at' newSignKey .~ Just _buId) -- and add new identification

removeBaker :: (MonadBlobStore m BlobRef, MonadIO m) => BakerId -> PersistentBakers -> m (Bool, PersistentBakers)
removeBaker bid !bakers = do
    let bInfoMap = bakers ^. bakerInfoMap
        bStakeMap = bakers ^. bakerStakeMap
    mBakerInfo <- Trie.lookup bid bInfoMap
    mBakerStake <- Trie.lookup bid bStakeMap
    case (mBakerInfo, mBakerStake) of
        (Nothing, Nothing) -> return (False, bakers)
        (Just bkr, Just stake) -> do
            newBakerInfoMap <- Trie.delete bid bInfoMap
            newBakerStakeMap <- Trie.delete bid bStakeMap
            return (True, bakers
                            & (bakerInfoMap .~ newBakerInfoMap)
                            & bakersByKey . at' (bkr ^. bakerSignatureVerifyKey) .~ Nothing -- remove the baker by key as well.
                            & (bakerStakeMap .~ newBakerStakeMap)
                            & (bakerTotalStake %~ subtract stake))
        _ -> error $ "bakerInfoMap and bakerStakeMap must have the same domain, but they differ for key " ++ show bid

modifyStake :: (MonadBlobStore m BlobRef, MonadIO m) => Maybe BakerId -> AmountDelta -> PersistentBakers -> m PersistentBakers
modifyStake (Just bid) delta bakers = do
    let bStakeMap = bakers ^. bakerStakeMap
    Trie.lookup bid bStakeMap >>= \case
        Nothing -> return bakers
        Just stake -> do
            newStakeMap <- Trie.insert bid (applyAmountDelta delta stake) bStakeMap
            return $ bakers & (bakerStakeMap .~ newStakeMap)
                            & (bakerTotalStake %~ applyAmountDelta delta)
modifyStake _ _ bakers = return bakers

addStake :: (MonadBlobStore m BlobRef, MonadIO m) => Maybe BakerId -> Amount -> PersistentBakers -> m PersistentBakers
addStake bid amt = modifyStake bid (amountToDelta amt)

removeStake :: (MonadBlobStore m BlobRef, MonadIO m) => Maybe BakerId -> Amount -> PersistentBakers -> m PersistentBakers
removeStake bid amt = modifyStake bid (- amountToDelta amt)