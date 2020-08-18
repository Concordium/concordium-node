{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
module Concordium.GlobalState.Persistent.Bakers where

import Control.Monad (foldM, forM)
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
    _bakerMap :: !(Trie BakerId (BufferedRef BakerInfo, Amount)),
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
        (pInfoMap, bInfoMap) <- storeUpdate p _bakerMap
        let newBakers = PersistentBakers {
                _bakerMap = bInfoMap,
                ..
            }
        let putBs = do
                    pInfoMap
                    put _nextBakerId
        return (putBs, newBakers)
    store p a = fst <$> storeUpdate p a
    load p = do
        mBakerMap <- load p
        mNextBakerId <- load p
        return $ do
            _bakerMap <- mBakerMap
            _nextBakerId <- mNextBakerId
            bakerIds <- Trie.keys _bakerMap
            -- Since we only store the baker-ID-to-baker-info map and next baker ID in the persistent storage,
            -- to create a 'PersistentBakers' result we need to reconstruct the baker-signature-key-to-ID map, the total baker stake,
            -- and the set of aggregation keys. For that we fold over the baker IDs, collecting the above information
            -- using this 'collectBakerInfo' function.
            -- The function
            --   - looks up a baker's 'BakerInfo' and stake in the '_bakerMap' trie
            --   - extracts the signature key from the 'BakerInfo' to construct the first map
            --   - extracts the aggregation key to compute the set of aggregation keys
            --   - uses the stake to compute the total baker stake
            let collectBakerInfo :: (MonadIO m, MonadBlobStore m BlobRef)
                                 => (Map.Map BakerSignVerifyKey BakerId, Amount, Set.Set BakerAggregationVerifyKey) -- an accumulator for the result computed so far
                                 -> BakerId -- the baker ID to gather information for in this iteration
                                 -> m (Map.Map BakerSignVerifyKey BakerId, Amount, Set.Set BakerAggregationVerifyKey)
                collectBakerInfo (m, t, aks) bid =
                  Trie.lookup bid _bakerMap >>= \case
                    Just (bInfoRef, stake) -> do
                      BakerInfo {..} <- loadBufferedRef bInfoRef
                      return (m & at' _bakerSignatureVerifyKey ?~ bid,
                              t + stake,
                              Set.insert _bakerAggregationVerifyKey aks)
                    Nothing ->
                      -- This should never happen because above, we collected all baker IDs contained in the '_bakerMap' trie
                      -- so this key lookup ought to be successful
                      error $ "Baker ID " ++ show bid ++ " should be contained in baker map"
            (_bakersByKey, _bakerTotalStake, _aggregationKeys) <- foldM collectBakerInfo (Map.empty, 0, Set.empty) bakerIds
            return PersistentBakers {..}

-- |Convert an in-memory 'Basic.Bakers' value to a persistent 'PersistentBakers' value.
-- The new object is not yet stored on disk.
makePersistentBakers :: MonadIO m => Basic.Bakers -> m PersistentBakers
makePersistentBakers Basic.Bakers{..} = do
    persistentMap <- forM _bakerMap $ \FullBakerInfo{..} -> do
      bakerRef <- makeBufferedRef _bakerInfo
      return (bakerRef, _bakerStake)
    _bakerMap <- Trie.fromList $ Map.toList persistentMap
    return PersistentBakers{..}

createBaker :: (MonadBlobStore m BlobRef, MonadIO m) => BakerInfo -> PersistentBakers -> m (Either BakerError (BakerId, PersistentBakers))
createBaker (BakerInfo _bakerElectionVerifyKey _bakerSignatureVerifyKey _bakerAggregationVerifyKey _bakerAccount) bkrs = do
    let bInfoMap = bkrs ^. bakerMap
    case bkrs ^. bakersByKey . at' _bakerSignatureVerifyKey of
        Nothing -> -- key does not yet exist
            if Set.member _bakerAggregationVerifyKey (bkrs ^. aggregationKeys) then
              return $ Left DuplicateAggregationKey
            else do -- aggregation keys is not already in use, so we insert baker
              bInfoRef <- makeBufferedRef BakerInfo{..}
              newBakerMap <- Trie.insert bid (bInfoRef, _bakerStake) bInfoMap
              return $ Right (bid, bkrs
                         & bakerMap .~ newBakerMap
                         & bakersByKey . at' _bakerSignatureVerifyKey ?~ bid
                         & nextBakerId .~ bid + 1
                         & aggregationKeys %~ Set.insert _bakerAggregationVerifyKey)
                where
                  _bakerStake = 0
                  bid = _nextBakerId bkrs
        Just _ -> return $ Left DuplicateSignKey

-- |Update a given baker. If this would lead to duplicate baker signing keys
-- return 'Nothing'. If the baker with the given id does not exist this function
-- returns the original 'PersistentBakers' object.
updateBaker :: (MonadBlobStore m BlobRef, MonadIO m) => Basic.BakerUpdate -> PersistentBakers -> m (Maybe PersistentBakers)
updateBaker !Basic.BakerUpdate{..} !bakers = do
  let bInfoMap = bakers ^. bakerMap
  Trie.lookup _buId bInfoMap >>= \case
    Nothing -> return $ Just bakers
    Just (binfoRef, stake) -> do
      binfo <- loadBufferedRef binfoRef
      let bacc = _buAccount ^. non (binfo ^. bakerAccount)
      case _buSignKey of
           Nothing -> do -- don't update the sign key, no clash possible
             newBakerInfoRef <- makeBufferedRef (binfo & bakerAccount .~ bacc)
             newBakerMap <- Trie.insert _buId (newBakerInfoRef, stake) bInfoMap
             return $ Just $ bakers & bakerMap .~ newBakerMap
           Just newSignKey ->
             -- new signing key, make sure it is new
             case bakers ^. bakersByKey . at' newSignKey of
               Just _ -> -- existing signing key
                 return Nothing
               Nothing -> do -- fresh signing key
                 newBakerInfoRef <- makeBufferedRef (binfo & bakerSignatureVerifyKey .~ newSignKey)
                 newBakerMap <- Trie.insert _buId (newBakerInfoRef, stake) bInfoMap
                 return $ Just (bakers
                       & bakerMap .~ newBakerMap
                       & bakersByKey . at' (binfo ^. bakerSignatureVerifyKey) .~ Nothing -- remove old identification
                       & bakersByKey . at' newSignKey ?~ _buId) -- and add new identification

-- Removes a baker from the 'PersistentBakers' data type and returns the resulting bakers plus a flag indicating whether
-- the baker was successfully removed (i.e. whether the baker with the given ID was part of the bakers). 
removeBaker :: (MonadBlobStore m BlobRef, MonadIO m) => BakerId -> PersistentBakers -> m (Bool, PersistentBakers)
removeBaker bid !bakers = do
    let bInfoMap = bakers ^. bakerMap
    mBakerInfo <- Trie.lookup bid bInfoMap
    case mBakerInfo of
        Nothing -> return (False, bakers)
        Just (bakerRef, stake) -> do
            newBakerMap <- Trie.delete bid bInfoMap
            bkr <- loadBufferedRef bakerRef
            return (True, bakers
                            & (bakerMap .~ newBakerMap)
                            & bakersByKey . at' (bkr ^. bakerSignatureVerifyKey) .~ Nothing -- remove the baker by key as well.
                            & (bakerTotalStake %~ subtract stake)
                            & aggregationKeys %~ Set.delete (bkr ^. bakerAggregationVerifyKey))

modifyStake :: (MonadBlobStore m BlobRef, MonadIO m) => Maybe BakerId -> AmountDelta -> PersistentBakers -> m PersistentBakers
modifyStake (Just bid) delta bakers = do
    let bInfoMap = bakers ^. bakerMap
    Trie.lookup bid bInfoMap >>= \case
        Nothing -> return bakers
        Just (bakerRef, stake) -> do
            newBakerMap <- Trie.insert bid (bakerRef, applyAmountDelta delta stake) bInfoMap
            return $ bakers & (bakerMap .~ newBakerMap)
                            & (bakerTotalStake %~ applyAmountDelta delta)
modifyStake _ _ bakers = return bakers

addStake :: (MonadBlobStore m BlobRef, MonadIO m) => Maybe BakerId -> Amount -> PersistentBakers -> m PersistentBakers
addStake bid amt = modifyStake bid (amountToDelta amt)

removeStake :: (MonadBlobStore m BlobRef, MonadIO m) => Maybe BakerId -> Amount -> PersistentBakers -> m PersistentBakers
removeStake bid amt = modifyStake bid (- amountToDelta amt)
