{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Concordium.GlobalState.Persistent.Bakers where

import Control.Monad (foldM, guard)
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform
import Data.Foldable
import Data.Maybe(isNothing)

import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Basic.BlockState.Bakers as Basic
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types
import Concordium.Utils

import qualified Concordium.Crypto.SHA256 as H
import Concordium.GlobalState.Persistent.LFMBTree (LFMBTree)
import qualified Concordium.GlobalState.Persistent.LFMBTree as L
import Concordium.Types.HashableTo

-- |Representation of the set of bakers on the chain.
data PersistentBakers = PersistentBakers {
    -- |Baker information, indexed by 'BakerId'
    _bakerMap :: !(LFMBTree BakerId HashedBufferedRef (Nullable (BufferedRef BakerInfo, Amount))),
    -- |The baker ids indexed by the keys
    _bakersByKey :: !(Map.Map BakerSignVerifyKey BakerId),
    -- |The total stake delegated to all bakers
    _bakerTotalStake :: !Amount,
    -- |Aggregation keys in use
    _aggregationKeys :: !(Set.Set BakerAggregationVerifyKey)
} deriving (Show)

makeLenses ''PersistentBakers

instance MonadBlobStore m => MHashableTo m H.Hash PersistentBakers where
  getHashM PersistentBakers {..} = getHashM _bakerMap

instance MonadBlobStore m => BlobStorable m PersistentBakers where
    storeUpdate PersistentBakers{..} = do
        (pInfoMap, bInfoMap) <- storeUpdate _bakerMap
        let newBakers = PersistentBakers {
                _bakerMap = bInfoMap,
                ..
            }
        return (pInfoMap, newBakers)
    store a = fst <$> storeUpdate a
    load = do
      mBakerMap <- load
      return $ do
        _bakerMap <- mBakerMap
        -- Since we only store the baker-ID-to-baker-info map in the persistent storage,
        -- to create a 'PersistentBakers' result we need to reconstruct the baker-signature-key-to-ID map, the total baker stake,
        -- and the set of aggregation keys. For that we fold over the baker IDs, collecting the above information
        -- using this 'collectBakerInfo' function.
        -- The function
        --   - looks up a baker's 'BakerInfo' and stake in the '_bakerMap' trie
        --   - extracts the signature key from the 'BakerInfo' to construct the first map
        --   - extracts the aggregation key to compute the set of aggregation keys
        --   - uses the stake to compute the total baker stake
        let collectBakerInfo ::
              (Map.Map BakerSignVerifyKey BakerId, Amount, Set.Set BakerAggregationVerifyKey) -> -- an accumulator for the result computed so far
              (BakerId, (BufferedRef BakerInfo, Amount)) -> -- the baker ID to gather information for in this iteration
              m (Map.Map BakerSignVerifyKey BakerId, Amount, Set.Set BakerAggregationVerifyKey)
            collectBakerInfo (m, t, aks) (bid, (bInfoRef, stake)) = do
              BakerInfo {..} <- loadBufferedRef bInfoRef
              return
                ( m & at' _bakerSignatureVerifyKey ?~ bid,
                  t + stake,
                  Set.insert _bakerAggregationVerifyKey aks
                )
        list <- L.toAscPairList _bakerMap
        (_bakersByKey, _bakerTotalStake, _aggregationKeys) <- foldM collectBakerInfo (Map.empty, 0, Set.empty) [(bid, b) | (bid, Some b) <- list]
        return PersistentBakers {..}

instance MonadBlobStore m => Cacheable m PersistentBakers where
  cache pbs = do
    bm <- cache (_bakerMap pbs)
    return pbs{_bakerMap = bm}


-- |Convert an in-memory 'Basic.Bakers' value to a persistent 'PersistentBakers' value.
-- The new object is not yet stored on disk.
makePersistentBakers :: MonadBlobStore m => Basic.Bakers -> m PersistentBakers
makePersistentBakers Basic.Bakers {..} = do
    bm <- mapM toPersistentElem $ toList _bakerMap
    _bakerMap <- L.fromAscList bm
    return PersistentBakers {..}
    where
      toPersistentElem (Just FullBakerInfo {..}) = do
        bakerRef <- makeBufferedRef _bakerInfo
        return $ Some (bakerRef, _bakerStake)
      toPersistentElem Nothing = return Null

createBaker :: MonadBlobStore m => BakerInfo -> PersistentBakers -> m (Either BakerError (BakerId, PersistentBakers))
createBaker (BakerInfo _bakerElectionVerifyKey _bakerSignatureVerifyKey _bakerAggregationVerifyKey _bakerAccount) bkrs = do
    let bInfoMap = bkrs ^. bakerMap
    case bkrs ^. bakersByKey . at' _bakerSignatureVerifyKey of
        Nothing -> -- key does not yet exist
            if Set.member _bakerAggregationVerifyKey (bkrs ^. aggregationKeys) then
              return $ Left DuplicateAggregationKey
            else do -- aggregation keys is not already in use, so we insert baker
              bInfoRef <- makeBufferedRef BakerInfo{..}

              newBakerMap <- snd <$> L.append (Some (bInfoRef, _bakerStake)) bInfoMap
              return $ Right (bid, bkrs
                         & bakerMap .~ newBakerMap
                         & bakersByKey . at' _bakerSignatureVerifyKey ?~ bid
                         & aggregationKeys %~ Set.insert _bakerAggregationVerifyKey)
                where
                  _bakerStake = 0
                  bid = BakerId $ L.size bInfoMap
        Just _ -> return $ Left DuplicateSignKey

-- |Update a given baker. If this would lead to duplicate baker signing keys or
-- duplicate aggregation keys return 'Nothing'. If the baker with the given id
-- does not exist this function returns the original 'PersistentBakers' object.
updateBaker :: MonadBlobStore m => Basic.BakerUpdate -> PersistentBakers -> m (Maybe PersistentBakers)
updateBaker Basic.BakerUpdate {..} !bakers = do
  let bInfoMap = bakers ^. bakerMap
  L.lookupNullable _buId bInfoMap >>= \case
    -- baker does not exist, don't do anything
    Nothing -> return $ Just bakers
    Just (binfoRef, stake) -> do
      binfo <- loadBufferedRef binfoRef
      let updates =
            -- sequence updates to bakers and baker infos
            -- if any properties would fail, e.g., duplicate sign or aggregation keys,
            -- then this will return Nothing.
            handleUpdateAggregationKey (bakers, binfo) >>= 
              handleUpdateSignKey >>=
              handleUpdateElectionKey >>= 
              handleUpdateAccount
      case updates of
        Nothing -> return Nothing -- one of the updates are wrong
        Just (newBakers, newBakerInfo) -> do
          newBakerInfoRef <- makeBufferedRef newBakerInfo
          updated <- L.update (const $ return ((), Some (newBakerInfoRef, stake))) _buId bInfoMap
          case updated of
            Nothing -> -- should not happen because we already checked the key exists.
              return Nothing
            Just (_, newBakerMap) -> return (Just (newBakers & bakerMap .~ newBakerMap))


    where handleUpdateAggregationKey (bs, binfo) = case _buAggregationKey of
            Nothing -> return (bs, binfo)
            Just newAggKey -> do
              -- we cannot have duplicate aggregation keys
              guard (Set.notMember newAggKey (bs ^. aggregationKeys))
              let oldAggKey = binfo ^. bakerAggregationVerifyKey
                  newBakerInfo = binfo & bakerAggregationVerifyKey .~ newAggKey
              return (bs
                       & aggregationKeys %~ Set.delete oldAggKey -- delete old aggregation key from pool of aggregation keys in use
                       & aggregationKeys %~ Set.insert newAggKey, -- add new aggregation key to pool
                       newBakerInfo)

          handleUpdateSignKey (bs, binfo) = case _buSignKey of
            Nothing -> return (bs, binfo)
            Just newSignKey -> do -- new signing key, make sure it is new
              guard (isNothing (bs ^. bakersByKey . at' newSignKey))
              -- we now know we have a fresh signing key 
              let newBakerInfo = binfo & bakerSignatureVerifyKey .~ newSignKey
              return $ (bs
                         & bakersByKey . at' (binfo ^. bakerSignatureVerifyKey) .~ Nothing -- remove old identification
                         & bakersByKey . at' newSignKey ?~ _buId,  -- and add new identification
                         newBakerInfo)

          -- it is currently allowed by the protocol to have duplicate election verification keys.
          handleUpdateElectionKey (bs, binfo) = case _buElectionKey of
            Nothing -> return (bs, binfo)
            Just newElectionKey -> do
              let newBakerInfo = binfo & bakerElectionVerifyKey .~ newElectionKey
              return (bs, newBakerInfo)

          handleUpdateAccount (bs, binfo) = case _buAccount of
            Nothing -> return (bs, binfo)
            Just newAccount -> do
              let newBakerInfo = binfo & bakerAccount .~ newAccount
              return (bs, newBakerInfo)

-- | Removes a baker from the 'PersistentBakers' data type and returns the resulting bakers plus a flag indicating whether
-- the baker was successfully removed (i.e. whether the baker with the given ID was part of the bakers).
removeBaker :: MonadBlobStore m => BakerId -> PersistentBakers -> m (Bool, PersistentBakers)
removeBaker bid !bakers = do
    let bInfoMap = bakers ^. bakerMap
    mBakerInfo <- L.lookupNullable bid bInfoMap
    case mBakerInfo of
      Nothing -> return (False, bakers)
      Just (bakerRef, stake) -> do
        updated <- L.delete bid bInfoMap
        case updated of
          Nothing -> return (False, bakers)
          Just newBakerMap -> do
            bkr <- loadBufferedRef bakerRef
            return (True, bakers
                            & (bakerMap .~ newBakerMap)
                            & bakersByKey . at' (bkr ^. bakerSignatureVerifyKey) .~ Nothing -- remove the baker by key as well.
                            & (bakerTotalStake %~ subtract stake)
                            & aggregationKeys %~ Set.delete (bkr ^. bakerAggregationVerifyKey))

-- | Modifies the stake of a baker.  If no 'BakerId' is given, or there is no
-- baker with the given 'BakerId', no change is made.
modifyStake :: MonadBlobStore m => Maybe BakerId -> AmountDelta -> PersistentBakers -> m PersistentBakers
modifyStake (Just bid) delta bakers = do
    let bInfoMap = bakers ^. bakerMap
    updated <- L.update thisUpdate bid bInfoMap
    case updated of
      Nothing -> return bakers
      Just (True, newBakerMap) ->
        return $
          bakers & (bakerMap .~ newBakerMap)
            & (bakerTotalStake %~ applyAmountDelta delta)
      Just (False, _) -> return bakers
    where
      thisUpdate Null = return (False, Null)
      thisUpdate (Some (bakerRef, stake)) = return (True, Some (bakerRef, applyAmountDelta delta stake))
modifyStake _ _ bakers = return bakers

-- | Adds an amount to the stake of a baker.  If no 'BakerId' is given, or there is no
-- baker with the given 'BakerId', no change is made.
addStake :: MonadBlobStore m => Maybe BakerId -> Amount -> PersistentBakers -> m PersistentBakers
addStake bid amt = modifyStake bid (amountToDelta amt)

-- | Removes an amount from the stake of a baker.  If no 'BakerId' is given, or there is no
-- baker with the given 'BakerId', no change is made.
removeStake :: MonadBlobStore m => Maybe BakerId -> Amount -> PersistentBakers -> m PersistentBakers
removeStake bid amt = modifyStake bid (- amountToDelta amt)
