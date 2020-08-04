{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Concordium.GlobalState.Persistent.Bakers where

import Control.Monad (foldM)
import Control.Monad.IO.Class
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform

import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Basic.BlockState.Bakers as Basic
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types
import Concordium.Utils

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.GlobalState.Basic.BlockState.LFMBTree as LB
import Concordium.GlobalState.Persistent.LFMBTree (LFMBTree)
import qualified Concordium.GlobalState.Persistent.LFMBTree as L
import Concordium.Types.HashableTo
import Control.Monad.Reader.Class

-- |Representation of the set of bakers on the chain.
data PersistentBakers = PersistentBakers {
    -- |Baker information, indexed by 'BakerId'
    _bakerMap :: !(LFMBTree HashedBufferedRef (Nullable (BufferedRef BakerInfo, Amount))),
    -- |The baker ids indexed by the keys
    _bakersByKey :: !(Map.Map BakerSignVerifyKey BakerId),
    -- |The total stake delegated to all bakers
    _bakerTotalStake :: !Amount,
    -- |Aggregation keys in use
    _aggregationKeys :: !(Set.Set BakerAggregationVerifyKey)
} deriving (Show)

makeLenses ''PersistentBakers

type CanStorePersistentBakers r m =
  ( MonadIO m,
    MonadReader r m,
    HasBlobStore r
  )

instance CanStorePersistentBakers r m => MHashableTo m H.Hash PersistentBakers where
  getHashM PersistentBakers {..} = getHashM _bakerMap

instance CanStorePersistentBakers r m => BlobStorable m BlobRef PersistentBakers where
    storeUpdate p PersistentBakers{..} = do
        (pInfoMap, bInfoMap) <- storeUpdate p _bakerMap
        let newBakers = PersistentBakers {
                _bakerMap = bInfoMap,
                ..
            }
        return (pInfoMap, newBakers)
    store p a = fst <$> storeUpdate p a
    load p = do
      mBakerMap <- load p
      return $ do
        _bakerMap <- mBakerMap
        -- Since we only store the baker-ID-to-baker-info map and next baker ID in the persistent storage,
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
        (_bakersByKey, _bakerTotalStake, _aggregationKeys) <- (\l -> foldM collectBakerInfo (Map.empty, 0, Set.empty) [(bid, b) | (bid, Some b) <- zip [0 ..] l]) =<< L.toList _bakerMap
        return PersistentBakers {..}

-- |Convert an in-memory 'Basic.Bakers' value to a persistent 'PersistentBakers' value.
-- The new object is not yet stored on disk.
makePersistentBakers :: CanStorePersistentBakers r m => Basic.Bakers -> m PersistentBakers
makePersistentBakers Basic.Bakers {..} = do
    bm <- mapM toPersistentElem $ LB.toList _bakerMap
    _bakerMap <- L.fromList bm
    return PersistentBakers {..}
    where
      toPersistentElem (Just FullBakerInfo {..}) = do
        bakerRef <- makeBufferedRef _bakerInfo
        return $ Some (bakerRef, _bakerStake)
      toPersistentElem Nothing = return Null

createBaker :: CanStorePersistentBakers r m => BakerInfo -> PersistentBakers -> m (Either BakerError (BakerId, PersistentBakers))
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

-- |Update a given baker. If this would lead to duplicate baker signing keys
-- return 'Nothing'. If the baker with the given id does not exist this function
-- returns the original 'PersistentBakers' object.
updateBaker :: CanStorePersistentBakers r m => Basic.BakerUpdate -> PersistentBakers -> m (Maybe PersistentBakers)
updateBaker Basic.BakerUpdate {..} !bakers = do
  let bInfoMap = bakers ^. bakerMap
      BakerId bid = _buId
  L.lookup bid bInfoMap >>= \case
    Nothing -> return $ Just bakers
    Just Null -> return $ Just bakers
    Just (Some (binfoRef, stake)) -> do
      binfo <- loadBufferedRef binfoRef
      let bacc = _buAccount ^. non (binfo ^. bakerAccount)
      case _buSignKey of
           Nothing -> do -- don't update the sign key, no clash possible
             newBakerInfoRef <- makeBufferedRef (binfo & bakerAccount .~ bacc)
             updated <- L.update (const $ return ((), Some (newBakerInfoRef, stake))) bid bInfoMap
             case updated of
               Nothing -> return $ Just bakers
               Just (_, newBakerMap) -> return $ Just $ bakers & bakerMap .~ newBakerMap
           Just newSignKey ->
             -- new signing key, make sure it is new
             case bakers ^. bakersByKey . at' newSignKey of
               Just _ -> -- existing signing key
                 return Nothing
               Nothing -> do -- fresh signing key
                 newBakerInfoRef <- makeBufferedRef (binfo & bakerSignatureVerifyKey .~ newSignKey)
                 updated <- L.update (const $ return ((), Some (newBakerInfoRef, stake))) bid bInfoMap
                 case updated of
                   Nothing -> return $ Just bakers
                   Just (_, newBakerMap) ->

                     return $ Just (bakers
                       & bakerMap .~ newBakerMap
                       & bakersByKey . at' (binfo ^. bakerSignatureVerifyKey) .~ Nothing -- remove old identification
                       & bakersByKey . at' newSignKey ?~ _buId) -- and add new identification


-- | Removes a baker from the 'PersistentBakers' data type and returns the resulting bakers plus a flag indicating whether
-- the baker was successfully removed (i.e. whether the baker with the given ID was part of the bakers).
removeBaker :: CanStorePersistentBakers r m => BakerId -> PersistentBakers -> m (Bool, PersistentBakers)
removeBaker (BakerId bid) !bakers = do
    let bInfoMap = bakers ^. bakerMap
    mBakerInfo <- L.lookup bid bInfoMap
    case mBakerInfo of
      Nothing -> return (False, bakers)
      Just Null -> return (False, bakers)
      Just (Some (bakerRef, stake)) -> do
        updated <- L.update (const $ return ((), Null)) bid bInfoMap
        case updated of
          Nothing -> return (False, bakers)
          Just (_, newBakerMap) -> do
            bkr <- loadBufferedRef bakerRef
            return (True, bakers
                            & (bakerMap .~ newBakerMap)
                            & bakersByKey . at' (bkr ^. bakerSignatureVerifyKey) .~ Nothing -- remove the baker by key as well.
                            & (bakerTotalStake %~ subtract stake))

modifyStake :: CanStorePersistentBakers r m => Maybe BakerId -> AmountDelta -> PersistentBakers -> m PersistentBakers
modifyStake (Just (BakerId bid)) delta bakers = do
    let bInfoMap = bakers ^. bakerMap
    updated <- L.update thisUpdate bid bInfoMap
    case updated of
      Nothing -> undefined
      Just (_, newBakerMap) ->
        return $
          bakers & (bakerMap .~ newBakerMap)
            & (bakerTotalStake %~ applyAmountDelta delta)
    where
      thisUpdate Null = return ((), Null)
      thisUpdate (Some (bakerRef, stake)) = return ((), Some (bakerRef, applyAmountDelta delta stake))
modifyStake _ _ bakers = return bakers

addStake :: CanStorePersistentBakers r m => Maybe BakerId -> Amount -> PersistentBakers -> m PersistentBakers
addStake bid amt = modifyStake bid (amountToDelta amt)

removeStake :: CanStorePersistentBakers r m => Maybe BakerId -> Amount -> PersistentBakers -> m PersistentBakers
removeStake bid amt = modifyStake bid (- amountToDelta amt)
