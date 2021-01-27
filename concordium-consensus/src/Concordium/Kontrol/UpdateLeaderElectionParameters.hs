{-# LANGUAGE ViewPatterns #-}
-- |Functionality for updating and computing leadership election nonces.
module Concordium.Kontrol.UpdateLeaderElectionParameters (updateSeedState, computeLeadershipElectionNonce) where

import Data.Serialize

import Concordium.Types
import Concordium.Types.SeedState
import Concordium.Crypto.SHA256 as H
import Concordium.Crypto.VRF


-- |Compute the update for the leadership election nonce due to a particular block nonce.
updateWithBlockNonce :: BlockNonce -> H.Hash -> H.Hash
updateWithBlockNonce bn un = hash $ runPut $ put un <> put (proofToHash bn)

-- |Compute the update for the leadership election nonce due to completion of an epoch.
updateWithEpoch
    :: Epoch
    -- ^Epoch that has been completed
    -> H.Hash
    -- ^Running updated leadership election nonce
    -> H.Hash
updateWithEpoch e n = hash $ runPut $ put n <> put e

-- |Update the seed state. The slot must not belong to a prior epoch.
updateSeedState :: Slot -> BlockNonce -> SeedState -> SeedState
updateSeedState slot bn state = case compare newEpoch oldEpoch of
        EQ -> updateNonce state
        GT -> updateEpochs oldEpoch state
        LT -> error $ "updateSeedState: new epoch (" ++ show newEpoch ++ ") precedes current epoch (" ++ show oldEpoch ++ ")"
    where
        oldEpoch = epoch state
        el = epochLength state
        (fromIntegral -> newEpoch, slotRem) = slot `quotRem` el
        shouldContributeBlockNonce = 3 * slotRem < 2 * el
        -- If the slot falls within the first 2/3 of the epoch's slots,
        -- add the block nonce to the updatedNonce.
        updateNonce
            | shouldContributeBlockNonce = \s -> s{
                    updatedNonce = updateWithBlockNonce bn (updatedNonce s)
                }
            | otherwise = id
        updateEpochs :: Epoch -> SeedState -> SeedState
        updateEpochs e s
            | e == newEpoch = updateNonce s{epoch = newEpoch}
            | otherwise = updateEpochs (e+1) s{currentLeadershipElectionNonce = h, updatedNonce = h}
                where
                    h = updateWithEpoch e (updatedNonce s)

-- |Derive the leadership election nonce for a particular slot from the seed state.
-- This compensates for if the slot is in a different epoch.
computeLeadershipElectionNonce :: SeedState -> Slot -> LeadershipElectionNonce
computeLeadershipElectionNonce state slot = case compare newEpoch oldEpoch of
        EQ -> currentLeadershipElectionNonce state
        GT -> updateEpochs oldEpoch (currentLeadershipElectionNonce state)
        LT -> error $ "computeLeadershipElectionNonce: new epoch (" ++ show newEpoch ++ ") precedes current epoch (" ++ show oldEpoch ++ ")"
    where
        oldEpoch = epoch state
        el = epochLength state
        newEpoch = fromIntegral $ slot `quot` el
        updateEpochs e n
            | e == newEpoch = n
            | otherwise = updateEpochs (e+1) (updateWithEpoch e n)