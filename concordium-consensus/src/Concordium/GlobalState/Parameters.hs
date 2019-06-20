{-# LANGUAGE DeriveGeneric #-}
-- |This module defines types for blockchain parameters, including genesis data,
-- baker parameters and finalization parameters.
module Concordium.GlobalState.Parameters where

import GHC.Generics
import qualified Data.Map as Map
import Data.Word
import Data.Serialize

import Concordium.Types

data BakerInfo = BakerInfo {
    bakerElectionVerifyKey :: BakerElectionVerifyKey,
    bakerSignatureVerifyKey :: BakerSignVerifyKey,
    bakerLotteryPower :: LotteryPower,
    bakerAccount :: AccountAddress
} deriving (Eq, Generic, Show)
instance Serialize BakerInfo where

data BirkParameters = BirkParameters {
    birkLeadershipElectionNonce :: LeadershipElectionNonce,
    birkElectionDifficulty :: ElectionDifficulty,
    birkBakers :: Map.Map BakerId BakerInfo,
    -- |Next available baker id. This is needed so that we do not recycle baker
    -- ids even if bakers are removed.
    nextBakerId :: BakerId
} deriving (Eq, Generic, Show)
instance Serialize BirkParameters where

birkBaker :: BakerId -> BirkParameters -> Maybe BakerInfo
birkBaker bid bps = Map.lookup bid (birkBakers bps)


data VoterInfo = VoterInfo {
    voterVerificationKey :: VoterVerificationKey,
    voterVRFKey :: VoterVRFPublicKey,
    voterPower :: VoterPower
} deriving (Eq, Generic, Show)
instance Serialize VoterInfo where

data FinalizationParameters = FinalizationParameters [VoterInfo]
    deriving (Eq, Generic, Show)
instance Serialize FinalizationParameters where

-- | Time in seconds since the epoch
type Timestamp = Word64
-- | Time duration in seconds
type Duration = Word64

data GenesisData = GenesisData {
    genesisTime :: Timestamp,
    genesisSlotDuration :: Duration,
    genesisBirkParameters :: BirkParameters,
    genesisBakerAccounts :: [Account],
    genesisFinalizationParameters :: FinalizationParameters
} deriving (Generic, Show)

instance Serialize GenesisData where
