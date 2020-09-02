{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, BangPatterns, TypeFamilies #-}
module Concordium.GlobalState.BakerInfo where

import Data.Serialize
import Lens.Micro.Platform
import Concordium.Types.HashableTo
import qualified Concordium.Crypto.SHA256 as H

import Concordium.Types

data BakerInfo = BakerInfo {
    -- |The baker's public VRF key
    _bakerElectionVerifyKey :: !BakerElectionVerifyKey,
    -- |The baker's public signature key
    _bakerSignatureVerifyKey :: !BakerSignVerifyKey,
    -- |The baker's public key for finalization record aggregation
    _bakerAggregationVerifyKey :: !BakerAggregationVerifyKey,
    -- |The account associated with the baker
    _bakerAccount :: !AccountAddress
} deriving (Eq, Show)

instance Serialize BakerInfo where
  put BakerInfo{..} = do
    put _bakerElectionVerifyKey
    put _bakerSignatureVerifyKey
    put _bakerAggregationVerifyKey
    put _bakerAccount
  get = do
    _bakerElectionVerifyKey <- get
    _bakerSignatureVerifyKey <- get
    _bakerAggregationVerifyKey <- get
    _bakerAccount <- get
    return BakerInfo{..}

makeLenses ''BakerInfo

data FullBakerInfo = FullBakerInfo {
    _bakerInfo :: !BakerInfo,
    _bakerStake :: !Amount
} deriving (Eq, Show)

instance Serialize FullBakerInfo where
  put FullBakerInfo{..} = do
    put _bakerInfo
    put _bakerStake
  get = do
    _bakerInfo <- get
    _bakerStake <- get
    return FullBakerInfo{..}

makeLenses ''FullBakerInfo

instance HashableTo H.Hash FullBakerInfo where
  getHash = H.hash . encode

data BakerError =
      DuplicateSignKey
    | DuplicateAggregationKey
  deriving (Eq, Show)
