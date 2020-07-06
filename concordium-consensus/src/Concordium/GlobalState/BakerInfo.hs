{-# LANGUAGE DeriveGeneric, TemplateHaskell #-}
{-# LANGUAGE RecordWildCards, BangPatterns, TypeFamilies #-}
module Concordium.GlobalState.BakerInfo where

import GHC.Generics
import Data.Serialize
import Lens.Micro.Platform

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
} deriving (Eq, Generic, Show)
instance Serialize BakerInfo

makeLenses ''BakerInfo

data FullBakerInfo = FullBakerInfo {
    _bakerInfo :: !BakerInfo,
    _bakerStake :: !Amount
} deriving (Eq, Generic, Show)
instance Serialize FullBakerInfo

makeLenses ''FullBakerInfo

data BakerError =
      DuplicateSignKey
    | DuplicateAggregationKey
  deriving (Eq, Show)
