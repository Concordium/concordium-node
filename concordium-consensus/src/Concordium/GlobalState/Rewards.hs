{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Concordium.GlobalState.Rewards where

import Concordium.Types.HashableTo
import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types

import Data.Serialize
import Lens.Micro.Platform
data RewardAccounts = RewardAccounts {
  -- |The Baking Reward Account. A fraction of newly minted GTUs are paid
  -- in here. These are distributed to the bakers of a previous epoch in
  -- the first block of a new epoch.
  _bakingRewardAccount :: !Amount,
  -- |The Finalization Reward Account. A fraction of newly minted GTUs are
  -- paid in here. These are distributed to the finalizers whenever a
  -- finalization record is included in a block.
  _finalizationRewardAccount :: !Amount,
  -- |The GAS Account. A fraction of execution costs is paid in here.
  -- A fraction is paid out to the baker of a block, with additional
  -- fractions to subsidise account creation, update transactions and
  -- the inclusion of finalization records in blocks.
  _gasAccount :: !Amount
  } deriving (Show, Eq)
makeClassy ''RewardAccounts

-- |The grand total of undistributed rewards. Comes in handy during testing.
rewardsTotal :: RewardAccounts -> Amount
rewardsTotal RewardAccounts{..} = _bakingRewardAccount + _finalizationRewardAccount + _gasAccount

-- |This datastructure contains the various (logical) accounts used for
-- the chain tokenomics. It also records the total of all GTU in existence,
-- and the total of all encrypted GTU.
data BankStatus = BankStatus {
  -- |Total amount of GTU in existence.
  _totalGTU :: !Amount,
  -- |Total amount of encrypted amounts. This is not an important field, but can
  -- be used for debugging.
  _totalEncryptedGTU :: !Amount,
  -- |Reward accounts.
  _bankRewardAccounts :: !RewardAccounts
  } deriving(Show, Eq)

makeLenses ''BankStatus
instance HasRewardAccounts BankStatus where
  rewardAccounts = bankRewardAccounts

instance Serialize BankStatus where
    put BankStatus{_bankRewardAccounts=RewardAccounts{..},..} = do
        put _totalGTU
        put _totalEncryptedGTU
        put _bakingRewardAccount
        put _finalizationRewardAccount
        put _gasAccount
    get = do
        _totalGTU <- get
        _totalEncryptedGTU <- get
        _bakingRewardAccount <- get
        _finalizationRewardAccount <- get
        _gasAccount <- get
        return BankStatus{_bankRewardAccounts=RewardAccounts{..},..}

-- |Define the hash of Bankstatus, for use in hashing the blockstate.
instance HashableTo H.Hash BankStatus where
  getHash BankStatus{_bankRewardAccounts=RewardAccounts{..},..} = H.hash $
                           "total" <>
                           encode _totalGTU <>
                           "secret" <>
                           encode _totalEncryptedGTU <>
                           "bakingRewardAccount" <>
                           encode _bakingRewardAccount <>
                           "finalizationRewardAccount" <>
                           encode _finalizationRewardAccount <>
                           "gasAccount" <>
                           encode _gasAccount

-- |Bank with no money.
emptyBankStatus :: BankStatus
emptyBankStatus = BankStatus{_bankRewardAccounts=RewardAccounts{..},..}
  where _totalGTU = 0
        _totalEncryptedGTU = 0
        _bakingRewardAccount = 0
        _finalizationRewardAccount = 0
        _gasAccount = 0

makeGenesisBankStatus :: Amount -- ^Total amount of GTU in existence.
                      -> BankStatus
makeGenesisBankStatus genesisTotal = emptyBankStatus{_totalGTU = genesisTotal}

newtype EpochBlocksHash = EpochBlocksHash {ebHash :: H.Hash}
  deriving newtype (Eq, Ord, Show, Serialize)

emptyEpochBlocksHash :: EpochBlocksHash
emptyEpochBlocksHash = EpochBlocksHash $ H.hash "emptyEpochBlocks"

epochBlockHash :: BakerId -> EpochBlocksHash -> EpochBlocksHash
epochBlockHash bid h = EpochBlocksHash $ H.hash $
    "epochBlock" <>
    encode bid <>
    H.hashToByteString (ebHash h)

newtype PoolRewardsHash = PoolRewardsHash {prHash :: H.Hash}
  deriving newtype (Eq, Ord, Show, Serialize)

-- |Hash of block reward details.
data BlockRewardDetailsHash (av :: AccountVersion) where
    BlockRewardDetailsHashV0 :: !EpochBlocksHash -> BlockRewardDetailsHash 'AccountV0
    BlockRewardDetailsHashV1 :: !PoolRewardsHash -> BlockRewardDetailsHash 'AccountV1

deriving instance Show (BlockRewardDetailsHash av)
deriving instance Eq (BlockRewardDetailsHash av)

-- |SHA256 hash of 'BlockRewardDetailsHash'.
brdHash :: BlockRewardDetailsHash av -> H.Hash
brdHash (BlockRewardDetailsHashV0 eb) = ebHash eb
brdHash (BlockRewardDetailsHashV1 ha) = prHash ha
