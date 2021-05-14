{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- |Types for representing the results of consensus queries.
module Concordium.Queries.Types where

import Data.Aeson
import Data.Aeson.TH
import Data.Char (isLower)
import qualified Data.Map as Map
import Data.Time
import qualified Data.Vector as Vec

import Concordium.Common.Version
import Concordium.GlobalState.Account
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule (AccountReleaseSchedule (..))
import Concordium.GlobalState.Basic.BlockState.Updates (Updates)
import Concordium.GlobalState.Finalization
import Concordium.ID.Types
import Concordium.Types
import Concordium.Types.Execution
import Concordium.Types.Transactions
import Concordium.Utils
import qualified Data.Sequence as Seq

-- |Result type for @getConsensusStatus@ queries.
data ConsensusStatus = ConsensusStatus
    { -- |Hash of the current best block
      csBestBlock :: !BlockHash,
      -- |Hash of the (original) genesis block
      csGenesisBlock :: !BlockHash,
      -- |Time of the (original) genesis block
      csGenesisTime :: !UTCTime,
      -- |(Current) slot duration in milliseconds
      csSlotDuration :: !Duration,
      -- |(Current) epoch duration in milliseconds
      csEpochDuration :: !Duration,
      -- |Hash of the last finalized block
      csLastFinalizedBlock :: !BlockHash,
      -- |Height of the best block (since latest regenesis)
      csBestBlockHeight :: !BlockHeight,
      -- |Height of the last finalized block (since latest regenesis)
      csLastFinalizedBlockHeight :: !BlockHeight,
      -- |Total number of blocks received
      csBlocksReceivedCount :: !Int,
      -- |The last time a block was received
      csBlockLastReceivedTime :: !(Maybe UTCTime),
      -- |Moving average latency between a block's slot time and received time
      csBlockReceiveLatencyEMA :: !Double,
      -- |Standard deviation of moving average latency between a block's slot time and received time
      csBlockReceiveLatencyEMSD :: !Double,
      -- |Moving average time between receiving blocks
      csBlockReceivePeriodEMA :: !(Maybe Double),
      -- |Standard deviation of moving average time between receiving blocks
      csBlockReceivePeriodEMSD :: !(Maybe Double),
      -- |Total number of blocks received and verified
      csBlocksVerifiedCount :: !Int,
      -- |The last time a block was verified (added to the tree)
      csBlockLastArrivedTime :: !(Maybe UTCTime),
      -- |Moving average latency between a block's slot time and its arrival
      csBlockArriveLatencyEMA :: !Double,
      -- |Standard deviation of moving average latency between a block's slot time and its arrival
      csBlockArriveLatencyEMSD :: !Double,
      -- |Moving average time between block arrivals
      csBlockArrivePeriodEMA :: !(Maybe Double),
      -- |Standard deviation of moving average time between block arrivals
      csBlockArrivePeriodEMSD :: !(Maybe Double),
      -- |Moving average number of transactions per block
      csTransactionsPerBlockEMA :: !Double,
      -- |Standard deviation of moving average number of transactions per block
      csTransactionsPerBlockEMSD :: !Double,
      -- |Number of finalizations
      csFinalizationCount :: !Int,
      -- |Time of last verified finalization
      csLastFinalizedTime :: !(Maybe UTCTime),
      -- |Moving average time between finalizations
      csFinalizationPeriodEMA :: !(Maybe Double),
      -- |Standard deviation of moving average time between finalizations
      csFinalizationPeriodEMSD :: !(Maybe Double)
    }
    deriving (Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''ConsensusStatus)

-- |Result type for @getBranches@ query.
data Branch = Branch
    { -- |Block hash
      branchBlockHash :: BlockHash,
      -- |Child branches
      branchChildren :: [Branch]
    }
    deriving (Eq, Ord, Show)

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''Branch)

-- |Result type for @getNextAccountNonce@ query.
data NextAccountNonce = NextAccountNonce
    { -- |The next account nonce
      nanNonce :: Nonce,
      -- |True if all transactions on the account are finalized
      nanAllFinal :: Bool
    }

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''NextAccountNonce)

-- |Result type for @getBlockInfo@ query.
data BlockInfo = BlockInfo
    { -- |The block hash
      biBlockHash :: BlockHash,
      -- |The parent block hash
      biBlockParent :: BlockHash,
      -- |The last finalized block when this block was baked
      biBlockLastFinalized :: BlockHash,
      -- |The height of this block
      biBlockHeight :: BlockHeight,
      -- |The time the block was received
      biBlockReceiveTime :: UTCTime,
      -- |The time the block was verified
      biBlockArriveTime :: UTCTime,
      -- |The slot number in which the block was baked
      biBlockSlot :: Slot,
      -- |The time of the slot in which the block was baked
      biBlockSlotTime :: UTCTime,
      -- |The identifier of the block baker, or @Nothing@ for a
      -- genesis block.
      biBlockBaker :: Maybe BakerId,
      -- |Whether the block is finalized
      biFinalized :: Bool,
      -- |The number of transactions in the block
      biTransactionCount :: Int,
      -- |The energy cost of the transaction in the block
      biTransactionEnergyCost :: Energy,
      -- |The size of the transactions
      biTransactionsSize :: Int,
      -- |The hash of the block state
      biBlockStateHash :: StateHash
    }

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''BlockInfo)

-- |Details of a party in a finalization.
data FinalizationSummaryParty = FinalizationSummaryParty
    { -- |The identity of the baker
      fspBakerId :: BakerId,
      -- |The party's relative weight in the committee
      fspWeight :: Integer,
      -- |Whether the party's signature is present
      fspSigned :: Bool
    }

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''FinalizationSummaryParty)

-- |Details of a finalization record.
data FinalizationSummary = FinalizationSummary
    { -- |Hash of the finalized block
      fsFinalizationBlockPointer :: BlockHash,
      -- |Index of the finalization
      fsFinalizationIndex :: FinalizationIndex,
      -- |Finalization delay value
      fsFinalizationDelay :: BlockHeight,
      -- |The finalization committee
      fsFinalizers :: Vec.Vector FinalizationSummaryParty
    }

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''FinalizationSummary)

-- |Detailed information about a block.
data BlockSummary = BlockSummary
    { -- |Details of transactions in the block
      bsTransactionSummaries :: Vec.Vector TransactionSummary,
      -- |Details of special events in the block
      bsSpecialEvents :: Seq.Seq SpecialTransactionOutcome,
      -- |Details of the finalization record in the block (if any)
      bsFinalizationData :: Maybe FinalizationSummary,
      -- |Details of the update queues and chain parameters as of the block
      bsUpdates :: Updates
    }

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''BlockSummary)

data AccountInfo = AccountInfo
    { -- |The next nonce for the account
      aiAccountNonce :: Nonce,
      -- |The total non-encrypted balance on the account
      aiAccountAmount :: Amount,
      -- |The release schedule for locked amounts on the account
      aiAccountReleaseSchedule :: AccountReleaseSchedule,
      -- |The credentials on the account
      aiAccountCredentials :: Map.Map CredentialIndex (Versioned AccountCredential),
      -- |Number of credentials required to sign a valid transaction
      aiAccountThreshold :: AccountThreshold,
      -- |The encrypted amount on the account
      aiAccountEncryptedAmount :: AccountEncryptedAmount,
      -- |The encryption key for the account
      aiAccountEncryptionKey :: AccountEncryptionKey,
      -- |The account index
      aiAccountIndex :: AccountIndex,
      -- |The baker associated with the account (if any)
      aiBaker :: Maybe AccountBaker
    }

instance ToJSON AccountInfo where
    toJSON AccountInfo{..} =
        object $
            [ "accountNonce" .= aiAccountNonce,
              "accountAmount" .= aiAccountAmount,
              "accountReleaseSchedule" .= aiAccountReleaseSchedule,
              "accountCredentials" .= aiAccountCredentials,
              "accountThreshold" .= aiAccountThreshold,
              "accountEncryptedAmount" .= aiAccountEncryptedAmount,
              "accountEncryptionKey" .= aiAccountEncryptionKey,
              "accountIndex" .= aiAccountIndex
            ]
                <> maybe [] (\b -> ["accountBaker" .= b]) aiBaker
    toEncoding AccountInfo{..} =
        pairs $
            "accountNonce" .= aiAccountNonce
                <> "accountAmount" .= aiAccountAmount
                <> "accountReleaseSchedule" .= aiAccountReleaseSchedule
                <> "accountCredentials" .= aiAccountCredentials
                <> "accountThreshold" .= aiAccountThreshold
                <> "accountEncryptedAmount" .= aiAccountEncryptedAmount
                <> "accountEncryptionKey" .= aiAccountEncryptionKey
                <> "accountIndex" .= aiAccountIndex
                <> maybe mempty ("accountBaker" .=) aiBaker

data RewardStatus = RewardStatus
    { -- |The total GTU in existence
      rsTotalAmount :: Amount,
      -- |The total GTU in encrypted balances
      rsTotalEncryptedAmount :: Amount,
      -- |The amount in the baking reward account
      rsBakingRewardAccount :: Amount,
      -- |The amount in the finalization reward account
      rsFinalizationRewardAccount :: Amount,
      -- |The amount in the GAS account
      rsGasAccount :: Amount
    }

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''RewardStatus)

-- |Summary of a baker.
data BakerSummary = BakerSummary
    { -- |Baker ID
      bsBakerId :: BakerId,
      -- |(Approximate) lottery power
      bsBakerLotteryPower :: Double,
      -- |Baker account (should never be @Nothing@)
      bsBakerAccount :: Maybe AccountAddress
    }

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''BakerSummary)

-- |Summary of the birk parameters applicable to a particular block.
data BlockBirkParameters = BlockBirkParameters
    { -- |Baking lottery election difficulty
      bbpElectionDifficulty :: ElectionDifficulty,
      -- |Current leadership election nonce for the lottery
      bbpElectionNonce :: LeadershipElectionNonce,
      -- |List of the currently eligible bakers
      bbpBakers :: Vec.Vector BakerSummary
    }

$(deriveJSON defaultOptions{fieldLabelModifier = firstLower . dropWhile isLower} ''BlockBirkParameters)

-- |The status of a transaction that is present in the transaction table.
data TransactionStatus
    = -- |Transaction was received but is not in any blocks
      Received
    | -- |Transaction was received an is present in some (non-finalized) block(s)
      Committed (Map.Map BlockHash (Maybe TransactionSummary))
    | -- |Transaction has been finalized in a block
      Finalized BlockHash (Maybe TransactionSummary)

instance ToJSON TransactionStatus where
    toJSON Received = object ["status" .= String "received"]
    toJSON (Committed m) =
        object
            [ "status" .= String "finalized",
              "outcomes" .= m
            ]
    toJSON (Finalized bh outcome) =
        object
            [ "status" .= String "finalized",
              "outcomes" .= Map.singleton bh outcome
            ]

-- |The status of a transaction with respect to a specified block
data BlockTransactionStatus
    = -- |Either the transaction is not in that block, or that block is not live
      BTSNotInBlock
    | -- |The transaction was received but not known to be in that block
      BTSReceived
    | -- |The transaction is in that (non-finalized) block
      BTSCommitted (Maybe TransactionSummary)
    | -- |The transaction is in that (finalized) block
      BTSFinalized (Maybe TransactionSummary)

instance ToJSON BlockTransactionStatus where
    toJSON BTSNotInBlock = Null
    toJSON BTSReceived = object ["status" .= String "received"]
    toJSON (BTSCommitted outcome) =
        object
            [ "status" .= String "committed",
              "result" .= outcome
            ]
    toJSON (BTSFinalized outcome) =
        object
            [ "status" .= String "finalized",
              "result" .= outcome
            ]

-- |Result of a baker status query.
data BakerStatus
    = -- |The baker is a member of the current committee
      ActiveBaker !BakerId
    | -- |The account has a baker, but it is not yet in the committee
      InactiveBaker !BakerId
    | -- |The baker id does not correspond with a current baker
      NoBaker
    | -- |The baker may exist, but the keys do not match
      BadKeys !BakerId
    deriving (Eq, Ord, Show)
