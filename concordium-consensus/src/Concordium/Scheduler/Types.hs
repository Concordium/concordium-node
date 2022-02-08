module Concordium.Scheduler.Types (module Concordium.Scheduler.Types,
                                   module Concordium.Types,
                                   module Concordium.Types.Updates,
                                   module Concordium.Types.Execution,
                                   module Concordium.Types.Transactions,
                                   module Concordium.GlobalState.Instance,
                                   module Concordium.GlobalState.Rewards,
                                   module Concordium.GlobalState.Parameters,
                                   module Concordium.Types.IdentityProviders,
                                   module Concordium.Types.AnonymityRevokers,
                                   IdentityProviderIdentity) where

import Concordium.Types
import Concordium.Types.Updates
import Concordium.Types.Execution
import Concordium.GlobalState.Instance
import Concordium.GlobalState.Rewards
import Concordium.Types.Transactions
import Concordium.GlobalState.Parameters
import Concordium.Types.IdentityProviders
import Concordium.Types.AnonymityRevokers

import Concordium.ID.Types(IdentityProviderIdentity)
import qualified Concordium.TransactionVerification as TVer

-- |Result of constructing a block from 'GroupedTransactions'.
data FilteredTransactions = FilteredTransactions {
  -- |Transactions which have been added to the block, in the order added, with results.
  ftAdded :: [(TVer.BlockItemWithStatus, TransactionSummary)],
  -- |Transactions which failed. No order is guaranteed.
  ftFailed :: [(TVer.TransactionWithStatus, FailureKind)],
  -- |Credential deployments which failed. No order is guaranteed.
  ftFailedCredentials :: [(TVer.CredentialDeploymentWithStatus, FailureKind)],
  -- |Update instructions which failed. No order is guaranteed.
  ftFailedUpdates :: [(TVer.ChainUpdateWithStatus, FailureKind)],
  -- |Transactions which were not processed. No order is guaranteed.
  ftUnprocessed :: [TVer.TransactionWithStatus],
  -- |Credentials which were not processed. No order is guaranteed.
  ftUnprocessedCredentials :: [TVer.CredentialDeploymentWithStatus],
  -- |Update instructions which were not processed. No order is guaranteed.
  ftUnprocessedUpdates :: [TVer.ChainUpdateWithStatus]
  }
  deriving (Show)

emptyFilteredTransactions :: FilteredTransactions
emptyFilteredTransactions = FilteredTransactions [] [] [] [] [] [] []

-- |A group of one or more block items with sequential dependencies.
data TransactionGroup
  = TGAccountTransactions [TVer.TransactionWithStatus]
  -- ^A collection of transactions for a single account, ordered with non-decreasing nonce.
  | TGCredentialDeployment TVer.CredentialDeploymentWithStatus
  -- ^A single credential deployment.
  | TGUpdateInstructions [TVer.ChainUpdateWithStatus]
  -- ^A collection of update instructions of a single type, ordered with non-decreasing sequence number.

type GroupedTransactions = [TransactionGroup]

emptyGroupedTransactions :: GroupedTransactions
emptyGroupedTransactions = []

fromTransactions :: [[TVer.TransactionWithStatus]] -> GroupedTransactions
fromTransactions = fmap TGAccountTransactions

perAccountTransactions :: GroupedTransactions -> [[TVer.TransactionWithStatus]]
perAccountTransactions gts = [ats | TGAccountTransactions ats <- gts]

credentialDeployments :: GroupedTransactions -> [TVer.CredentialDeploymentWithStatus]
credentialDeployments gts = [cd | TGCredentialDeployment cd <- gts]
