module Concordium.Scheduler.Types (module Concordium.Scheduler.Types,
                                   module Concordium.Types,
                                   module Concordium.Types.Updates,
                                   module Concordium.Types.Execution,
                                   module Concordium.Types.Transactions,
                                   module Concordium.Types.Instance,
                                   module Concordium.GlobalState.Rewards,
                                   module Concordium.GlobalState.Parameters,
                                   module Concordium.Types.IdentityProviders,
                                   module Concordium.Types.AnonymityRevokers,
                                   IdentityProviderIdentity) where

import Concordium.Types
import Concordium.Types.Updates
import Concordium.Types.Execution
import Concordium.Types.Instance
import Concordium.GlobalState.Rewards
import Concordium.Types.Transactions
import Concordium.GlobalState.Parameters
import Concordium.Types.IdentityProviders
import Concordium.Types.AnonymityRevokers

import Concordium.ID.Types(IdentityProviderIdentity)

-- |Result of constructing a block from 'GroupedTransactions'.
data FilteredTransactions = FilteredTransactions {
  -- |Transactions which have been added to the block, in the order added, with results.
  ftAdded :: [(BlockItem, TransactionSummary)],
  -- |Transactions which failed. No order is guaranteed.
  ftFailed :: [(Transaction, FailureKind)],
  -- |Credential deployments which failed. No order is guaranteed.
  ftFailedCredentials :: [(CredentialDeploymentWithMeta, FailureKind)],
  -- |Update instructions which failed. No order is guaranteed.
  ftFailedUpdates :: [(WithMetadata UpdateInstruction, FailureKind)],
  -- |Transactions which were not processed. No order is guaranteed.
  ftUnprocessed :: [Transaction],
  -- |Credentials which were not processed. No order is guaranteed.
  ftUnprocessedCredentials :: [CredentialDeploymentWithMeta],
  -- |Update instructions which were not processed. No order is guaranteed.
  ftUnprocessedUpdates :: [WithMetadata UpdateInstruction]
  }
  deriving (Show)

emptyFilteredTransactions :: FilteredTransactions
emptyFilteredTransactions = FilteredTransactions [] [] [] [] [] [] []

-- |A group of one or more block items with sequential dependencies.
data TransactionGroup
  = TGAccountTransactions [Transaction]
  -- ^A collection of transactions for a single account, ordered with non-decreasing nonce.
  | TGCredentialDeployment CredentialDeploymentWithMeta
  -- ^A single credential deployment.
  | TGUpdateInstructions [WithMetadata UpdateInstruction]
  -- ^A collection of update instructions of a single type, ordered with non-decreasing sequence number.

type GroupedTransactions = [TransactionGroup]

emptyGroupedTransactions :: GroupedTransactions
emptyGroupedTransactions = []

fromTransactions :: [[Transaction]] -> GroupedTransactions
fromTransactions = fmap TGAccountTransactions

perAccountTransactions :: GroupedTransactions -> [[Transaction]]
perAccountTransactions gts = [ats | TGAccountTransactions ats <- gts]

credentialDeployments :: GroupedTransactions -> [CredentialDeploymentWithMeta]
credentialDeployments gts = [cd | TGCredentialDeployment cd <- gts]