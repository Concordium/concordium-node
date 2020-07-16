module Concordium.Scheduler.Types (module Concordium.Scheduler.Types,
                                   module Concordium.Types,
                                   module Concordium.Types.Execution,
                                   module Concordium.Types.Transactions,
                                   module Concordium.GlobalState.Instance,
                                   module Concordium.GlobalState.Rewards,
                                   module Concordium.GlobalState.Parameters,
                                   module Concordium.GlobalState.IdentityProviders,
                                   module Concordium.GlobalState.AnonymityRevokers,
                                   IdentityProviderIdentity) where

import Concordium.Types
import Concordium.Types.Execution
import Concordium.GlobalState.Instance
import Concordium.GlobalState.Rewards
import Concordium.Types.Transactions
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.AnonymityRevokers

import Concordium.ID.Types(IdentityProviderIdentity)

dummyChainMeta :: ChainMetadata
dummyChainMeta = ChainMetadata { slotNumber = 0
                               , blockHeight = 0
                               , finalizedHeight = 0
                               , slotTime = 0
                               }

-- |Result of constructing a block from 'GroupedTransactions'.
data FilteredTransactions = FilteredTransactions {
  -- |Transactions which have been added to the block, in the order added, with results.
  ftAdded :: [(BlockItem, TransactionSummary)],
  -- |Transactions which failed. No order is guaranteed.
  ftFailed :: [(Transaction, FailureKind)],
  -- |Credential deployments which failed. No order is guaranteed.
  ftFailedCredentials :: [(CredentialDeploymentWithMeta, FailureKind)],
  -- |Transactions which were not processed. No order is guaranteed.
  ftUnprocessed :: [Transaction],
  -- |Credentials which were not processed. No order is guaranteed.
  ftUnprocessedCredentials :: [CredentialDeploymentWithMeta]
  }
  deriving (Show)

emptyFilteredTransactions :: FilteredTransactions
emptyFilteredTransactions = FilteredTransactions [] [] [] [] []

-- |Transactions grouped by accounts.
-- For example, if T1 and T2 are transactions from account A,
-- and T3, T4, and T5 are transactions from account B, then
-- we group the transactions as [[T1, T2], [T3, T4, T5]].
-- Additionally a list of pending credentials to deploy together
data GroupedTransactions msg = GroupedTransactions{
  perAccountTransactions :: [[msg]],
  credentialDeployments :: [CredentialDeploymentWithMeta]
  }

emptyGroupedTransactions :: GroupedTransactions msg
emptyGroupedTransactions = GroupedTransactions [] []

fromTransactions :: [[msg]] -> GroupedTransactions msg
fromTransactions = flip GroupedTransactions []
