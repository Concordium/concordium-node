{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module BlockStateDump.StateDump.Accounts where

import Data.Coerce

import Concordium.Types

import qualified BlockStateDump.StateDump.LFMBTree as LFMBDump
import qualified Concordium.GlobalState.Persistent.Account as Account
import qualified Concordium.GlobalState.Persistent.Accounts as Account
import qualified Concordium.GlobalState.Persistent.BlobStore as Blob
import qualified Concordium.GlobalState.Persistent.BlockState as BS

import BlockStateDump.Shared
import qualified Concordium.GlobalState.Account as Account
import qualified Concordium.GlobalState.Persistent.Account.StructureV0 as AccountV0
import qualified Concordium.GlobalState.Persistent.Account.StructureV1 as AccountV1

dumpAccounts ::
    forall pv m.
    (BS.SupportsPersistentState pv m) =>
    OutputFiles ->
    NodeId ->
    Account.Accounts pv ->
    m ()
dumpAccounts output parentNode accounts = do
    LFMBDump.dumpLFMBTree output "accounttbl" parentNode (Account.accountTable accounts) $ \accountLeafNode accountRef -> do
        accountForAV <- Blob.refLoad accountRef
        accountAddress <- Account.accountCanonicalAddress accountForAV

        visitHCRNode
            output
            accountLeafNode
            ""
            (show $ take 6 $ show accountAddress)
            accountRef
            $ \accountNode accountBlobRef accountHash -> do
                account' <-
                    PersistentAccount'
                        <$> Account.accountNonce accountForAV
                        <*> Account.accountAmount accountForAV
                buildStateData output (coerce accountBlobRef) accountHash account'

                case accountStructure accountForAV of
                    AccountStructureV0 _account -> error "AccountStructureV0 not implemented"
                    AccountStructureV1 account -> do
                        let accountEnduringRef = AccountV1.accountEnduringData account
                        accountEnduring <- Blob.refLoad accountEnduringRef
                        visitEBRNode @(Account.AccountMerkleHash (AccountVersionFor pv))
                            output
                            accountNode
                            ""
                            "enduring"
                            accountEnduringRef
                            $ \enduringNode _enduringBlobRef _enduringHash -> do
                                let accountPersistingRef = AccountV1.paedPersistingData accountEnduring
                                accountPersisting <- Blob.refLoad accountPersistingRef
                                visitEBRNode @Account.PersistingAccountDataHash
                                    output
                                    enduringNode
                                    ""
                                    "persisting"
                                    accountPersistingRef
                                    $ \_persistentNode persistentBlobRef persistentHash -> do
                                        buildStateData output (coerce persistentBlobRef) persistentHash accountPersisting

data PersistentAccount' av = PersistentAccount'
    { accountNonce :: !Nonce,
      accountAmount :: !Amount
      --   accountTokenStateTable :: !(Cond.Conditionally (SupportsPLT av) (Blob.Nullable (Blob.EagerlyHashedBufferedRef' Account.TokenStateTableHash AccountTokens.TokenAccountStateTable)))
    }
    deriving (Show)

data PersistentAccountStructure (av :: AccountVersion) where
    AccountStructureV0 :: !(AccountV0.PersistentAccount av) -> PersistentAccountStructure av
    AccountStructureV1 :: !(AccountV1.PersistentAccount av) -> PersistentAccountStructure av

accountStructure :: Account.PersistentAccount av -> PersistentAccountStructure av
accountStructure = \case
    Account.PAV0 account -> AccountStructureV0 account
    Account.PAV1 account -> AccountStructureV0 account
    Account.PAV2 account -> AccountStructureV1 account
    Account.PAV3 account -> AccountStructureV1 account
    Account.PAV4 account -> AccountStructureV1 account
    Account.PAV5 account -> AccountStructureV1 account
