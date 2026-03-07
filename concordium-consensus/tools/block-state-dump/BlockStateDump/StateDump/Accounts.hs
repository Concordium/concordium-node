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

dumpAccounts ::
    forall pv m.
    (BS.SupportsPersistentState pv m) =>
    OutputFiles ->
    NodeId ->
    Account.Accounts pv ->
    m ()
dumpAccounts output parentNode accounts = do
    LFMBDump.dumpLFMBTree output "accounttable" parentNode (Account.accountTable accounts) $ \accountLeafNode accountRef -> do
        accountForAV <- Blob.refLoad accountRef
        accountAddress <- Account.accountCanonicalAddress accountForAV

        visitHCRNode output accountLeafNode "" (show $ take 6 $ show accountAddress) accountRef $ \_accountNode accountBlobRef accountHash -> do
            account' <-
                PersistentAccount'
                    <$> Account.accountNonce accountForAV
                    <*> Account.accountAmount accountForAV
            buildStateData output (coerce accountBlobRef) accountHash account'


data PersistentAccount' av = PersistentAccount'
    { accountNonce :: !Nonce,
      accountAmount :: !Amount
      --   accountTokenStateTable :: !(Cond.Conditionally (SupportsPLT av) (Blob.Nullable (Blob.EagerlyHashedBufferedRef' Account.TokenStateTableHash AccountTokens.TokenAccountStateTable)))
    }
    deriving (Show)

-- todo ar add PersistentAccountEnduringData and persistentaccountdata 