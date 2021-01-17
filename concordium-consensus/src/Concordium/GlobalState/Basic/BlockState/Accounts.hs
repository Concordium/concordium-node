{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Concordium.GlobalState.Basic.BlockState.Accounts where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro.Platform
import Lens.Micro.Internal (Ixed,Index,IxValue)
import Concordium.Utils
import Concordium.Types
import Concordium.GlobalState.Basic.BlockState.Account
import qualified Concordium.GlobalState.Basic.BlockState.AccountTable as AT
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
import Concordium.Types.HashableTo
import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.ID.Types as ID
import Data.Foldable

-- |Representation of the set of accounts on the chain.
-- Each account has an 'AccountIndex' which is the order
-- in which it was created.
--
-- The operations on 'Accounts', when used correctly, maintain the following invariants:
--
-- * Every @(address, index)@ pair in 'accountMap' has a corresponding account
--   in 'accountTable' with the given index and address.
-- * Every @(index, account)@ pair in 'accountTable' has a corresponding entry
--   in 'accountMap', which maps the account address to @index@.
-- * The 'accountMap' only ever increases: no accounts are removed, and account
--   indexes do not change.
-- * 'accountRegIds' only ever increases.
--
-- Note that the operations *do not* enforce a correspondence between 'accountRegIds'
-- and the credentials used by the accounts in 'accountTable'.
-- The data integrity of accounts is also not enforced by these operations.
data Accounts = Accounts {
    -- |Unique index of accounts by 'AccountAddress'
    accountMap :: !(Map.Map AccountAddress AccountIndex),
    -- |Hashed Merkle-tree of the accounts.
    accountTable :: !AT.AccountTable,
    -- |Set of 'ID.CredentialRegistrationID's that have been used for accounts.
    accountRegIds :: !(Set.Set ID.CredentialRegistrationID)
}

instance Show Accounts where
    show Accounts{..} = "Accounts {\n" ++ Map.foldMapWithKey showAcct accountMap ++ "accountRegIds = " ++ show accountRegIds ++ "\n}"
        where
            showAcct addr ind = show addr ++ " => " ++ maybe "MISSING" show (accountTable ^? ix ind) ++ "\n"

-- |An 'Accounts' with no accounts.
emptyAccounts :: Accounts
emptyAccounts = Accounts Map.empty AT.Empty Set.empty

-- |Add or modify a given account.
-- If an account matching the given account's address does not exist,
-- the account is created, giving it the next available account index
-- and recording it in 'accountMap'.
-- If an account with the address already exists, 'accountTable' is updated
-- to reflect the new state of the account.
putAccount :: Account -> Accounts -> Accounts
putAccount !acct Accounts{..} =
  case Map.lookup addr accountMap of
    Nothing -> let (i, newAccountTable) = AT.append acct accountTable
               in Accounts (Map.insert addr i accountMap) newAccountTable accountRegIds
    Just i -> Accounts accountMap (accountTable & ix i .~ acct) accountRegIds

  where addr = acct ^. accountAddress

-- |Equivalent to calling putAccount and recordRegId in sequence.
putAccountWithRegIds :: Account -> Accounts -> Accounts
putAccountWithRegIds !acct accts =
  foldl' (\accs currentAcc -> recordRegId (ID.regId currentAcc) accs) (putAccount acct accts) (acct ^. accountCredentials)

-- |Determine if an account with the given address exists.
exists :: AccountAddress -> Accounts -> Bool
exists addr Accounts{..} = Map.member addr accountMap

-- |Retrieve an account with the given address.
-- Returns @Nothing@ if no such account exists.
getAccount :: AccountAddress -> Accounts -> Maybe Account
getAccount addr Accounts{..} = case Map.lookup addr accountMap of
                                 Nothing -> Nothing
                                 Just i -> accountTable ^? ix i

getAccountIndex :: AccountAddress -> Accounts -> Maybe AccountIndex
getAccountIndex addr Accounts{..} = Map.lookup addr accountMap

-- |Retrieve an account and its index with the given address.
-- Returns @Nothing@ if no such account exists.
getAccountWithIndex :: AccountAddress -> Accounts -> Maybe (AccountIndex, Account)
getAccountWithIndex addr Accounts{..} = case Map.lookup addr accountMap of
                                 Nothing -> Nothing
                                 Just i -> (i, ) <$> accountTable ^? ix i

-- |Traversal for accessing the account at a given index.
indexedAccount :: AccountIndex -> Traversal' Accounts Account
indexedAccount ai = lens accountTable (\a v-> a{accountTable = v}) . ix ai

-- |Apply account updates to an account. It is assumed that the address in
-- account updates and account are the same.
updateAccount :: AccountUpdate -> Account -> Account
updateAccount !upd
    = updateNonce
      . updateReleaseSchedule
      . updateAmount
      . updateCredential (upd ^. auCredential)
      . updateEncryptedAmount
      . updateAccountKeys (upd ^. auKeysUpdate) (upd ^. auSignThreshold)
  where
    maybeUpdate :: Maybe a -> (a -> b -> b) -> b -> b
    maybeUpdate Nothing _ = id
    maybeUpdate (Just x) f = f x
    updateReleaseSchedule = maybeUpdate (upd ^. auReleaseSchedule) (\d acc -> acc & accountReleaseSchedule %~ (flip (foldl' (flip addReleases))) d
                                                                                 -- the amount that is scheduled is also added to the account amount
                                                                                 & accountAmount +~ foldl' (+) 0 (concatMap (\(l,_) -> map snd l) d))
    updateNonce = maybeUpdate (upd ^. auNonce) (accountNonce .~)
    updateAmount = maybeUpdate (upd ^. auAmount) $ \d -> accountAmount %~ applyAmountDelta d
    updateEncryptedAmount acc = foldr updateSingle acc (upd ^. auEncrypted)
    updateSingle AddSelf{..} = accountEncryptedAmount %~ addToSelfEncryptedAmount newAmount
    updateSingle Add{..} = accountEncryptedAmount %~ addIncomingEncryptedAmount newAmount
    updateSingle ReplaceUpTo{..} = accountEncryptedAmount %~ replaceUpTo aggIndex newAmount

-- |Retrieve an account with the given address.
-- An account with the address is required to exist.
unsafeGetAccount :: AccountAddress -> Accounts -> Account
unsafeGetAccount addr Accounts{..} = case Map.lookup addr accountMap of
                                       Nothing -> error $ "unsafeGetAccount: Account " ++ show addr ++ " does not exist."
                                       Just i -> accountTable ^?! ix i

-- |Check that an account registration ID is not already on the chain.
-- See the foundation (Section 4.2) for why this is necessary.
-- Return @True@ if the registration ID already exists in the set of known registration ids, and @False@ otherwise.
regIdExists :: ID.CredentialRegistrationID -> Accounts -> Bool
regIdExists rid Accounts{..} = rid `Set.member` accountRegIds

-- |Record an account registration ID as used.
recordRegId :: ID.CredentialRegistrationID -> Accounts -> Accounts
recordRegId rid accs = accs { accountRegIds = Set.insert rid (accountRegIds accs) }

instance HashableTo H.Hash Accounts where
    getHash Accounts{..} = getHash accountTable

type instance Index Accounts = AccountAddress
type instance IxValue Accounts = Account

instance Ixed Accounts where
  ix addr f acc@(Accounts{..}) =
     case accountMap ^. at' addr of
       Nothing -> pure acc
       Just i -> (\atable -> acc { accountTable = atable }) <$> ix i f accountTable
