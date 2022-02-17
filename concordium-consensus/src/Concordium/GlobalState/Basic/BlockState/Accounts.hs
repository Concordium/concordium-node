{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Concordium.GlobalState.Basic.BlockState.Accounts where

import Data.Serialize
import qualified Data.Map.Strict as Map
import GHC.Stack (HasCallStack)
import Lens.Micro.Platform
import Lens.Micro.Internal (Ixed,Index,IxValue)
import Concordium.Types
import Concordium.Types.Accounts
import Concordium.GlobalState.Basic.BlockState.Account
import qualified Concordium.GlobalState.AccountMap as AccountMap
import qualified Concordium.GlobalState.Basic.BlockState.AccountTable as AT
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
import Concordium.Types.HashableTo
import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.ID.Types as ID
import Data.Foldable
import Concordium.ID.Parameters
import Control.Monad
import Concordium.Genesis.Data

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
data Accounts (pv :: ProtocolVersion) = Accounts {
    -- |Unique index of accounts by 'AccountAddress'
    accountMap :: !(AccountMap.PureAccountMap pv),
    -- |Hashed Merkle-tree of the accounts.
    accountTable :: !(AT.AccountTable (AccountVersionFor pv)),
    -- |A mapping of 'ID.CredentialRegistrationID's to accounts on which they are used.
    accountRegIds :: !(Map.Map ID.CredentialRegistrationID AccountIndex)
}

instance IsProtocolVersion pv => Show (Accounts pv) where
    show Accounts{..} = "Accounts {\n" ++ (concatMap showAcct . AccountMap.toListPure $ accountMap) ++ "accountRegIds = " ++ show accountRegIds ++ "\n}"
        where
            showAcct (addr, ind) = show addr ++ " => " ++ maybe "MISSING" show (accountTable ^? ix ind) ++ "\n"

-- |An 'Accounts' with no accounts.
emptyAccounts :: Accounts pv
emptyAccounts = Accounts AccountMap.empty AT.Empty Map.empty

-- |Add or modify a given account.
-- If an account matching the given account's address does not exist,
-- the account is created, giving it the next available account index
-- and recording it in 'accountMap'.
-- If an account with the address already exists, 'accountTable' is updated
-- to reflect the new state of the account.
putAccount :: IsProtocolVersion pv => Account (AccountVersionFor pv) -> Accounts pv -> Accounts pv
putAccount !acct = snd . putAccountWithIndex acct

-- |Add or modify a given account.
-- If an account matching the given account's address does not exist,
-- the account is created, giving it the next available account index,
-- recording it in 'accountMap', and returning it.
-- If an account with the address already exists, 'accountTable' is updated
-- to reflect the new state of the account, and the index of the account is returned.
putAccountWithIndex :: IsProtocolVersion pv => Account (AccountVersionFor pv) -> Accounts pv -> (AccountIndex, Accounts pv)
putAccountWithIndex !acct Accounts{..} =
  case AccountMap.lookupPure addr accountMap of
    Nothing -> let (i, newAccountTable) = AT.append acct accountTable
               in (i, Accounts (AccountMap.insertPure addr i accountMap) newAccountTable accountRegIds)
    Just i -> (i, Accounts accountMap (accountTable & ix i .~ acct) accountRegIds)

  where addr = acct ^. accountAddress

-- |Add a new account. Returns @Just idx@ if the new account is fresh, i.e., the address does not exist,
-- or @Nothing@ in case the account already exists. In the latter case there is no change to the accounts structure.
putNewAccount :: IsProtocolVersion pv => Account (AccountVersionFor pv) -> Accounts pv -> (Maybe AccountIndex, Accounts pv)
putNewAccount acct accts = 
  case AccountMap.lookupPure (acct ^. accountAddress) (accountMap accts) of
    Just _ -> (Nothing, putAccount acct accts)
    Nothing -> let (ai, accts') = putAccountWithIndex acct accts in (Just ai, accts')

-- |Equivalent to calling putAccount and recordRegId in sequence.
putAccountWithRegIds :: IsProtocolVersion pv => Account (AccountVersionFor pv) -> Accounts pv -> Accounts pv
putAccountWithRegIds !acct accts =
  foldl' (\accs currentCred -> recordRegId (ID.credId currentCred) idx accs) initialAccts (acct ^. accountCredentials)
  where (idx, initialAccts) = putAccountWithIndex acct accts

-- |Determine if an account with the given address exists.
exists :: IsProtocolVersion pv => AccountAddress -> Accounts pv -> Bool
exists addr Accounts{..} = AccountMap.isAddressAssignedPure addr accountMap

-- |Retrieve an account with the given address.
-- Returns @Nothing@ if no such account exists.
getAccount :: IsProtocolVersion pv => AccountAddress -> Accounts pv -> Maybe (Account (AccountVersionFor pv))
getAccount addr Accounts{..} = case AccountMap.lookupPure addr accountMap of
                                 Nothing -> Nothing
                                 Just i -> accountTable ^? ix i

getAccountIndex :: IsProtocolVersion pv => AccountAddress -> Accounts pv -> Maybe AccountIndex
getAccountIndex addr Accounts{..} = AccountMap.lookupPure addr accountMap

-- |Retrieve an account and its index with the given address.
-- Returns @Nothing@ if no such account exists.
getAccountWithIndex :: IsProtocolVersion pv => AccountAddress -> Accounts pv -> Maybe (AccountIndex, Account (AccountVersionFor pv))
getAccountWithIndex addr Accounts{..} = case AccountMap.lookupPure addr accountMap of
                                 Nothing -> Nothing
                                 Just i -> (i, ) <$> accountTable ^? ix i

-- |Traversal for accessing the account at a given index.
indexedAccount :: IsProtocolVersion pv => AccountIndex -> Traversal' (Accounts pv) (Account (AccountVersionFor pv))
indexedAccount ai = lens accountTable (\a v-> a{accountTable = v}) . ix ai

-- |Lens for accessing the account at a given index, assuming the account exists.
-- If the account does not exist, this throws an error.
unsafeIndexedAccount :: (HasCallStack, IsProtocolVersion pv) => AccountIndex -> Lens' (Accounts pv) (Account (AccountVersionFor pv))
unsafeIndexedAccount ai = singular (indexedAccount ai)

-- |Apply account updates to an account. It is assumed that the address in
-- account updates and account are the same.
updateAccount :: AccountUpdate -> Account av -> Account av
updateAccount !upd
    = updateNonce
      . updateReleaseSchedule
      . updateAmount
      . updateEncryptedAmount
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
unsafeGetAccount :: IsProtocolVersion pv => AccountAddress -> Accounts pv -> Account (AccountVersionFor pv)
unsafeGetAccount addr Accounts{..} = case AccountMap.lookupPure addr accountMap of
                                       Nothing -> error $ "unsafeGetAccount: Account " ++ show addr ++ " does not exist."
                                       Just i -> accountTable ^?! ix i

-- |Check whether the given address would clash with any existing addresses in
-- the accounts structure. The meaning of this depends on the protocol version.
addressWouldClash :: IsProtocolVersion pv => AccountAddress -> Accounts pv -> Bool
addressWouldClash addr Accounts{..} = AccountMap.addressWouldClashPure addr accountMap

-- |Check that an account registration ID is not already on the chain. See the
-- foundation (Section 4.2) for why this is necessary. Return @Just ai@ if the
-- registration ID already exists in the set of known registration ids, and @ai@
-- is the account index of the account is or was associated with, and @Nothing@
-- otherwise.
regIdExists :: ID.CredentialRegistrationID -> Accounts pv -> Maybe AccountIndex
regIdExists rid Accounts{..} = rid `Map.lookup` accountRegIds

-- |Record an account registration ID as used on the account.
recordRegId :: ID.CredentialRegistrationID -> AccountIndex -> Accounts pv -> Accounts pv
recordRegId rid idx accs = accs { accountRegIds = Map.insert rid idx (accountRegIds accs) }

-- |Record multiple registration ids as used. This implementation is marginally
-- more efficient than repeatedly calling `recordRegId`.
recordRegIds :: [(ID.CredentialRegistrationID, AccountIndex)] -> Accounts pv -> Accounts pv
recordRegIds rids accs = accs { accountRegIds = Map.union (accountRegIds accs) (Map.fromAscList rids) }
    -- since credentials can only be used on one account the union is well-defined, the maps should be disjoint.

instance HashableTo H.Hash (Accounts pv) where
    getHash Accounts{..} = getHash accountTable

type instance Index (Accounts pv) = AccountAddress
type instance IxValue (Accounts pv) = (Account (AccountVersionFor pv))

instance IsProtocolVersion pv => Ixed (Accounts pv) where
  ix addr f acc@Accounts{..} =
     case AccountMap.lookupPure addr accountMap of
       Nothing -> pure acc
       Just i -> (\atable -> acc { accountTable = atable }) <$> ix i f accountTable

-- |Convert an 'Accounts' to a list of 'Account's.
accountList :: Accounts pv -> [Account (AccountVersionFor pv)]
accountList = fmap snd . AT.toList . accountTable

-- |Serialize 'Accounts' in V0 format.
serializeAccounts :: IsProtocolVersion pv => GlobalContext -> Putter (Accounts pv)
serializeAccounts cryptoParams Accounts{..} = do
    putWord64be $ AT.size accountTable
    forM_ (AT.toList accountTable) $ \(_, acct) -> serializeAccount cryptoParams acct

-- |Deserialize 'Accounts'. The serialization format may depend on the protocol version.
-- This validates the following invariants:
--
--  * Every baker account's 'BakerId' must match the account index.
--  * 'CredentialRegistrationID's must not be used on more than one account.
deserializeAccounts :: (IsProtocolVersion oldpv, IsProtocolVersion pv) => StateMigrationParameters oldpv pv -> GlobalContext -> Get (Accounts pv)
deserializeAccounts migration cryptoParams = do
    nAccounts <- getWord64be
    let loop i accts@Accounts{..}
          | i < nAccounts = do
            acct <- deserializeAccount migration cryptoParams
            let acctId = AccountIndex i
            case _accountStaking acct of
              AccountStakeBaker bkr ->
                unless (bkr ^. accountBakerInfo . bakerIdentity == BakerId acctId) $
                  fail "BakerID does not match account index"
              _ -> return ()              
            let addRegId regids cred
                  | cred `Map.member` regids = fail "Duplicate credential"
                  | otherwise = return $ Map.insert cred acctId regids
            newRegIds <- foldM addRegId accountRegIds $
                (ID.credId <$> Map.elems (acct ^. accountCredentials))
                ++ removedCredentialsToList (acct ^. accountRemovedCredentials . unhashed)
            loop (i+1)
              Accounts {
                accountMap = AccountMap.insertPure (acct ^. accountAddress) acctId accountMap,
                accountTable = snd (AT.append acct accountTable),
                accountRegIds = newRegIds
              }
          | otherwise = return accts
    loop 0 emptyAccounts
