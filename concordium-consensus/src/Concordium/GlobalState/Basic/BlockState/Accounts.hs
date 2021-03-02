{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}
module Concordium.GlobalState.Basic.BlockState.Accounts where

import Data.Serialize
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro.Platform
import Lens.Micro.Internal (Ixed,Index,IxValue)
import Concordium.Utils
import Concordium.Types
import Concordium.GlobalState.Basic.BlockState.Account
import qualified Concordium.GlobalState.Basic.BlockState.AccountTable as AT
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
import Concordium.GlobalState.BakerInfo
import Concordium.Types.HashableTo
import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.ID.Types as ID
import Data.Foldable
import Concordium.ID.Parameters
import Control.Monad

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
    accountMap :: !(Map.Map AccountAddress AccountIndex),
    -- |Hashed Merkle-tree of the accounts.
    accountTable :: !(AT.AccountTable pv),
    -- |Set of 'ID.CredentialRegistrationID's that have been used for accounts.
    accountRegIds :: !(Set.Set ID.CredentialRegistrationID)
}

instance IsProtocolVersion pv => Show (Accounts pv) where
    show Accounts{..} = "Accounts {\n" ++ Map.foldMapWithKey showAcct accountMap ++ "accountRegIds = " ++ show accountRegIds ++ "\n}"
        where
            showAcct addr ind = show addr ++ " => " ++ maybe "MISSING" show (accountTable ^? ix ind) ++ "\n"

-- |An 'Accounts' with no accounts.
emptyAccounts :: Accounts pv
emptyAccounts = Accounts Map.empty AT.Empty Set.empty

-- |Add or modify a given account.
-- If an account matching the given account's address does not exist,
-- the account is created, giving it the next available account index
-- and recording it in 'accountMap'.
-- If an account with the address already exists, 'accountTable' is updated
-- to reflect the new state of the account.
putAccount :: IsProtocolVersion pv => Account pv -> Accounts pv -> Accounts pv
putAccount !acct Accounts{..} =
  case Map.lookup addr accountMap of
    Nothing -> let (i, newAccountTable) = AT.append acct accountTable
               in Accounts (Map.insert addr i accountMap) newAccountTable accountRegIds
    Just i -> Accounts accountMap (accountTable & ix i .~ acct) accountRegIds

  where addr = acct ^. accountAddress

-- |Equivalent to calling putAccount and recordRegId in sequence.
putAccountWithRegIds :: IsProtocolVersion pv => Account pv -> Accounts pv -> Accounts pv
putAccountWithRegIds !acct accts =
  foldl' (\accs currentAcc -> recordRegId (ID.regId currentAcc) accs) (putAccount acct accts) (acct ^. accountCredentials)

-- |Determine if an account with the given address exists.
exists :: AccountAddress -> Accounts pv -> Bool
exists addr Accounts{..} = Map.member addr accountMap

-- |Retrieve an account with the given address.
-- Returns @Nothing@ if no such account exists.
getAccount :: IsProtocolVersion pv => AccountAddress -> Accounts pv -> Maybe (Account pv)
getAccount addr Accounts{..} = case Map.lookup addr accountMap of
                                 Nothing -> Nothing
                                 Just i -> accountTable ^? ix i

getAccountIndex :: AccountAddress -> Accounts pv -> Maybe AccountIndex
getAccountIndex addr Accounts{..} = Map.lookup addr accountMap

-- |Retrieve an account and its index with the given address.
-- Returns @Nothing@ if no such account exists.
getAccountWithIndex :: IsProtocolVersion pv => AccountAddress -> Accounts pv -> Maybe (AccountIndex, Account pv)
getAccountWithIndex addr Accounts{..} = case Map.lookup addr accountMap of
                                 Nothing -> Nothing
                                 Just i -> (i, ) <$> accountTable ^? ix i

-- |Traversal for accessing the account at a given index.
indexedAccount :: IsProtocolVersion pv => AccountIndex -> Traversal' (Accounts pv) (Account pv)
indexedAccount ai = lens accountTable (\a v-> a{accountTable = v}) . ix ai

-- |Apply account updates to an account. It is assumed that the address in
-- account updates and account are the same.
updateAccount :: IsProtocolVersion pv => AccountUpdate -> Account pv -> Account pv
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
unsafeGetAccount :: IsProtocolVersion pv => AccountAddress -> Accounts pv -> Account pv
unsafeGetAccount addr Accounts{..} = case Map.lookup addr accountMap of
                                       Nothing -> error $ "unsafeGetAccount: Account " ++ show addr ++ " does not exist."
                                       Just i -> accountTable ^?! ix i

-- |Check that an account registration ID is not already on the chain.
-- See the foundation (Section 4.2) for why this is necessary.
-- Return @True@ if the registration ID already exists in the set of known registration ids, and @False@ otherwise.
regIdExists :: ID.CredentialRegistrationID -> Accounts pv -> Bool
regIdExists rid Accounts{..} = rid `Set.member` accountRegIds

-- |Record an account registration ID as used.
recordRegId :: ID.CredentialRegistrationID -> Accounts pv -> Accounts pv
recordRegId rid accs = accs { accountRegIds = Set.insert rid (accountRegIds accs) }

instance HashableTo H.Hash (Accounts pv) where
    getHash Accounts{..} = getHash accountTable

type instance Index (Accounts pv) = AccountAddress
type instance IxValue (Accounts pv) = (Account pv)

instance IsProtocolVersion pv => Ixed (Accounts pv) where
  ix addr f acc@(Accounts{..}) =
     case accountMap ^. at' addr of
       Nothing -> pure acc
       Just i -> (\atable -> acc { accountTable = atable }) <$> ix i f accountTable

accountList :: Accounts pv -> [Account pv]
accountList = fmap snd . AT.toList . accountTable

-- |Serialize 'Accounts' in V0 format.
putAccountsV0 :: IsProtocolVersion pv => GlobalContext -> Putter (Accounts pv)
putAccountsV0 cryptoParams Accounts{..} = do
    putWord64be $ AT.size accountTable
    forM_ (AT.toList accountTable) $ \(_, acct) -> putAccountV0 cryptoParams acct

-- |Deserialize 'Accounts' in V0 format.
-- This validates the following invariants:
--
--  * Every baker account's 'BakerId' must match the account index.
--  * 'CredentialRegistrationID's must not be used on more than one account.
getAccountsV0 :: IsProtocolVersion pv => GlobalContext -> Get (Accounts pv)
getAccountsV0 cryptoParams = do
    nAccounts <- getWord64be
    let loop i accts@Accounts{..}
          | i < nAccounts = do
            acct <- getAccountV0 cryptoParams
            let acctId = AccountIndex i
            forM_ (_accountBaker acct) $ \bkr ->
              unless (_bakerIdentity (_accountBakerInfo bkr) == BakerId acctId) $
                fail "BakerID does not match account index"
            let addRegId regids cred
                  | ID.regId cred `Set.member` regids = fail "Duplicate credential"
                  | otherwise = return $ Set.insert (ID.regId cred) regids
            newRegIds <- foldM addRegId accountRegIds (acct ^. accountCredentials)
            loop (i+1)
              Accounts {
                accountMap = Map.insert (acct ^. accountAddress) acctId accountMap,
                accountTable = snd (AT.append acct accountTable),
                accountRegIds = newRegIds
              }
          | otherwise = return accts
    loop 0 emptyAccounts
