{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns, BangPatterns, RecordWildCards, MultiParamTypeClasses #-}
module Concordium.GlobalState.Account where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Lens.Micro.Platform
import Lens.Micro.Internal (Ixed,Index,IxValue)

import Concordium.Types
import qualified Concordium.GlobalState.AccountTable as AT
import Concordium.GlobalState.AccountTable (AccountIndex, AccountTable(Empty))
import Concordium.Types.HashableTo
import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.ID.Types as ID

import qualified Data.List as List

data Accounts = Accounts {
    accountMap :: !(Map.Map AccountAddress AccountIndex),
    accountTable :: !AccountTable,
    accountRegIds :: !(Set.Set ID.CredentialRegistrationID)
}

emptyAccounts :: Accounts
emptyAccounts = Accounts Map.empty Empty Set.empty

putAccount :: Account -> Accounts -> Accounts
putAccount !acct Accounts{..} =
  case Map.lookup addr accountMap of
    Nothing -> let (i, newAccountTable) = AT.append acct accountTable
               in Accounts (Map.insert addr i accountMap) newAccountTable accountRegIds'
    Just i -> Accounts accountMap (accountTable & ix i .~ acct) accountRegIds'

  where addr = acct ^. accountAddress
        accountRegIds' = List.foldl' (flip Set.insert) accountRegIds (map ID.cdi_regId (acct ^. accountCredentials))

exists :: AccountAddress -> Accounts -> Bool
exists addr Accounts{..} = Map.member addr accountMap

getAccount :: AccountAddress -> Accounts -> Maybe Account
getAccount addr Accounts{..} = case Map.lookup addr accountMap of
                                 Nothing -> Nothing
                                 Just i -> accountTable ^? ix i

unsafeGetAccount :: AccountAddress -> Accounts -> Account
unsafeGetAccount addr Accounts{..} = case Map.lookup addr accountMap of
                                       Nothing -> error $ "unsafeGetAccount: Account " ++ show addr ++ " does not exist."
                                       Just i -> accountTable ^?! ix i

-- |Check that an account registration ID is not already on the chain.
-- See the foundation (Section 4.2) for why this is necessary.
-- Return @True@ if the registration ID already exists in the set of known registration ids, and @False@ otherwise.
regIdExists :: ID.CredentialRegistrationID -> Accounts -> Bool
regIdExists rid Accounts{..} = rid `Set.member` accountRegIds

recordRegId :: ID.CredentialRegistrationID -> Accounts -> Accounts
recordRegId rid accs = accs { accountRegIds = Set.insert rid (accountRegIds accs) }

instance HashableTo H.Hash Accounts where
    getHash Accounts{..} = getHash accountTable

type instance Index Accounts = AccountAddress
type instance IxValue Accounts = Account

instance Ixed Accounts where
  ix addr f acc@(Accounts{..}) =
     case accountMap ^. at addr of
       Nothing -> pure acc
       Just i -> (\atable -> acc { accountTable = atable }) <$> ix i f accountTable
