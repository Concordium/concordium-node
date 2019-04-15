{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns, RecordWildCards, MultiParamTypeClasses #-}
module Concordium.GlobalState.Account where

import qualified Data.Map as Map
import Lens.Micro.Platform
import Lens.Micro.Internal (Ixed,Index,IxValue)

import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.AccountTable as AT
import Concordium.GlobalState.AccountTable (AccountIndex, AccountTable(Empty))
import Concordium.GlobalState.HashableTo
import qualified Concordium.Crypto.SHA256 as H

data Accounts = Accounts {
    accountMap :: Map.Map AccountAddress AccountIndex,
    accountTable :: AccountTable
}

emptyAccounts :: Accounts
emptyAccounts = Accounts Map.empty Empty

putAccount :: Account -> Accounts -> Accounts
putAccount acct Accounts{..} =
  case Map.lookup addr accountMap of
    Nothing -> let (i, newAccountTable) = AT.append acct accountTable in Accounts (Map.insert addr i accountMap) newAccountTable
    Just i -> Accounts accountMap (accountTable & ix i .~ acct)

  where addr = acct ^. accountAddress

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


instance HashableTo H.Hash Accounts where
    getHash Accounts{..} = getHash accountTable

type instance Index Accounts = AccountAddress
type instance IxValue Accounts = Account

instance Ixed Accounts where
  ix addr f acc@(Accounts{..}) =
     case accountMap ^. at addr of
       Nothing -> pure acc
       Just i -> (\atable -> acc { accountTable = atable }) <$> ix i f accountTable
