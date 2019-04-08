{-# LANGUAGE ViewPatterns, RecordWildCards, MultiParamTypeClasses #-}
module Concordium.GlobalState.Account where

import qualified Data.Map as Map
import Lens.Micro.Platform

import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.AccountTable as AT
import Concordium.GlobalState.AccountTable (AccountIndex, AccountTable)
import Concordium.GlobalState.HashableTo
import qualified Concordium.Crypto.SHA256 as H

data Accounts = Accounts {
    accountMap :: Map.Map AccountAddress AccountIndex,
    accountTable :: AccountTable
}

insert :: Account -> Accounts -> Accounts
insert acct Accounts{..} = case Map.lookup addr accountMap of
        Nothing -> let (i, newAccountTable) = AT.append acct accountTable in Accounts (Map.insert addr i accountMap) newAccountTable
        Just i -> Accounts accountMap (accountTable & ix i .~ acct)
    where
        addr = accountAddress acct

instance HashableTo H.Hash Accounts where
    getHash Accounts{..} = getHash accountTable

    