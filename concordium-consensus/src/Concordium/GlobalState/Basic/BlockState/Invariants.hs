{-# LANGUAGE ViewPatterns, BangPatterns, TypeFamilies #-}
-- |This module defines global state invariants.
-- These are intended for testing purposes, but could also be used for
-- auditing.
module Concordium.GlobalState.Basic.BlockState.Invariants where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Lens.Micro.Platform
import Concordium.Utils

import qualified Concordium.ID.Types as ID
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.Bakers
import Concordium.GlobalState.Basic.BlockState.Account
import qualified Concordium.GlobalState.Basic.BlockState.Accounts as Account
import qualified Concordium.GlobalState.Basic.BlockState.AccountTable as AT
import Concordium.GlobalState.Basic.BlockState.Instances as Instances
import qualified Concordium.GlobalState.Rewards as Rewards

import qualified Concordium.GlobalState.Basic.BlockState.LFMBTree as L
import Concordium.Types
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule

checkBinary :: (Show a, Show b) => (a -> b -> Bool) -> a -> b -> String -> String -> String -> Either String ()
checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"

invariantBlockState :: BlockState -> Either String ()
invariantBlockState bs = do
        (creds, amp, totalBalance, delegationMap, ninstances) <- foldM checkAccount (Set.empty, Map.empty, 0, Map.empty, 0) (AT.toList $ Account.accountTable $ bs ^. blockAccounts)
        checkBinary (==) ninstances (instanceCount $ bs ^. blockInstances) "==" "accounted for instances" "all instances"
        totalStake <- foldM (checkBaker delegationMap) 0 ([(i, x) | (i, Just x) <- L.toAscPairList $ bs ^. blockBirkParameters . birkCurrentBakers . bakerMap])
        checkBinary (==) totalStake (bs ^. blockBirkParameters . birkCurrentBakers . bakerTotalStake) "==" "total baker stake" "recorded amount"
        let untrackedRegIds = Set.difference creds (bs ^. blockAccounts . to Account.accountRegIds)
        unless (null untrackedRegIds) $ Left $ "Untracked account reg ids: " ++ show untrackedRegIds
        let
            tenc = bs ^. blockBank . unhashed . Rewards.totalEncryptedGTU
            tcb = bs ^. blockBank . unhashed . Rewards.centralBankGTU
            txc = bs ^. blockBank . unhashed . Rewards.executionCost
        checkBinary (==)
            (totalBalance + tenc + tcb + txc)
            (bs ^. blockBank . unhashed . Rewards.totalGTU)
            "=="
            ("Total account balances (" ++ show totalBalance ++
                ") + total encrypted (" ++ show tenc ++
                ") + central bank (" ++ show tcb ++
                ") + execution cost (" ++ show txc ++ ")")
            "Total GTU"
        checkBinary (==) amp (bs ^. blockAccounts . to Account.accountMap) "==" "computed account map" "recorded account map"
    where
        checkAccount (creds, amp, bal, delegMap, ninsts) (i, acct) = do
            let addr = acct ^. accountAddress
            -- check that we didn't already find this credential
            creds' <- foldM checkCred creds (acct ^. accountCredentials)
            -- check that we didn't already find this same account
            when (Map.member addr amp) $ Left $ "Duplicate account address: " ++ show (acct ^. accountAddress)
            let lockedBalance = acct ^. accountReleaseSchedule . totalLockedUpBalance
            -- check that the locked balance is the same as the sum of the pending releases
            unless (lockedBalance == sum (acct ^. accountReleaseSchedule . pendingReleases)) $ Left "Total locked balance doesn't sum up to the pending releases stake"
            -- check that the instances exist and add their amounts to my balance
            !myBal <- foldM (checkInst addr) (acct ^. accountAmount) (acct ^. accountInstances)
            -- construct a baker map with the information from the accounts
            let delegMap' = maybe delegMap (\delBkr -> delegMap & at' delBkr . non 0 %~ (+ myBal)) (acct ^. accountStakeDelegate)
            return (creds', Map.insert addr i amp, bal + myBal, delegMap', ninsts + fromIntegral (Set.size (acct ^. accountInstances)))
        checkCred creds (ID.regId -> cred)
            | cred `Set.member` creds = Left $ "Duplicate credential: " ++ show cred
            | otherwise = return $ Set.insert cred creds
        checkInst owner bal caddr = case bs ^? blockInstances . ix caddr of
            Nothing -> Left $ "Missing smart contract instance: " ++ show caddr
            Just inst -> do
                checkBinary (==) owner (instanceOwner $ instanceParameters inst) "==" "account claiming contract ownership" "contract's indicated owner"
                -- TODO: Add more instance invariant checking
                return $ bal + (instanceAmount inst)
        checkBaker deleg stake (bid, binfo) = do
            checkBinary (==) (binfo ^. bakerStake) (deleg ^. at' bid . non 0) "==" "baker's stake" "amount delegated to baker"
            return $ stake + (binfo ^. bakerStake)
