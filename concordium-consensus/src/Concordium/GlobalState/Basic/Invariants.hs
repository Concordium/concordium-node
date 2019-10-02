{-# LANGUAGE ViewPatterns, BangPatterns, TypeFamilies #-}
-- |This module defines global state invariants.
-- These are intended for testing purposes, but could also be used for
-- auditing.
module Concordium.GlobalState.Basic.Invariants where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Monad
import Lens.Micro.Platform

import qualified Concordium.ID.Types as ID
import Concordium.Types
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Bakers
import qualified Concordium.GlobalState.Account as Account
import qualified Concordium.GlobalState.AccountTable as AT
import Concordium.GlobalState.Instances as Instances
import qualified Concordium.GlobalState.Rewards as Rewards

checkBinary :: (Show a, Show b) => (a -> b -> Bool) -> a -> b -> String -> String -> String -> Either String ()
checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"

invariantBlockState :: BlockState -> Either String ()
invariantBlockState bs = do
        (creds, amp, totalBalance, delegationMap, ninstances) <- foldM checkAccount (Set.empty, Map.empty, 0, Map.empty, 0) (AT.toList $ Account.accountTable $ bs ^. blockAccounts)
        checkBinary (==) ninstances (instanceCount $ bs ^. blockInstances) "==" "accounted for instances" "all instances"
        totalStake <- foldM (checkBaker delegationMap) 0 (Map.toList $ bs ^. blockBirkParameters . birkCurrentBakers . bakerMap)
        checkBinary (==) totalStake (bs ^. blockBirkParameters . birkCurrentBakers . bakerTotalStake) "==" "total baker stake" "recorded amount"
        let untrackedRegIds = Set.difference creds (bs ^. blockAccounts . to Account.accountRegIds)
        unless (null untrackedRegIds) $ Left $ "Untracked account reg ids: " ++ show untrackedRegIds
        let
            tenc = bs ^. blockBank . Rewards.totalEncryptedGTU
            tcb = bs ^. blockBank . Rewards.centralBankGTU
            txc = bs ^. blockBank . Rewards.executionCost
        checkBinary (==) 
            (totalBalance + tenc + tcb + txc) 
            (bs ^. blockBank . Rewards.totalGTU) 
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
            creds' <- foldM checkCred creds (acct ^. accountCredentials)
            when (Map.member addr amp) $ Left $ "Duplicate account address: " ++ show (acct ^. accountAddress)
            !myBal <- foldM (checkInst addr) (acct ^. accountAmount) (acct ^. accountInstances)
            let delegMap' = maybe delegMap (\delBkr -> delegMap & at delBkr . non 0 %~ (+myBal)) (acct ^. accountStakeDelegate)
            return (creds', Map.insert addr i amp, bal + myBal, delegMap', ninsts + fromIntegral (Set.size (acct ^. accountInstances)))
        checkCred creds (ID.cdvRegId -> cred)
            | cred `Set.member` creds = Left $ "Duplicate credential: " ++ show cred
            | otherwise = return $ Set.insert cred creds
        checkInst owner bal caddr  = case bs ^? blockInstances . ix caddr of
            Nothing -> Left $ "Missing smart contract instance: " ++ show caddr
            Just inst -> do
                checkBinary (==) owner (instanceOwner $ instanceParameters inst) "==" "account claiming contract ownership" "contract's indicated owner"
                -- TODO: Add more instance invariant checking
                return $ bal + (instanceAmount inst)
        checkBaker deleg stake (bid, binfo) = do
            checkBinary (==) (binfo ^. bakerStake) (deleg ^. at bid . non 0) "==" "baker's stake" "amount delegated to baker"
            return $ stake + (binfo ^. bakerStake)

