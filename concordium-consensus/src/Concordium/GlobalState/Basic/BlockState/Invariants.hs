{-# LANGUAGE ViewPatterns, TypeFamilies #-}
-- |This module defines global state invariants.
-- These are intended for testing purposes, but could also be used for
-- auditing.
module Concordium.GlobalState.Basic.BlockState.Invariants where

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Vector as Vec
import Control.Monad
import Lens.Micro.Platform

import qualified Concordium.ID.Types as ID
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.Bakers
import Concordium.GlobalState.Basic.BlockState.Account
import qualified Concordium.GlobalState.Basic.BlockState.Accounts as Account
import qualified Concordium.GlobalState.Basic.BlockState.AccountTable as AT
import Concordium.GlobalState.Basic.BlockState.Instances as Instances
import qualified Concordium.GlobalState.Rewards as Rewards

import Concordium.Types
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule

checkBinary :: (Show a, Show b) => (a -> b -> Bool) -> a -> b -> String -> String -> String -> Either String ()
checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"

invariantBlockState :: (IsProtocolVersion pv) => BlockState pv -> Amount -> Either String ()
invariantBlockState bs extraBalance = do
        (creds, amp, totalBalance, remainingIds, remainingKeys) <- foldM checkAccount (Map.empty, Map.empty, 0, bs ^. blockBirkParameters . birkActiveBakers . activeBakers, bs ^. blockBirkParameters . birkActiveBakers . aggregationKeys) (AT.toList $ Account.accountTable $ bs ^. blockAccounts)
        unless (Set.null remainingIds) $ Left $ "Active bakers with no baker record: " ++ show (Set.toList remainingIds)
        unless (Set.null remainingKeys) $ Left $ "Unaccounted for baker aggregation keys: " ++ show (Set.toList remainingKeys)
        instancesBalance <- foldM checkInstance 0 (bs ^.. blockInstances . foldInstances)
        checkEpochBakers (bs ^. blockBirkParameters . birkCurrentEpochBakers . unhashed)
        checkEpochBakers (bs ^. blockBirkParameters . birkNextEpochBakers . unhashed)
        checkCredentialResults creds (bs ^. blockAccounts . to Account.accountRegIds)
        let
            bank = bs ^. blockBank . unhashed
            tenc = bank ^. Rewards.totalEncryptedGTU
            brew = bank ^. Rewards.bakingRewardAccount
            frew = bank ^. Rewards.finalizationRewardAccount
            gas = bank ^. Rewards.gasAccount
        checkBinary (==)
            (totalBalance + instancesBalance + tenc + brew + frew + gas + extraBalance)
            (bank ^. Rewards.totalGTU)
            "=="
            ("Total account balances (" ++ show totalBalance ++
                ") + total instance balances (" ++ show instancesBalance ++
                ") + total encrypted (" ++ show tenc ++
                ") + baking reward account (" ++ show brew ++
                ") + finalization reward account (" ++ show frew ++
                ") + GAS account (" ++ show gas ++
                ") + extra balance (" ++ show extraBalance ++ ")")
            "Total GTU"
        checkBinary (==) amp (bs ^. blockAccounts . to Account.accountMap) "==" "computed account map" "recorded account map"
    where
        checkAccount (creds, amp, bal, bakerIds, bakerKeys) (i, acct) = do
            let addr = acct ^. accountAddress
            -- check that we didn't already find this credential
            creds' <- foldM (checkCred i) creds (acct ^. accountCredentials)
            -- check that we didn't already find this same account
            when (Map.member addr amp) $ Left $ "Duplicate account address: " ++ show (acct ^. accountAddress)
            let lockedBalance = acct ^. accountReleaseSchedule . totalLockedUpBalance
            -- check that the locked balance is the same as the sum of the pending releases
            unless (lockedBalance == sum [ am | Just (r, _) <- Vec.toList (acct ^. accountReleaseSchedule . values), Release _ am <- r ]) $ Left "Total locked balance doesn't sum up to the pending releases stake"
            (bakerIds', bakerKeys') <- case acct ^. accountBaker of
                    Nothing -> return (bakerIds, bakerKeys)
                    Just abkr -> do
                        let binfo = abkr ^. accountBakerInfo
                        checkBinary (==) (binfo ^. bakerIdentity) (BakerId i) "==" "account baker id" "account index"
                        unless (Set.member (BakerId i) bakerIds) $ Left "Account has baker record, but is not an active baker"
                        unless (Set.member (binfo ^. bakerAggregationVerifyKey) bakerKeys) $ Left "Baker aggregation key is missing from active bakers"
                        return (Set.delete (BakerId i) bakerIds, Set.delete (binfo ^. bakerAggregationVerifyKey) bakerKeys)
            return (creds', Map.insert addr i amp, bal + (acct ^. accountAmount), bakerIds', bakerKeys')
        checkCred i creds (ID.credId -> cred)
            | cred `Map.member` creds = Left $ "Duplicate credential: " ++ show cred
            | otherwise = return $ Map.insert cred i creds
        checkEpochBakers EpochBakers{..} = do
            checkBinary (==) (Vec.length _bakerInfos) (Vec.length _bakerStakes) "==" "#baker infos" "#baker stakes"
            checkBinary (==) _bakerTotalStake (sum _bakerStakes) "==" "baker total stake" "sum of baker stakes"
        checkInstance amount Instance{..} = return $! (amount + instanceAmount)

        -- check that the two credential maps are the same, the one recorded in block state and the model one.
        checkCredentialResults modelCreds actualCreds = do
          let untrackedRegIds = Map.difference modelCreds actualCreds
          unless (null untrackedRegIds) $ Left $ "Untracked account reg ids: " ++ show untrackedRegIds
          -- now also check that all the indices are correct.
          -- at this point we know that all the cred ids are the same
          forM_ (Map.toList modelCreds) $ \(cid, ai) -> do
            case Map.lookup cid actualCreds of
              Nothing -> Left $ "Untracked reg id: " ++ show cid
              Just ai' | ai /= ai' -> Left $ "Reg id account indices differ: " ++ show ai' ++ " /= " ++ show ai
                       | otherwise -> return ()
