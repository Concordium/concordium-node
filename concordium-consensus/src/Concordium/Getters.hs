{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
module Concordium.Getters where

import Lens.Micro.Platform hiding ((.=))

-- import Concordium.Payload.Transaction
--import Concordium.Types as T
import Concordium.MonadImplementation
import Concordium.Kontrol.BestBlock
import Concordium.Skov.Monad
import Concordium.Logger

import qualified Concordium.Scheduler.Types as AT
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.TreeState.Basic
import Concordium.Types as T
import Concordium.GlobalState.Information
import Concordium.GlobalState.Block
import Concordium.Types.HashableTo
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Instances
import Concordium.GlobalState.Account

import Data.IORef
import Text.Read hiding (get)
import qualified Data.Map as Map
import qualified Data.HashMap.Strict as HashMap

import Data.Aeson

import Data.Word

hsh :: BlockPointer -> String
hsh = show . (getHash :: BlockPointer -> BlockHash)

getBestBlockState :: IORef SkovFinalizationState -> IO BlockState
getBestBlockState sfsRef = do
    sfs <- readIORef sfsRef
    runSilentLogger $ flip evalSSM (sfs ^. sfsSkov) (bpState <$> bestBlock)

getLastFinalState :: IORef SkovFinalizationState -> IO BlockState
getLastFinalState sfsRef = do
    sfs <- readIORef sfsRef
    runSilentLogger $ flip evalSSM (sfs ^. sfsSkov) (bpState <$> lastFinalizedBlock)

getLastFinalAccountList :: IORef SkovFinalizationState -> IO [AccountAddress]
getLastFinalAccountList sfsRef = (Map.keys . accountMap . (^. blockAccounts)) <$> getLastFinalState sfsRef

getLastFinalInstances :: IORef SkovFinalizationState -> IO [ContractAddress]
getLastFinalInstances sfsRef = (HashMap.keys . _instances . (^. blockInstances)) <$> getLastFinalState sfsRef

getLastFinalAccountInfo :: IORef SkovFinalizationState -> AccountAddress -> IO (Maybe AccountInfo)
getLastFinalAccountInfo sfsRef addr = do
  maybeAccount <- (getAccount addr . (^. blockAccounts)) <$> getLastFinalState sfsRef
  case maybeAccount of
    Nothing -> return Nothing
    Just acc -> return $ Just (AccountInfo (acc ^. T.accountNonce) (acc ^. T.accountAmount))

getLastFinalContractInfo :: IORef SkovFinalizationState -> AT.ContractAddress -> IO (Maybe InstanceInfo)
getLastFinalContractInfo sfsRef addr = do
  maybeAccount <- (HashMap.lookup addr . _instances . (^. blockInstances)) <$> getLastFinalState sfsRef
  case maybeAccount of
    Nothing -> return Nothing
    Just is -> return $ Just (InstanceInfo (imsgTy is) (imodel is) (iamount is))

getConsensusStatus :: IORef SkovFinalizationState -> IO Value
getConsensusStatus sfsRef = do
    sfs <- readIORef sfsRef
    runSilentLogger $ flip evalSSM (sfs ^. sfsSkov) $ do
        bb <- bestBlock
        lfb <- lastFinalizedBlock
        return $ object [
                "bestBlock" .= hsh bb,
                "genesisBlock" .= hsh (sfs ^. genesisBlockPointer),
                "lastFinalizedBlock" .= hsh lfb,
                "bestBlockHeight" .= theBlockHeight (bpHeight bb),
                "lastFinalizedBlockHeight" .= theBlockHeight (bpHeight lfb),
                "blocksReceivedCount" .= (sfs ^. statistics . blocksReceivedCount),
                "blockLastReceivedTime" .= (sfs ^. statistics . blockLastReceived),
                "blockReceiveLatencyEMA" .= (sfs ^. statistics . blockReceiveLatencyEMA),
                "blockReceiveLatencyEMSD" .= sqrt (sfs ^. statistics . blockReceiveLatencyEMVar),
                "blockReceivePeriodEMA" .= (sfs ^. statistics . blockReceivePeriodEMA),
                "blockReceivePeriodEMSD" .= (sqrt <$> (sfs ^. statistics . blockReceivePeriodEMVar)),
                "blocksVerifiedCount" .= (sfs ^. statistics . blocksVerifiedCount),
                "blockLastArrivedTime" .= (sfs ^. statistics . blockLastArrive),
                "blockArriveLatencyEMA" .= (sfs ^. statistics . blockArriveLatencyEMA),
                "blockArriveLatencyEMSD" .= sqrt (sfs ^. statistics . blockArriveLatencyEMVar),
                "blockArrivePeriodEMA" .= (sfs ^. statistics . blockArrivePeriodEMA),
                "blockArrivePeriodEMSD" .= (sqrt <$> (sfs ^. statistics . blockArrivePeriodEMVar)),
                "transactionsPerBlockEMA" .= (sfs ^. statistics . transactionsPerBlockEMA),
                "transactionsPerBlockEMSD" .= sqrt (sfs ^. statistics . transactionsPerBlockEMVar),
                "finalizationCount" .= (sfs ^. statistics . finalizationCount),
                "lastFinalizedTime" .= (sfs ^. statistics . lastFinalizedTime),
                "finalizationPeriodEMA" .= (sfs ^. statistics . finalizationPeriodEMA),
                "finalizationPeriodEMSD" .= (sqrt <$> (sfs ^. statistics . finalizationPeriodEMVar))
            ]

getBlockInfo :: IORef SkovFinalizationState -> String -> IO Value
getBlockInfo sfsRef blockHash = case readMaybe blockHash of
        Nothing -> return Null
        Just bh -> do
            sfs <- readIORef sfsRef
            runSilentLogger $ flip evalSSM (sfs ^. sfsSkov) $
                resolveBlock bh >>= \case
                    Nothing -> return Null
                    Just bp -> do
                        let slot = blockSlot (bpBlock bp)
                        slotTime <- getSlotTime slot
                        bfin <- isFinalized bh
                        return $ object [
                            "blockHash" .= hsh bp,
                            "blockParent" .= hsh (bpParent bp),
                            "blockLastFinalized" .= hsh (bpLastFinalized bp),
                            "blockHeight" .= theBlockHeight (bpHeight bp),
                            "blockReceiveTime" .= bpReceiveTime bp,
                            "blockArriveTime" .= bpArriveTime bp,
                            "blockSlot" .= (fromIntegral slot :: Word64),
                            "blockSlotTime" .= slotTime,
                            "blockBaker" .= if slot == 0 then Null else toJSON (blockBaker (bpBlock bp)),
                            "finalized" .= bfin,
                            "transactionCount" .= bpTransactionCount bp
                            ]

getAncestors :: IORef SkovFinalizationState -> String -> BlockHeight -> IO Value
getAncestors sfsRef blockHash count = case readMaybe blockHash of
        Nothing -> return Null
        Just bh -> do
            sfs <- readIORef sfsRef
            runSilentLogger $ flip evalSSM (sfs ^. sfsSkov) $
                resolveBlock bh >>= \case
                    Nothing -> return Null
                    Just bp -> do
                        let heightLim = if count > bpHeight bp then 0 else bpHeight bp - count + 1
                        return $ toJSONList $ map hsh $ takeWhile (\b -> bpHeight b >= heightLim) $ iterate bpParent bp
 
getBranches :: IORef SkovFinalizationState -> IO Value
getBranches sfsRef = do
        sfs <- readIORef sfsRef
        runSilentLogger $ flip evalSSM (sfs ^. sfsSkov) $ do
            brs <- branchesFromTop
            let brt = foldl up Map.empty brs
            lastFin <- lastFinalizedBlock
            return $ object ["blockHash" .= hsh lastFin, "children" .= Map.findWithDefault [] lastFin brt]
    where
        up :: Map.Map BlockPointer [Value] -> [BlockPointer] -> Map.Map BlockPointer [Value]
        up childrenMap = foldr (\b -> at (bpParent b) . non [] %~ (object ["blockHash" .= hsh b, "children" .= Map.findWithDefault [] b childrenMap] :)) Map.empty
 
