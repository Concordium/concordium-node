{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Concordium.GlobalState.TreeState(BlockPointerData(..))
import qualified Concordium.GlobalState.TreeState as TS
import qualified Concordium.GlobalState.TreeState.Basic as Basic
import Concordium.Types as T
import Concordium.GlobalState.Information
import Concordium.GlobalState.Block
import Concordium.Types.HashableTo
import Concordium.GlobalState.Instances
import Concordium.GlobalState.Account as Account

import Data.IORef
import Text.Read hiding (get)
import qualified Data.Map as Map

import Data.Aeson

import Data.Word

hsh :: Basic.BlockPointer -> String
hsh = show . (getHash :: Basic.BlockPointer -> BlockHash)

getBestBlockState :: IORef SkovFinalizationState -> IO (TS.BlockState (SimpleSkovMonad s m))
getBestBlockState sfsRef = do
    sfs <- readIORef sfsRef
    runSilentLogger $ flip evalSSM (sfs ^. sfsSkov) (bpState <$> bestBlock)

getLastFinalState :: IORef SkovFinalizationState -> IO (TS.BlockState (SimpleSkovMonad s m))
getLastFinalState sfsRef = do
    sfs <- readIORef sfsRef
    runSilentLogger $ flip evalSSM (sfs ^. sfsSkov) (bpState <$> lastFinalizedBlock)

getLastFinalAccountList :: IORef SkovFinalizationState -> IO [AccountAddress]
getLastFinalAccountList sfsRef = do
  bs <- getLastFinalState sfsRef
  evalSSM (TS.getAccountList bs) bs
  -- (Map.keys . accountMap . (^. blockAccounts)) <$> getLastFinalState sfsRef

getLastFinalInstances :: IORef SkovFinalizationState -> IO [ContractAddress]
getLastFinalInstances sfsRef = do
  bs <- getLastFinalState sfsRef
  evalSSM (do ilist <- TS.getContractInstanceList bs
              return (map iaddress ilist)) bs

getLastFinalAccountInfo :: IORef SkovFinalizationState -> AccountAddress -> IO (Maybe AccountInfo)
getLastFinalAccountInfo sfsRef addr = do
  bs <- getLastFinalState sfsRef
  maybeAccount <- evalSSM (TS.getAccount bs addr) bs
  case maybeAccount of
    Nothing -> return Nothing
    Just acc -> return $ Just (AccountInfo (acc ^. T.accountNonce) (acc ^. T.accountAmount))

getLastFinalContractInfo :: IORef SkovFinalizationState -> AT.ContractAddress -> IO (Maybe InstanceInfo)
getLastFinalContractInfo sfsRef addr = do
  bs <- getLastFinalState sfsRef
  maybeIstance <- evalSSM (TS.getContractInstance bs addr) bs
  case maybeIstance of
    Nothing -> return Nothing
    Just is -> return $ Just (instanceInfo is)

getConsensusStatus :: IORef SkovFinalizationState -> IO Value
getConsensusStatus sfsRef = do
    sfs <- readIORef sfsRef
    runSilentLogger $ flip evalSSM (sfs ^. sfsSkov) $ do
        bb <- bestBlock
        lfb <- lastFinalizedBlock
        return $ object [
                "bestBlock" .= hsh bb,
                "genesisBlock" .= hsh (sfs ^. Basic.genesisBlockPointer),
                "lastFinalizedBlock" .= hsh lfb,
                "bestBlockHeight" .= theBlockHeight (bpHeight bb),
                "lastFinalizedBlockHeight" .= theBlockHeight (bpHeight lfb),
                "blocksReceivedCount" .= (sfs ^. Basic.statistics . TS.blocksReceivedCount),
                "blockLastReceivedTime" .= (sfs ^. Basic.statistics . TS.blockLastReceived),
                "blockReceiveLatencyEMA" .= (sfs ^. Basic.statistics . TS.blockReceiveLatencyEMA),
                "blockReceiveLatencyEMSD" .= sqrt (sfs ^. Basic.statistics . TS.blockReceiveLatencyEMVar),
                "blockReceivePeriodEMA" .= (sfs ^. Basic.statistics . TS.blockReceivePeriodEMA),
                "blockReceivePeriodEMSD" .= (sqrt <$> (sfs ^. Basic.statistics . TS.blockReceivePeriodEMVar)),
                "blocksVerifiedCount" .= (sfs ^. Basic.statistics . TS.blocksVerifiedCount),
                "blockLastArrivedTime" .= (sfs ^. Basic.statistics . TS.blockLastArrive),
                "blockArriveLatencyEMA" .= (sfs ^. Basic.statistics . TS.blockArriveLatencyEMA),
                "blockArriveLatencyEMSD" .= sqrt (sfs ^. Basic.statistics . TS.blockArriveLatencyEMVar),
                "blockArrivePeriodEMA" .= (sfs ^. Basic.statistics . TS.blockArrivePeriodEMA),
                "blockArrivePeriodEMSD" .= (sqrt <$> (sfs ^. Basic.statistics . TS.blockArrivePeriodEMVar)),
                "transactionsPerBlockEMA" .= (sfs ^. Basic.statistics . TS.transactionsPerBlockEMA),
                "transactionsPerBlockEMSD" .= sqrt (sfs ^. Basic.statistics . TS.transactionsPerBlockEMVar),
                "finalizationCount" .= (sfs ^. Basic.statistics . TS.finalizationCount),
                "lastFinalizedTime" .= (sfs ^. Basic.statistics . TS.lastFinalizedTime),
                "finalizationPeriodEMA" .= (sfs ^. Basic.statistics . TS.finalizationPeriodEMA),
                "finalizationPeriodEMSD" .= (sqrt <$> (sfs ^. Basic.statistics . TS.finalizationPeriodEMVar))
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
        up :: Map.Map Basic.BlockPointer [Value] -> [Basic.BlockPointer] -> Map.Map Basic.BlockPointer [Value]
        up childrenMap = foldr (\b -> at (bpParent b) . non [] %~ (object ["blockHash" .= hsh b, "children" .= Map.findWithDefault [] b childrenMap] :)) Map.empty
