{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, FlexibleContexts, TypeFamilies #-}
module Concordium.Getters where

import Lens.Micro.Platform hiding ((.=))

import Concordium.Kontrol.BestBlock
import Concordium.Skov
import Concordium.Skov.Update (isAncestorOf)

import qualified Concordium.Scheduler.Types as AT
import Concordium.GlobalState.BlockState(BlockPointerData(..))
import qualified Concordium.GlobalState.TreeState as TS
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Statistics as Stat
import Concordium.Types as T
import Concordium.GlobalState.Information(jsonStorable)
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.Block
import Concordium.GlobalState.Basic.Block
import Concordium.GlobalState.Basic.BlockState
import Concordium.Types.HashableTo
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.GlobalState.Instances
import Concordium.GlobalState.Modules(moduleSource)
import Concordium.GlobalState.Finalization
import qualified Concordium.Skov.CatchUp as CU

import Control.Concurrent.MVar
import Data.IORef
import Text.Read hiding (get, String)
import qualified Data.Map as Map
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as EL
import Data.Word
import Data.ByteString.Builder(toLazyByteString, byteStringHex)
import Data.Vector (fromList)

class SkovQueryMonad m => SkovStateQueryable z m | z -> m where
    runStateQuery :: z -> m a -> IO a

instance SkovStateQueryable (IORef SkovActiveState) (SkovQueryM SkovActiveState IO) where
    runStateQuery sfsRef a = readIORef sfsRef >>= evalSkovQueryM a

instance SkovStateQueryable (MVar SkovBufferedState) (SkovQueryM SkovBufferedState IO) where
    runStateQuery sfsRef a = readMVar sfsRef >>= evalSkovQueryM a

instance SkovStateQueryable (MVar SkovBufferedHookedState) (SkovQueryM SkovBufferedHookedState IO) where
    runStateQuery sfsRef a = readMVar sfsRef >>= evalSkovQueryM a

instance SkovStateQueryable (MVar SkovPassiveState) (SkovQueryM SkovPassiveState IO) where
    runStateQuery sfsRef a = readMVar sfsRef >>= evalSkovQueryM a

instance SkovStateQueryable (MVar SkovPassiveHookedState) (SkovQueryM SkovPassiveHookedState IO) where
    runStateQuery sfsRef a = readMVar sfsRef >>= evalSkovQueryM a

hsh :: (HashableTo BlockHash a) => a -> String
hsh x = show (getHash x :: BlockHash)

getBestBlockState :: SkovQueryMonad m => m (BS.BlockState m)
getBestBlockState = bpState <$> bestBlock

getLastFinalState :: SkovQueryMonad m => m (BS.BlockState m)
getLastFinalState = bpState <$> lastFinalizedBlock

withBlockStateJSON :: SkovQueryMonad m => BlockHash -> (BS.BlockState m -> m Value) -> m Value
withBlockStateJSON hash f =
  resolveBlock hash >>=
    \case Nothing -> return Null
          Just bp -> f (bpState bp)

getAccountList :: SkovStateQueryable z m => BlockHash -> z -> IO Value
getAccountList hash sfsRef = runStateQuery sfsRef $
  withBlockStateJSON hash $ \st -> do
  alist <- BS.getAccountList st
  return . toJSON . map show $ alist  -- show instance for account addresses is based on Base58 encoding

getInstances :: (SkovStateQueryable z m) => BlockHash -> z -> IO Value
getInstances hash sfsRef = runStateQuery sfsRef $
  withBlockStateJSON hash $ \st -> do
  ilist <- BS.getContractInstanceList st
  return $ toJSON (map iaddress ilist)

getAccountInfo :: (SkovStateQueryable z m) => BlockHash -> z -> AccountAddress -> IO Value
getAccountInfo hash sfsRef addr = runStateQuery sfsRef $
  withBlockStateJSON hash $ \st ->
  BS.getAccount st addr >>=
      \case Nothing -> return Null
            Just acc -> return $ object ["accountNonce" .= let Nonce n = (acc ^. T.accountNonce) in n
                                        ,"accountAmount" .= toInteger (acc ^. T.accountAmount)
                                        ,"accountCredentials" .= (acc ^. accountCredentials)
                                        ,"accountDelegation" .= (toInteger <$> (acc ^. T.accountStakeDelegate))
                                        ]

getContractInfo :: (SkovStateQueryable z m) => BlockHash -> z -> AT.ContractAddress -> IO Value
getContractInfo hash sfsRef addr = runStateQuery sfsRef $
  withBlockStateJSON hash $ \st ->
  BS.getContractInstance st addr >>=
      \case Nothing -> return Null
            Just istance -> let params = instanceParameters istance
                            in return $ object ["model" .= jsonStorable (instanceModel istance)
                                               ,"owner" .= String (T.pack (show (instanceOwner params))) -- account address show instance is base58
                                               ,"amount" .= toInteger (instanceAmount istance)]

getRewardStatus :: (SkovStateQueryable z m) => BlockHash -> z -> IO Value
getRewardStatus hash sfsRef = runStateQuery sfsRef $
  withBlockStateJSON hash $ \st -> do
  reward <- BS.getRewardStatus st
  return $ object [
    "totalAmount" .= (fromIntegral (reward ^. AT.totalGTU) :: Integer),
    "totalEncryptedAmount" .= (fromIntegral (reward ^. AT.totalEncryptedGTU) :: Integer),
    "centralBankAmount" .= (fromIntegral (reward ^. AT.centralBankGTU) :: Integer),
    "mintedAmountPerSlot" .= (fromIntegral (reward ^. AT.mintedGTUPerSlot) :: Integer)
    ]

getBlockBirkParameters :: (SkovStateQueryable z m) => BlockHash -> z -> IO Value
getBlockBirkParameters hash sfsRef = runStateQuery sfsRef $
  withBlockStateJSON hash $ \st -> do
  BirkParameters{..} <- BS.getBlockBirkParameters st
  return $ object [
    "electionDifficulty" .= _birkElectionDifficulty,
    "electionNonce" .= String (TL.toStrict . EL.decodeUtf8 . toLazyByteString . byteStringHex $ _birkLeadershipElectionNonce),
    "bakers" .= Array (fromList .
                       map (\(bid, BakerInfo{..}) -> object ["bakerId" .= (toInteger bid)
                                                            ,"bakerAccount" .= show _bakerAccount
                                                            ,"bakerLotteryPower" .= ((fromIntegral _bakerStake :: Double) / fromIntegral (_bakerTotalStake _birkBakers))
                                                            ]) .
                       Map.toList $ _bakerMap $ _birkBakers)
    ]

getModuleList :: (SkovStateQueryable z m) => BlockHash -> z -> IO Value
getModuleList hash sfsRef = runStateQuery sfsRef $
  withBlockStateJSON hash $ \st -> do
  mlist <- BS.getModuleList st
  return . toJSON . map show $ mlist -- show instance of ModuleRef displays it in Base16


getModuleSource :: (SkovStateQueryable z m) => BlockHash -> z -> ModuleRef -> IO (Maybe (Core.Module Core.UA))
getModuleSource hash sfsRef mhash = runStateQuery sfsRef $
  resolveBlock hash >>=
    \case Nothing -> return Nothing
          Just bp -> do
            mmodul <- BS.getModule (bpState bp) mhash
            return $ (moduleSource <$> mmodul)

getConsensusStatus :: (SkovStateQueryable z m, TS.TreeStateMonad m) => z -> IO Value
getConsensusStatus sfsRef = runStateQuery sfsRef $ do
        bb <- bestBlock
        lfb <- lastFinalizedBlock
        genesis <- genesisBlock
        stats <- TS.getConsensusStatistics
        return $ object [
                "bestBlock" .= hsh bb,
                "genesisBlock" .= hsh genesis,
                "lastFinalizedBlock" .= hsh lfb,
                "bestBlockHeight" .= theBlockHeight (bpHeight bb),
                "lastFinalizedBlockHeight" .= theBlockHeight (bpHeight lfb),
                "blocksReceivedCount" .= (stats ^. Stat.blocksReceivedCount),
                "blockLastReceivedTime" .= (stats ^. Stat.blockLastReceived),
                "blockReceiveLatencyEMA" .= (stats ^. Stat.blockReceiveLatencyEMA),
                "blockReceiveLatencyEMSD" .= sqrt (stats ^. Stat.blockReceiveLatencyEMVar),
                "blockReceivePeriodEMA" .= (stats ^. Stat.blockReceivePeriodEMA),
                "blockReceivePeriodEMSD" .= (sqrt <$> (stats ^. Stat.blockReceivePeriodEMVar)),
                "blocksVerifiedCount" .= (stats ^. Stat.blocksVerifiedCount),
                "blockLastArrivedTime" .= (stats ^. Stat.blockLastArrive),
                "blockArriveLatencyEMA" .= (stats ^. Stat.blockArriveLatencyEMA),
                "blockArriveLatencyEMSD" .= sqrt (stats ^. Stat.blockArriveLatencyEMVar),
                "blockArrivePeriodEMA" .= (stats ^. Stat.blockArrivePeriodEMA),
                "blockArrivePeriodEMSD" .= (sqrt <$> (stats ^. Stat.blockArrivePeriodEMVar)),
                "transactionsPerBlockEMA" .= (stats ^. Stat.transactionsPerBlockEMA),
                "transactionsPerBlockEMSD" .= sqrt (stats ^. Stat.transactionsPerBlockEMVar),
                "finalizationCount" .= (stats ^. Stat.finalizationCount),
                "lastFinalizedTime" .= (stats ^. Stat.lastFinalizedTime),
                "finalizationPeriodEMA" .= (stats ^. Stat.finalizationPeriodEMA),
                "finalizationPeriodEMSD" .= (sqrt <$> (stats ^. Stat.finalizationPeriodEMVar))
            ]

getBlockInfo :: (SkovStateQueryable z m, TS.TreeStateMonad m) => z -> String -> IO Value
getBlockInfo sfsRef blockHash = case readMaybe blockHash of
        Nothing -> return Null
        Just bh -> runStateQuery sfsRef $
                resolveBlock bh >>= \case
                    Nothing -> return Null
                    Just bp -> do
                        let slot = blockSlot bp
                        reward <- BS.getRewardStatus (bpState bp)
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
                            "blockBaker" .= case blockFields bp of
                                            Nothing -> Null
                                            Just bf -> toJSON (toInteger (blockBaker bf)),
                            "finalized" .= bfin,
                            "transactionCount" .= bpTransactionCount bp,

                            "totalAmount" .= (fromIntegral (reward ^. AT.totalGTU) :: Integer),
                            "totalEncryptedAmount" .= (fromIntegral (reward ^. AT.totalEncryptedGTU) :: Integer),
                            "centralBankAmount" .= (fromIntegral (reward ^. AT.centralBankGTU) :: Integer),
                            "mintedAmountPerSlot" .= (fromIntegral (reward ^. AT.mintedGTUPerSlot) :: Integer),
                            "executionCost" .= (fromIntegral (reward ^. AT.executionCost) :: Integer)
                            ]

getAncestors :: (SkovStateQueryable z m, TS.TreeStateMonad m) => z -> String -> BlockHeight -> IO Value
getAncestors sfsRef blockHash count = case readMaybe blockHash of
        Nothing -> return Null
        Just bh -> runStateQuery sfsRef $
                resolveBlock bh >>= \case
                    Nothing -> return Null
                    Just bp -> do
                        let heightLim = if count > bpHeight bp then 0 else bpHeight bp - count + 1
                        return $ toJSONList $ map hsh $ takeWhile (\b -> bpHeight b >= heightLim) $ iterate bpParent bp

getBranches :: (SkovStateQueryable z m, TS.TreeStateMonad m) => z -> IO Value
getBranches sfsRef = runStateQuery sfsRef $ do
            brs <- branchesFromTop
            let brt = foldl up Map.empty brs
            lastFin <- lastFinalizedBlock
            return $ object ["blockHash" .= hsh lastFin, "children" .= Map.findWithDefault [] lastFin brt]
    where
        up childrenMap = foldr (\b -> at (bpParent b) . non [] %~ (object ["blockHash" .= hsh b, "children" .= Map.findWithDefault [] b childrenMap] :)) Map.empty

getBlockData :: (SkovStateQueryable z m, TS.TreeStateMonad m, BS.BlockPointer m ~ BlockPointer, TS.PendingBlock m ~ PendingBlock)
    => z -> BlockHash -> IO (Maybe Block)
getBlockData sfsRef bh = runStateQuery sfsRef $
            TS.getBlockStatus bh <&> \case
                Just (TS.BlockAlive bp) -> Just (_bpBlock bp)
                Just (TS.BlockFinalized bp _) -> Just (_bpBlock bp)
                Just (TS.BlockPending pb) -> Just $ NormalBlock (pbBlock pb)
                Just (TS.BlockDead) -> Nothing
                Nothing -> Nothing

getBlockDescendant :: (SkovStateQueryable z m, BS.BlockPointer m ~ BlockPointer) => z -> BlockHash -> BlockHeight -> IO (Maybe Block)
getBlockDescendant sfsRef ancestor distance = runStateQuery sfsRef $
            resolveBlock ancestor >>= \case
                Nothing -> return Nothing
                Just bp -> do
                    candidates <- getBlocksAtHeight (bpHeight bp + distance)
                    return $ _bpBlock <$> candidates ^? each . filtered (bp `isAncestorOf`)

getBlockFinalization :: (SkovStateQueryable z m, TS.TreeStateMonad m) => z -> BlockHash -> IO (Maybe FinalizationRecord)
getBlockFinalization sfsRef bh = runStateQuery sfsRef $ do
            bs <- TS.getBlockStatus bh
            case bs of
                Just (TS.BlockFinalized _ fr) -> return $ Just fr
                _ -> return Nothing

getIndexedFinalization :: (SkovStateQueryable z m, TS.TreeStateMonad m) => z -> FinalizationIndex -> IO (Maybe FinalizationRecord)
getIndexedFinalization sfsRef finInd = runStateQuery sfsRef $ TS.getFinalizationAtIndex finInd

getCatchUpStatus :: (SkovStateQueryable z m, TS.TreeStateMonad m) => z -> IO CU.CatchUpStatus
getCatchUpStatus sRef = runStateQuery sRef $ CU.getCatchUpStatus True

handleCatchUpStatus :: (SkovStateQueryable z m, TS.TreeStateMonad m) => z -> CU.CatchUpStatus -> IO (Either String (Maybe ([Either FinalizationRecord (BS.BlockPointer m)], CU.CatchUpStatus), Bool))
handleCatchUpStatus sRef cus = runStateQuery sRef $ CU.handleCatchUp cus
