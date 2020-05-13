{-# LANGUAGE
    OverloadedStrings,
    ScopedTypeVariables,
    TypeFamilies,
    CPP,
    MonoLocalBinds #-}
module Concordium.Getters where

import Lens.Micro.Platform hiding ((.=))

import Concordium.Kontrol.BestBlock
import Concordium.Skov
import qualified Data.HashMap.Strict as HM

import Control.Monad.State.Class

import qualified Concordium.Scheduler.Types as AT
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Types
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.BlockMonads
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Statistics as Stat
import Concordium.Types as T
import Concordium.GlobalState.Information(jsonStorable)
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.Block hiding (PendingBlock)
import Concordium.Types.HashableTo
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.GlobalState.Instance
import Concordium.GlobalState.Finalization
import qualified Data.PQueue.Prio.Max as Queue

import Concordium.Afgjort.Finalize(FinalizationStateLenses(..))

import Control.Concurrent.MVar
import Data.IORef
import Text.Read hiding (get, String)
import qualified Data.Map as Map
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Set as S
import Data.String(fromString)
import Data.Word
import Data.Vector (fromList)
import Control.Monad
import Data.Foldable (foldrM)

class SkovQueryMonad m => SkovStateQueryable z m | z -> m where
    runStateQuery :: z -> m a -> IO a

instance (SkovConfiguration c, SkovQueryMonad (SkovT () c IO))
        => SkovStateQueryable (SkovContext c, IORef (SkovState c)) (SkovT () c IO) where
    runStateQuery (ctx, st) a = readIORef st >>= evalSkovT a () ctx

instance (SkovConfiguration c, SkovQueryMonad (SkovT () c IO))
        => SkovStateQueryable (SkovContext c, MVar (SkovState c)) (SkovT () c IO) where
    runStateQuery (ctx, st) a = readMVar st >>= evalSkovT a () ctx

hsh :: (HashableTo BlockHash a) => a -> String
hsh x = show (getHash x :: BlockHash)

getBestBlockState :: (BlockPointerMonad m, SkovQueryMonad m) => m (BlockState m)
getBestBlockState = queryBlockState =<< bestBlock

getLastFinalState :: SkovQueryMonad m => m (BlockState m)
getLastFinalState = queryBlockState =<< lastFinalizedBlock

getTransactionStatus :: SkovStateQueryable z m => AT.TransactionHash -> z -> IO Value
getTransactionStatus hash sfsRef = runStateQuery sfsRef $
  queryTransactionStatus hash >>= \case
    Nothing -> return Null
    Just AT.Received{} ->
      return $ object ["status" .= String "received"]
    Just AT.Finalized{..} ->
      withBlockStateJSON tsBlockHash $ \bs -> do
        outcome <- BS.getTransactionOutcome bs tsFinResult
        return $ object ["status" .= String "finalized",
                         "outcomes" .= object [fromString (show tsBlockHash) .= outcome]
                        ]
    Just AT.Committed{..} -> do
      outcomes <- forM (HM.toList tsResults) $ \(bh, idx) ->
        resolveBlock bh >>= \case
          Nothing -> return (T.pack (show bh) .= Null) -- should not happen
          Just bp -> do
            outcome <- flip BS.getTransactionOutcome idx =<< queryBlockState bp
            return (T.pack (show bh) .= outcome)
      return $ object ["status" .= String "committed",
                       "outcomes" .= object outcomes
                      ]

getTransactionStatusInBlock :: SkovStateQueryable z m => AT.TransactionHash -> BlockHash -> z -> IO Value
getTransactionStatusInBlock txHash blockHash sfsRef = runStateQuery sfsRef $
  queryTransactionStatus txHash >>= \case
    Nothing -> return Null
    Just AT.Received{} ->
      return $ object ["status" .= String "received"]
    Just AT.Finalized{..} ->
      if tsBlockHash == blockHash then
        withBlockStateJSON tsBlockHash $ \bs -> do
          outcome <- BS.getTransactionOutcome bs tsFinResult
          return $ object ["status" .= String "finalized",
                           "result" .= outcome
                          ]
      else
        return Null
    Just AT.Committed{..} ->
      case HM.lookup blockHash tsResults of
        Nothing -> return Null
        Just idx ->
          withBlockStateJSON blockHash $ \bs -> do
            outcome <- BS.getTransactionOutcome bs idx
            return $ object ["status" .= String "committed",
                             "result" .= outcome
                            ]

getAccountNonFinalizedTransactions :: SkovStateQueryable z m => AccountAddress -> z -> IO [TransactionHash]
getAccountNonFinalizedTransactions addr sfsRef = runStateQuery sfsRef $
    queryNonFinalizedTransactions addr

-- |Return the best guess as to what the next account nonce should be.
-- If all account transactions are finalized then this information is reliable.
-- Otherwise this is the best guess, assuming all other transactions will be
-- committed to blocks and eventually finalized.
-- The 'Bool' indicates whether all transactions are finalized.
getNextAccountNonce :: SkovStateQueryable z m => AccountAddress -> z -> IO Value
getNextAccountNonce addr sfsRef = runStateQuery sfsRef $ do
    (nonce, allFinal) <- (queryNextAccountNonce addr)
    return $ object ["nonce" .= nonce,
                     "allFinal" .= allFinal
                    ]

-- |Return a block with given hash and outcomes.
getBlockSummary :: SkovStateQueryable z m => BlockHash -> z -> IO Value
getBlockSummary hash sfsRef = runStateQuery sfsRef $
  resolveBlock hash >>= \case
    Nothing -> return Null
    Just bp -> do
      bs <- queryBlockState bp
      outcomes <- BS.getOutcomes bs
      specialOutcomes <- BS.getSpecialOutcomes bs
      return $ object [
        "transactionSummaries" .= outcomes,
        "specialEvents" .= specialOutcomes
        ]

withBlockState :: SkovQueryMonad m => BlockHash -> (BlockState m -> m a) -> m (Maybe a)
withBlockState hash f =
  resolveBlock hash >>=
    \case Nothing -> return Nothing
          Just bp -> fmap Just . f =<< queryBlockState bp

withBlockStateJSON :: SkovQueryMonad m => BlockHash -> (BlockState m -> m Value) -> m Value
withBlockStateJSON hash f =
  resolveBlock hash >>=
    \case Nothing -> return Null
          Just bp -> f =<< queryBlockState bp

getAccountList :: SkovStateQueryable z m => BlockHash -> z -> IO Value
getAccountList hash sfsRef = runStateQuery sfsRef $
  withBlockStateJSON hash $ \st -> do
  alist <- BS.getAccountList st
  return . toJSON $ alist  -- show instance for account addresses is based on Base58 encoding

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
                                        -- credentials in descending order
                                        ,"accountCredentials" .= Queue.elems (acc ^. accountCredentials)
                                        ,"accountDelegation" .= (acc ^. T.accountStakeDelegate)
                                        ,"accountInstances" .= S.toList (acc ^. T.accountInstances)
                                        ]

getContractInfo :: (SkovStateQueryable z m) => BlockHash -> z -> AT.ContractAddress -> IO Value
getContractInfo hash sfsRef addr = runStateQuery sfsRef $
  withBlockStateJSON hash $ \st ->
  BS.getContractInstance st addr >>=
      \case Nothing -> return Null
            Just istance -> let params = instanceParameters istance
                            in return $ object ["model" .= jsonStorable (instanceModel istance)
                                               ,"owner" .= instanceOwner params
                                               ,"amount" .= instanceAmount istance]

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
  bps <- BS.getBlockBirkParameters st
  elDiff <- BS.getElectionDifficulty bps
  nonce <- BS.birkLeadershipElectionNonce bps
  lotteryBakers <- BS.getLotteryBakers bps
  return $ object [
    "electionDifficulty" .= elDiff,
    "electionNonce" .= nonce,
    "bakers" .= Array (fromList .
                       map (\(bid, BakerInfo{..}) -> object ["bakerId" .= toInteger bid
                                                            ,"bakerAccount" .= show _bakerAccount
                                                            ,"bakerLotteryPower" .= ((fromIntegral _bakerStake :: Double) / fromIntegral (_bakerTotalStake lotteryBakers))
                                                            ]) .
                       Map.toList $ _bakerMap lotteryBakers)
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
            st <- queryBlockState bp
            mmodul <- BS.getModule st mhash
            return (BS.moduleSource <$> mmodul)

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

getBlockInfo :: (SkovStateQueryable z m, HashableTo BlockHash (BlockPointerType m), BlockPointerMonad m) => z -> String -> IO Value
getBlockInfo sfsRef blockHash = case readMaybe blockHash of
        Nothing -> return Null
        Just bh -> runStateQuery sfsRef $
                resolveBlock bh >>= \case
                    Nothing -> return Null
                    Just bp -> do
                        let slot = blockSlot bp
                        slotTime <- getSlotTime slot
                        bfin <- isFinalized bh
                        parent <- bpParent bp
                        lfin <- bpLastFinalized bp
                        return $ object [
                            "blockHash" .= hsh bp,
                            "blockParent" .= hsh parent,
                            "blockLastFinalized" .= hsh lfin,
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
                            "transactionEnergyCost" .= toInteger (bpTransactionsEnergyCost bp),
                            "transactionsSize" .= toInteger (bpTransactionsSize bp)
                            ]

getAncestors :: (SkovStateQueryable z m, HashableTo BlockHash (BlockPointerType m), BlockPointerMonad m) => z -> String -> BlockHeight -> IO Value
getAncestors sfsRef blockHash count = case readMaybe blockHash of
        Nothing -> return Null
        Just bh -> runStateQuery sfsRef $
                resolveBlock bh >>= \case
                    Nothing -> return Null
                    Just bp -> do
                      parents <- iterateForM bpParent (fromIntegral $ min count (1 + bpHeight bp)) bp
                      return $ toJSONList $ map hsh parents
   where
     iterateForM :: (Monad m) => (a -> m a) -> Int -> a -> m [a]
     iterateForM f steps initial = reverse <$> (go [] steps initial)
       where go acc n a | n <= 0 = return acc
                        | otherwise = do
                         a' <- f a
                         go (a:acc) (n-1) a'

getBranches :: forall z m. (SkovStateQueryable z m, Ord (BlockPointerType m), HashableTo BlockHash (BlockPointerType m), BlockPointerMonad m) => z -> IO Value
getBranches sfsRef = runStateQuery sfsRef $ do
            brs <- branchesFromTop :: m [[BlockPointerType m]]
            brt <- foldM up Map.empty brs :: m (Map.Map (BlockPointerType m) [Value])
            lastFin <- lastFinalizedBlock :: m (BlockPointerType m)
            return $ object ["blockHash" .= hsh lastFin, "children" .= Map.findWithDefault [] lastFin brt]
    where
        up :: Map.Map (BlockPointerType m) [Value] -> [BlockPointerType m] -> m (Map.Map (BlockPointerType m) [Value])
        up childrenMap = foldrM (\(b :: BlockPointerType m) (ma :: Map.Map (BlockPointerType m) [Value]) -> do
                                    parent <- bpParent b :: m (BlockPointerType m)
                                    return $ (at parent . non [] %~ (object ["blockHash" .= hsh b, "children" .= (Map.findWithDefault [] b childrenMap :: [Value])] :)) ma) Map.empty

getBlockFinalization :: (SkovStateQueryable z m, TS.TreeStateMonad m) => z -> BlockHash -> IO (Maybe FinalizationRecord)
getBlockFinalization sfsRef bh = runStateQuery sfsRef $ do
            bs <- TS.getBlockStatus bh
            case bs of
                Just (TS.BlockFinalized _ fr) -> return $ Just fr
                _ -> return Nothing

-- |Check whether a keypair is part of the baking committee by a key pair in the current best block.
-- Returns 0 if keypair is not added as a baker.
-- Returns 1 if keypair is added as a baker, but not part of the baking committee yet.
-- Returns 2 if keypair is part of the baking committee.
checkBakerExistsBestBlock :: (BlockPointerMonad m, SkovStateQueryable z m)
    => BakerSignVerifyKey
    -> z
    -> IO Word8
checkBakerExistsBestBlock key sfsRef = runStateQuery sfsRef $ do
  bb <- bestBlock
  bps <- BS.getBlockBirkParameters =<< queryBlockState bb
  lotteryBakers <- BS.getLotteryBakers bps
  currentBakers <- BS.getCurrentBakers bps
  case lotteryBakers ^. bakersByKey . at key of
    Just _ -> return 2
    Nothing ->
      case currentBakers ^. bakersByKey . at key of
        Just _ -> return 1
        Nothing -> return 0

-- |Check whether the node is currently a member of the finalization committee.
checkIsCurrentFinalizer :: (SkovStateQueryable z m, MonadState s m, FinalizationStateLenses s t) => z -> IO Bool
checkIsCurrentFinalizer sfsRef = runStateQuery sfsRef $ do
   fs <- use finState
   case fs ^. finCurrentRound of
     Left _ -> return False
     Right _ -> return True
