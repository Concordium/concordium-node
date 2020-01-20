{-# LANGUAGE FlexibleContexts, NumericUnderscores, ScopedTypeVariables, TypeFamilies, FlexibleInstances, GeneralizedNewtypeDeriving, TemplateHaskell, UndecidableInstances, StandaloneDeriving, DerivingVia, RecordWildCards #-}
-- |

module Concordium.GlobalState.Persistent.LMDB where

import Lens.Micro.Platform
import Concordium.Types
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Basic.BlockPointer
import qualified Concordium.GlobalState.Basic.Block as B
import Concordium.GlobalState.Block
import Concordium.GlobalState.Classes
import Concordium.Types.HashableTo
import Concordium.GlobalState.Finalization
import Database.LMDB.Simple as L
import Database.LMDB.Raw
import Control.Monad.IO.Class
import Control.Exception
import System.Directory
import Data.Serialize as S (Serialize, get, put, runGet, runPut, runGetState, decode)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Except
import Data.ByteString
import Data.Time.Clock
import Concordium.Types.Transactions (utcTimeToTransactionTime)

data DatabaseHandlers bs = DatabaseHandlers {
    _limits :: Limits,
    _storeEnv :: Environment ReadWrite,
    _blockStore :: Database BlockHash ByteString,
    _finalizationRecordStore :: Database FinalizationIndex FinalizationRecord
}
makeLenses ''DatabaseHandlers

defaultLocation :: IO FilePath
defaultLocation = do
  dir <- (++ "/treestate") <$> getCurrentDirectory
  createDirectoryIfMissing False dir
  return dir

initialDatabaseHandlers :: BasicBlockPointer bs -> ByteString -> IO (DatabaseHandlers bs)
initialDatabaseHandlers gb serState = liftIO $ do
  let _limits = defaultLimits { mapSize = 64_000_000, maxDatabases = 2 } -- 64MB
  dir <- defaultLocation
  _storeEnv <- openEnvironment dir _limits
  _blockStore <- transaction _storeEnv $
    (getDatabase (Just "blocks") :: L.Transaction ReadWrite (Database BlockHash ByteString))
  _finalizationRecordStore <- transaction _storeEnv $
    (getDatabase (Just "finalization") :: L.Transaction ReadWrite (Database FinalizationIndex FinalizationRecord))
  let gbh = getHash gb
      gbfin = FinalizationRecord 0 gbh emptyFinalizationProof 0
  transaction _storeEnv $ L.put _blockStore (getHash gb) (Just $ runPut (putBlock gb >> S.put serState >> S.put (BlockHeight 0)))
  transaction _storeEnv $ L.put _finalizationRecordStore 0 (Just gbfin)
  return $ DatabaseHandlers {..}

newtype LMDBMonad s m a = LMDBMonad { runLMDBMonad :: m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState s, BlockStateTypes,
            BlockStateQuery, BlockStateOperations, BlockStateStorage)

instance (bs ~ BlockState m) => GlobalStateTypes (LMDBMonad (DatabaseHandlers bs) m) where
  type PendingBlock (LMDBMonad (DatabaseHandlers bs) m) = B.PendingBlock
  type BlockPointer (LMDBMonad (DatabaseHandlers bs) m) = BasicBlockPointer bs

class (MonadIO m) => LMDBStoreMonad m where
   putOrResize :: (Serialize k, Serialize v) => Limits -> String -> Environment ReadWrite -> Database k v -> k -> v -> m (Limits, Environment ReadWrite, Database k v)
   putOrResize lim name env db k v = liftIO $ catch (do
                                              transaction env $ L.put db k (Just v)
                                              return $ (lim, env, db))
                                    (\(e :: LMDB_Error) -> case e of
                                        LMDB_Error _ _ (Right MDB_MAP_FULL) -> do
                                          dir <- defaultLocation
                                          let lim' = lim { mapSize = mapSize lim + 64_000_000 }
                                          env' <- openEnvironment dir lim'
                                          db' <- transaction env' $ (getDatabase (Just name) :: Transaction ReadWrite (Database k v))
                                          transaction env' $ L.put db' k (Just v)
                                          return $ (lim', env', db')
                                        _ -> error $ show e
                                    )

   writeBlock :: (BlockPointer m) -> m ()

   readBlock :: BlockHash -> m (Maybe (BlockPointer m))

   writeFinalizationRecord :: FinalizationRecord -> m ()

   readFinalizationRecord :: FinalizationIndex -> m (Maybe FinalizationRecord)

instance (bs ~ BlockState (LMDBMonad (DatabaseHandlers bs) m),
          MonadIO m,
          MonadState (DatabaseHandlers bs) m,
          BlockStateStorage m) => LMDBStoreMonad (LMDBMonad (DatabaseHandlers bs) m) where
  writeBlock bp = do
    lim <- use limits
    env <- use storeEnv
    dbB <- use blockStore
    bs <- putBlockState (_bpState bp)
    (l, e, d) <- putOrResize lim "blocks" env dbB (getHash bp) $ runPut (putBlock bp >> bs >> S.put (bpHeight bp))
    limits  .= l
    storeEnv .= e
    blockStore .= d
  readBlock bh = do
    env <- use storeEnv
    dbB <- use blockStore
    bytes <- liftIO $ transaction env $ (L.get dbB bh :: L.Transaction ReadOnly (Maybe ByteString))
    case bytes of
      Just b -> do
        tm <- liftIO $ getCurrentTime
        case runGetState (B.getBlock (utcTimeToTransactionTime tm)) b 0 of
          Right (newBlock, rest) ->
           case runGetState S.get rest 0 of
             Right (bs, rest') -> do
              case runGet getBlockState bs of
                Right state' -> do
                  st <- state'
                  case decode rest' of
                    Right height' -> liftIO $ Just <$> makeBlockPointerFromBlock newBlock st height'
                    Left _ -> return Nothing
                Left _ -> return Nothing
             Left _ -> return Nothing
          Left _ -> return Nothing
      Nothing -> return Nothing
  readFinalizationRecord bh = do
    env <- use storeEnv
    dbF <- use finalizationRecordStore
    liftIO $ transaction env $ (L.get dbF bh :: L.Transaction ReadOnly (Maybe FinalizationRecord))
  writeFinalizationRecord fr = do
    lim <- use limits
    env <- use storeEnv
    dbF <- use finalizationRecordStore
    (l, e, d) <- putOrResize lim "finalization" env dbF (finalizationIndex fr) fr
    limits .= l
    storeEnv .= e
    finalizationRecordStore .= d


instance (Monad (t m), MonadIO (t m), MonadTrans t, LMDBStoreMonad m) => LMDBStoreMonad (MGSTrans t m) where
  writeBlock = lift . writeBlock
  readBlock = lift . readBlock
  readFinalizationRecord = lift . readFinalizationRecord
  writeFinalizationRecord = lift . writeFinalizationRecord

deriving via (MGSTrans MaybeT m) instance LMDBStoreMonad m => LMDBStoreMonad (MaybeT m)
deriving via (MGSTrans (ExceptT e) m) instance LMDBStoreMonad m => LMDBStoreMonad (ExceptT e m)
