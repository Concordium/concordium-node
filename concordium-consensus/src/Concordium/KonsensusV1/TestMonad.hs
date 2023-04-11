{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Concordium.KonsensusV1.TestMonad where

import Control.Monad.Catch
import Control.Monad.RWS.Strict
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Time
import Lens.Micro.Platform

import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Types

import qualified Concordium.Genesis.Data.BaseV1 as BaseV1
import qualified Concordium.Genesis.Data.P6 as P6
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters (
    GenesisData (GDP6),
    defaultRuntimeParameters,
    genesisBlockHash,
 )
import Concordium.GlobalState.Persistent.Account (AccountCache)
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Module
import qualified Concordium.GlobalState.Persistent.Cache as Cache
import Concordium.GlobalState.Persistent.Genesis (genesisState)
import Concordium.GlobalState.Types
import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.LowLevel.Memory
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types
import Concordium.TimerMonad (Timeout, TimerMonad (..))
import Concordium.Types.HashableTo
import Concordium.Types.Parameters hiding (getChainParameters)

data TestContext (pv :: ProtocolVersion) = TestContext
    { _tcBakerContext :: !BakerContext,
      _tcPersistentBlockStateContext :: PersistentBlockStateContext pv,
      _tcMemoryLLDB :: !(IORef (LowLevelDB pv)),
      _tcCurrentTime :: !UTCTime,
      _tcLogger :: !(LogMethod (TestMonad pv))
    }

instance HasBlobStore (TestContext pv) where
    blobStore = blobStore . _tcPersistentBlockStateContext
    blobLoadCallback = blobLoadCallback . _tcPersistentBlockStateContext
    blobStoreCallback = blobStoreCallback . _tcPersistentBlockStateContext

instance AccountVersionFor pv ~ av => Cache.HasCache (AccountCache av) (TestContext pv) where
    projectCache = Cache.projectCache . _tcPersistentBlockStateContext

instance Cache.HasCache Module.ModuleCache (TestContext pv) where
    projectCache = Cache.projectCache . _tcPersistentBlockStateContext

instance HasMemoryLLDB pv (TestContext pv) where
    theMemoryLLDB = _tcMemoryLLDB

data TestState pv = TestState
    { _tsSkovData :: !(SkovData pv),
      _tsPendingTimers :: !(Map.Map Integer (Timeout, TestMonad pv ()))
    }

data TestEvent
    = ResetTimer !Duration
    | SendTimeoutMessage !TimeoutMessage
    | SendQuorumMessage !QuorumMessage
    deriving (Eq, Show)

type TestWrite = [TestEvent]

type PersistentBlockStateMonadHelper pv =
    PersistentBlockStateMonad
        pv
        (TestContext pv)
        (RWST (TestContext pv) TestWrite (TestState pv) IO)

newtype TestMonad (pv :: ProtocolVersion) a = TestMonad {runTestMonad' :: RWST (TestContext pv) TestWrite (TestState pv) IO a}
    deriving newtype (Functor, Applicative, Monad, MonadReader (TestContext pv), MonadIO, MonadThrow, MonadWriter TestWrite)
    deriving
        (BlockStateTypes, ContractStateOperations, ModuleQuery)
        via (PersistentBlockStateMonadHelper pv)

makeLenses ''TestContext
makeLenses ''TestState

instance HasBakerContext (TestContext pv) where
    bakerContext = tcBakerContext

genesisCore :: forall pv. (IsConsensusV1 pv, IsProtocolVersion pv) => GenesisData pv -> BaseV1.CoreGenesisParametersV1
genesisCore = case protocolVersion @pv of
    SP6 -> \(GDP6 P6.GDP6Initial{genesisCore = core}) -> core

runTestMonad :: (IsConsensusV1 pv, IsProtocolVersion pv) => BakerContext -> UTCTime -> GenesisData pv -> TestMonad pv a -> IO a
runTestMonad _tcBakerContext _tcCurrentTime genData (TestMonad a) =
    runBlobStoreTemp "." $ withNewAccountCache 1000 $ do
        (genState, genStateRef, initTT, genTimeoutBase, genEpochBakers) <- runPersistentBlockStateMonad $ do
            genesisState genData >>= \case
                Left e -> error e
                Right (genState, initTT) -> do
                    chainParams <- getChainParameters genState
                    let genTimeoutBase = chainParams ^. cpConsensusParameters . cpTimeoutParameters . tpTimeoutBase
                    curBakers <- getCurrentEpochBakers genState
                    curFCP <- getCurrentEpochFinalizationCommitteeParameters genState
                    let curBF = computeBakersAndFinalizers curBakers curFCP
                    nextBakers <- getNextEpochBakers genState
                    nextFCP <- getNextEpochFinalizationCommitteeParameters genState
                    let nextBF = computeBakersAndFinalizers nextBakers nextFCP
                    payday <- getPaydayEpoch genState
                    let genEpochBakers =
                            EpochBakers
                                { _previousEpochBakers = curBF,
                                  _currentEpochBakers = curBF,
                                  _nextEpochBakers = nextBF,
                                  _nextPayday = payday
                                }
                    genStateRef <- saveBlockState genState
                    return (genState, genStateRef, initTT, genTimeoutBase, genEpochBakers)
        let genMetadata =
                GenesisMetadata
                    { gmParameters = genesisCore genData,
                      gmCurrentGenesisHash = genesisBlockHash genData,
                      gmFirstGenesisHash = genesisBlockHash genData,
                      gmStateHash = getHash genState
                    }
        let _tsSkovData =
                mkInitialSkovData
                    defaultRuntimeParameters
                    genMetadata
                    genState
                    genTimeoutBase
                    genEpochBakers
                    & transactionTable .~ initTT
        let genBlockPtr = _tsSkovData ^. lastFinalized
        let genStoredBlock =
                LowLevel.StoredBlock
                    { stbInfo = blockMetadata genBlockPtr,
                      stbBlock = bpBlock genBlockPtr,
                      stbStatePointer = genStateRef
                    }
        _tcMemoryLLDB <- liftIO . newIORef $! initialLowLevelDB genStoredBlock (_tsSkovData ^. roundStatus)
        _tcPersistentBlockStateContext <- ask
        let _tcLogger src lvl msg = liftIO $ putStrLn $ "[" ++ show lvl ++ "] " ++ show src ++ ": " ++ msg
        let ctx = TestContext{..}
        let _tsPendingTimers = Map.empty
        let st = TestState{..}
        fst <$> liftIO (evalRWST a ctx st)

deriving via
    (PersistentBlockStateMonadHelper pv)
    instance
        IsProtocolVersion pv => MonadProtocolVersion (TestMonad pv)

deriving via
    (PersistentBlockStateMonadHelper pv)
    instance
        IsProtocolVersion pv => AccountOperations (TestMonad pv)

deriving via
    (PersistentBlockStateMonadHelper pv)
    instance
        IsProtocolVersion pv => BlockStateQuery (TestMonad pv)

deriving via
    (PersistentBlockStateMonadHelper pv)
    instance
        IsProtocolVersion pv => BlockStateOperations (TestMonad pv)

deriving via
    (PersistentBlockStateMonadHelper pv)
    instance
        IsProtocolVersion pv => BlockStateStorage (TestMonad pv)

deriving via
    (MemoryLLDBM pv (RWST (TestContext pv) TestWrite (TestState pv) IO))
    instance
        IsProtocolVersion pv => LowLevel.MonadTreeStateStore (TestMonad pv)

instance MonadState (SkovData pv) (TestMonad pv) where
    state = TestMonad . state . tsSkovData
    get = TestMonad (use tsSkovData)
    put = TestMonad . (tsSkovData .=)

instance TimeMonad (TestMonad pv) where
    currentTime = asks _tcCurrentTime

instance MonadTimeout (TestMonad pv) where
    resetTimer = tell . (: []) . ResetTimer

instance MonadMulticast (TestMonad pv) where
    sendTimeoutMessage = tell . (: []) . SendTimeoutMessage
    sendQuorumMessage = tell . (: []) . SendQuorumMessage

instance TimerMonad (TestMonad pv) where
    type Timer (TestMonad pv) = Integer
    onTimeout delay action = TestMonad $ do
        pts <- use tsPendingTimers
        let newIndex = case Map.lookupMax pts of
                Nothing -> 0
                Just (k, _) -> k + 1
        tsPendingTimers .= Map.insert newIndex (delay, void action) pts
        return newIndex
    cancelTimer timer = TestMonad $ tsPendingTimers %= Map.delete timer

instance MonadLogger (TestMonad pv) where
    logEvent src lvl msg = do
        logger <- view tcLogger
        logger src lvl msg
