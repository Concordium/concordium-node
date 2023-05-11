{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- |This module provides a 'TestMonad' that implements a number of interfaces that are used by
-- consensus. The implementation provides a bare-bones scaffolding that can be used for testing.
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

-- |Context used for running the 'TestMonad'.
data TestContext (pv :: ProtocolVersion) = TestContext
    { -- |The baker context (i.e. baker keys if any).
      _tcBakerContext :: !BakerContext,
      -- |Blob store and caches used by the block state storage.
      _tcPersistentBlockStateContext :: PersistentBlockStateContext pv,
      -- |In-memory low-level tree state database.
      _tcMemoryLLDB :: !(IORef (LowLevelDB pv)),
      -- |The current time (reported by 'currentTime').
      _tcCurrentTime :: !UTCTime,
      -- |Callback to use for logging.
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

-- |State used for running the 'TestMonad'.
data TestState pv = TestState
    { -- |The 'SkovData'.
      _tsSkovData :: !(SkovData pv),
      -- |The pending timers.
      _tsPendingTimers :: !(Map.Map Integer (Timeout, TestMonad pv ()))
    }

-- |Events raised in running the 'TestMonad'.
data TestEvent (pv :: ProtocolVersion)
    = -- |Implements 'resetTimer' of 'MonadTimeout'
      ResetTimer !Duration
    | -- |Implements 'sendTimeoutMessage' of 'MonadBroadcast'.
      SendTimeoutMessage !TimeoutMessage
    | -- |Implements 'sendQuorumMessage' of 'MonadBroadcast'.
      SendQuorumMessage !QuorumMessage
    | -- |Implements 'sendBlock' of 'MonadBroadcast'.
      SendBlock !SignedBlock
    | -- |Implements 'onBlock' of 'MonadConsensusEvent'.
      OnBlock !(Block pv)
    | -- |Implements 'onFinalize' of 'MonadConsensusEvent'.
      OnFinalize !FinalizationEntry
    | -- |Implements 'onPendingLive' of 'MonadConsensusEvent'.
      OnPendingLive
    deriving (Eq, Show)

-- |Write event monoid. This is simply a list of events.
type TestWrite pv = [TestEvent pv]

-- |This type is used to derive instances of various block state classes for 'TestMonad'.
type PersistentBlockStateMonadHelper pv =
    PersistentBlockStateMonad
        pv
        (TestContext pv)
        (RWST (TestContext pv) (TestWrite pv) (TestState pv) IO)

-- |The 'TestMonad' type itself wraps 'RWST' over 'IO'.
-- The reader context is 'TestContext'.
-- The writer monoid is 'TestWrite', which is a list of 'TestEvent's, each of which represents a
-- callback from the consensus to an operation of 'MonadTimeout', 'MonadBroadcast', or
-- 'MonadConsensusEvent'.
-- The state is 'TestState', which includes the 'SkovData' and a map of the pending timer events.
newtype TestMonad (pv :: ProtocolVersion) a = TestMonad {runTestMonad' :: RWST (TestContext pv) (TestWrite pv) (TestState pv) IO a}
    deriving newtype (Functor, Applicative, Monad, MonadReader (TestContext pv), MonadIO, MonadThrow, MonadWriter (TestWrite pv))
    deriving
        (BlockStateTypes, ContractStateOperations, ModuleQuery)
        via (PersistentBlockStateMonadHelper pv)

makeLenses ''TestContext
makeLenses ''TestState

instance HasBakerContext (TestContext pv) where
    bakerContext = tcBakerContext

-- |Project the core genesis data from genesis data for consensus version 1.
genesisCore :: forall pv. (IsConsensusV1 pv, IsProtocolVersion pv) => GenesisData pv -> BaseV1.CoreGenesisParametersV1
genesisCore = case protocolVersion @pv of
    SP6 -> \(GDP6 P6.GDP6Initial{genesisCore = core}) -> core

-- |Run an operation in the 'TestMonad' with the given baker, time and genesis data.
-- This sets up a temporary blob store for the block state that is deleted after use.
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
        _tcMemoryLLDB <-
            liftIO . newIORef $!
                initialLowLevelDB genStoredBlock (_tsSkovData ^. persistentRoundStatus)
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
    (MemoryLLDBM pv (RWST (TestContext pv) (TestWrite pv) (TestState pv) IO))
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

instance MonadBroadcast (TestMonad pv) where
    sendTimeoutMessage = tell . (: []) . SendTimeoutMessage
    sendQuorumMessage = tell . (: []) . SendQuorumMessage
    sendBlock = tell . (: []) . SendBlock

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

instance MonadConsensusEvent (TestMonad pv) where
    onBlock = tell . (: []) . OnBlock . bpBlock
    onFinalize fe _ _ = tell [OnFinalize fe]
    onPendingLive = tell [OnPendingLive]

instance MonadLogger (TestMonad pv) where
    logEvent src lvl msg = do
        logger <- view tcLogger
        logger src lvl msg

-- |Get the currently-pending timers.
getPendingTimers :: TestMonad pv (Map.Map Integer (Timeout, TestMonad pv ()))
getPendingTimers = TestMonad (gets _tsPendingTimers)

-- |Clear all currently-pending timers.
clearPendingTimers :: TestMonad pv ()
clearPendingTimers = TestMonad (modify (\s -> s{_tsPendingTimers = Map.empty}))
