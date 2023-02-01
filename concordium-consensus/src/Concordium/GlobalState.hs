{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |This module exposes functions for configuring the global state, such as initializing, migrating
-- and shutdown.
module Concordium.GlobalState where

import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.Trans.Reader hiding (ask)
import Data.Proxy

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Persistent.Account (newAccountCache)
import Concordium.GlobalState.Persistent.BlobStore (BlobStoreT (runBlobStoreT), closeBlobStore, createBlobStore, destroyBlobStore, loadBlobStore)
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as Modules
import Concordium.GlobalState.Persistent.Genesis
import Concordium.GlobalState.Persistent.TreeState
import Concordium.Logger
import Concordium.Types.ProtocolVersion

-- |Configuration that uses the disk implementation for both the tree state
-- and the block state
data GlobalStateConfig = GlobalStateConfig
    { dtdbRuntimeParameters :: !RuntimeParameters,
      dtdbTreeStateDirectory :: !FilePath,
      dtdbBlockStateFile :: !FilePath
    }

-- |Exceptions that can occur when initialising the global state.
data GlobalStateInitException
    = -- |Genesis data could not be used to construct initial state.
      InvalidGenesisData !String

instance Show GlobalStateInitException where
    show (InvalidGenesisData desc) =
        "Could not initialise state from genesis data: " ++ desc

instance Exception GlobalStateInitException

-- |The read-only context type associated with a global state configuration.
type GSContext pv = PersistentBlockStateContext pv

-- |The (mutable) state type associated with a global state configuration.
type GSState pv = SkovPersistentData pv

-- |Generate context and state from the initial configuration if the state
-- exists already. This may have 'IO' side effects to set up any necessary
-- storage. This may throw a 'GlobalStateInitException'.
--
-- The return value is 'Nothing' if the state could not be found. If the
-- state could be found, but could not be loaded, recovery is attempted,
-- and if that fails,  an exception is thrown.
--
-- Note that even if the state is successfully loaded it is not in a usable
-- state for an active consensus and must be activated before. Use
-- 'activateGlobalState' for that.
initialiseExistingGlobalState :: forall pv. IsProtocolVersion pv => SProtocolVersion pv -> GlobalStateConfig -> LogIO (Maybe (GSContext pv, GSState pv))
initialiseExistingGlobalState _ GlobalStateConfig{..} = do
    -- check if all the necessary database files exist
    existingDB <- checkExistingDatabase dtdbTreeStateDirectory dtdbBlockStateFile
    if existingDB
        then do
            logm <- ask
            liftIO $ do
                pbscAccountCache <- newAccountCache (rpAccountsCacheSize dtdbRuntimeParameters)
                pbscModuleCache <- Modules.newModuleCache (rpModulesCacheSize dtdbRuntimeParameters)
                pbscBlobStore <- loadBlobStore dtdbBlockStateFile
                let pbsc = PersistentBlockStateContext{..}
                skovData <-
                    runLoggerT (loadSkovPersistentData dtdbRuntimeParameters dtdbTreeStateDirectory pbsc) logm
                        `onException` closeBlobStore pbscBlobStore
                return (Just (pbsc, skovData))
        else return Nothing

-- |Migrate an existing global state. This is only intended to be used on a
-- protocol update and requires that the initial state for the new protocol
-- version is prepared (see @TreeState.storeFinalState@). This function will
-- construct a new active instance of global state by migrating state from
-- the existing instance. The existing instance is unchanged. It is assumed
-- that the existing instance is in a good state, i.e., all internal
-- invariants normally maintained during execution still hold. This function
-- additionally assumes the focus block is the last finalized block, and
-- there are no branches. This in particular implies that any transactions
-- in the transaction table are either finalized, or "received".
--
-- As well as block and tree state, this migrates the transaction table with
-- the auxiliary pending table. At present these are simply carried over,
-- but future protocol updates might need to do some migration of these as
-- well.
migrateExistingState ::
    (IsProtocolVersion pv, IsProtocolVersion oldpv) =>
    -- |The configuration.
    GlobalStateConfig ->
    -- |Global state context for the state we are migrating from.
    GSContext oldpv ->
    -- |The state of the chain we are migrating from. See documentation above for assumptions.
    GSState oldpv ->
    -- |Auxiliary migration data.
    StateMigrationParameters oldpv pv ->
    -- |Regenesis data for the new chain. This is in effect the genesis block of the new chain.
    Regenesis pv ->
    -- |The return value is the context and state for the new chain.
    LogIO (GSContext pv, GSState pv)
migrateExistingState GlobalStateConfig{..} oldPbsc oldState migration genData = do
    pbscBlobStore <- liftIO $ createBlobStore dtdbBlockStateFile
    pbscAccountCache <- liftIO $ newAccountCache (rpAccountsCacheSize dtdbRuntimeParameters)
    pbscModuleCache <- liftIO $ Modules.newModuleCache (rpModulesCacheSize dtdbRuntimeParameters)
    let pbsc = PersistentBlockStateContext{..}
    newInitialBlockState <- flip runBlobStoreT oldPbsc . flip runBlobStoreT pbsc $ do
        case _nextGenesisInitialState oldState of
            Nothing -> error "Precondition violation. Migration called in state without initial block state."
            Just initState -> do
                newState <- migratePersistentBlockState migration (hpbsPointers initState)
                Concordium.GlobalState.Persistent.BlockState.hashBlockState newState
    let initGS = do
            ser <- saveBlockState newInitialBlockState
            initialSkovPersistentData
                dtdbRuntimeParameters
                dtdbTreeStateDirectory
                (regenesisConfiguration genData)
                newInitialBlockState
                ser
                (_transactionTable oldState)
                (Just (_pendingTransactions oldState))
    isd <-
        runReaderT (runPersistentBlockStateMonad initGS) pbsc
            `onException` liftIO (destroyBlobStore pbscBlobStore)
    return (pbsc, isd)

-- |Initialise new global state with the given genesis. If the state already
-- exists this will raise an exception. It is not necessary to call 'activateGlobalState'
-- on the generated state, as this will establish the necessary invariants.
initialiseNewGlobalState :: IsProtocolVersion pv => GenesisData pv -> GlobalStateConfig -> LogIO (GSContext pv, GSState pv)
initialiseNewGlobalState genData GlobalStateConfig{..} = do
    pbscBlobStore <- liftIO $ createBlobStore dtdbBlockStateFile
    pbscAccountCache <- liftIO $ newAccountCache (rpAccountsCacheSize dtdbRuntimeParameters)
    pbscModuleCache <- liftIO $ Modules.newModuleCache (rpModulesCacheSize dtdbRuntimeParameters)
    let pbsc = PersistentBlockStateContext{..}
    let initGS = do
            logEvent GlobalState LLTrace "Creating persistent global state"
            result <- genesisState genData
            (pbs, genTT) <- case result of
                Left err -> logExceptionAndThrow GlobalState (InvalidGenesisData err)
                Right genState -> return genState
            logEvent GlobalState LLTrace "Writing persistent global state"
            ser <- saveBlockState pbs
            logEvent GlobalState LLTrace "Creating persistent global state context"
            initialSkovPersistentData dtdbRuntimeParameters dtdbTreeStateDirectory (genesisConfiguration genData) pbs ser genTT Nothing
    isd <-
        runReaderT (runPersistentBlockStateMonad initGS) pbsc
            `onException` liftIO (destroyBlobStore pbscBlobStore)
    return (pbsc, isd)

-- |Either initialise an existing state, or if it does not exist, initialise a new one with the given genesis.
initialiseGlobalState :: forall pv. IsProtocolVersion pv => GenesisData pv -> GlobalStateConfig -> LogIO (GSContext pv, GSState pv)
initialiseGlobalState gd cfg =
    initialiseExistingGlobalState (protocolVersion @pv) cfg >>= \case
        Nothing -> initialiseNewGlobalState gd cfg
        Just config -> return config

-- |Establish all the necessary invariants so that the state can be used by
-- consensus. This should only be called once per initialised state.
activateGlobalState :: IsProtocolVersion pv => Proxy pv -> GSContext pv -> GSState pv -> LogIO (GSState pv)
activateGlobalState _ = activateSkovPersistentData

-- |Shutdown the global state.
shutdownGlobalState :: SProtocolVersion pv -> GSContext pv -> GSState pv -> IO ()
shutdownGlobalState _ PersistentBlockStateContext{..} st = do
    closeBlobStore pbscBlobStore
    closeSkovPersistentData st
