{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides the interface for supporting the transaction dry-run facility in the
--  node API. A dry run is initiated by a call to 'dryRunStart', which returns a stable pointer to a
--  'DryRunHandle'. This handle should be passed to the various dry run operations before ultimately
--  being released with a call to 'dryRunEnd', which frees the resources associated with the handle.
--
--  Note, the operations on a single dry run handle are not thread safe, and must be serialised.
--  That is, you must wait for each dry run operation to return before invoking another operation
--  on the same handle. However, it is safe to concurrently invoke operations on different dry
--  run handles.
module Concordium.External.DryRun () where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont (ContT (..))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.ByteString.Unsafe as BS
import Data.IORef
import qualified Data.Map.Strict as Map
import qualified Data.ProtoLens as Proto
import Foreign
import GHC.Stack
import Lens.Micro.Platform

import qualified Concordium.Crypto.SignatureScheme as Sig
import Concordium.GRPC2
import Concordium.Logger
import Concordium.Types
import Concordium.Types.Execution
import qualified Concordium.Types.InvokeContract as InvokeContract
import Concordium.Types.Parameters
import Concordium.Types.Queries
import Concordium.Types.Transactions
import Concordium.Utils
import qualified Proto.V2.Concordium.Types as Proto

import qualified Concordium.Cost as Cost
import qualified Concordium.External as Ext
import Concordium.External.Helpers
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.TreeState as SkovV0
import Concordium.GlobalState.Types (BlockStateTypes (..))
import Concordium.ID.Types
import qualified Concordium.KonsensusV1.SkovMonad as SkovV1
import qualified Concordium.KonsensusV1.Types as SkovV1
import qualified Concordium.Kontrol as SkovV0
import Concordium.MultiVersion
import Concordium.Queries
import qualified Concordium.Scheduler as Scheduler
import qualified Concordium.Scheduler.Environment as Scheduler
import qualified Concordium.Scheduler.EnvironmentImplementation as Scheduler
import qualified Concordium.Scheduler.InvokeContract as InvokeContract
import qualified Concordium.Skov as SkovV0
import qualified Concordium.TransactionVerification as TVer

-- | The charge to the energy quota for loading a block state.
costLoadBlockState :: Energy
costLoadBlockState = 2000

-- | The charge to the energy quota for getting account info.
costGetAccountInfo :: Energy
costGetAccountInfo = 200

-- | The charge to the energy quota for getting contract instance info.
costGetInstanceInfo :: Energy
costGetInstanceInfo = 200

-- | The base charge to the energy quota for running invoke instance on a contract.
--  In addition, the energy cost of the execution will be charged.
costInvokeInstanceBase :: Energy
costInvokeInstanceBase = 200

-- | The charge to the energy quota for setting the block timestamp.
costSetTimestamp :: Energy
costSetTimestamp = 50

-- | The charge to the energy quota for minting to an account.
costMintToAccount :: Energy
costMintToAccount = 400

-- | The base cost for dry-running a transaction.
--  In addition, the energy cost of the execution will be charged.
costTransactionBase :: Energy
costTransactionBase = 400

-- | An opaque rust vector.
data ForeignVec

-- | A function that copies bytes to a rust vector.
type CopyToForeignVec = Ptr ForeignVec -> Ptr Word8 -> Int64 -> IO ()

-- | Boilerplate wrapper to invoke C callbacks.
foreign import ccall "dynamic" callCopyToForeignVecCallback :: FunPtr CopyToForeignVec -> CopyToForeignVec

-- | Write a value that represents a dry-run response.
writeProtoResponse ::
    ( ToProto (DryRunResponse a),
      Output (DryRunResponse a) ~ Proto.DryRunResponse
    ) =>
    -- | Writer function.
    (Ptr Word8 -> Int64 -> IO ()) ->
    -- | Remaining energy quota.
    Energy ->
    -- | Value to write.
    a ->
    IO ()
writeProtoResponse writer quotaRem response =
    BS.unsafeUseAsCStringLen encoded (\(ptr, len) -> writer (castPtr ptr) (fromIntegral len))
  where
    encoded =
        Proto.encodeMessage . toProto $
            DryRunResponse
                { drrResponse = response,
                  drrQuotaRemaining = quotaRem
                }

-- | Write a value that represents a dry-run response, but could fail in the conversion.
writeProtoResponseEither ::
    ( ToProto (DryRunResponse a),
      Output (DryRunResponse a) ~ Either e Proto.DryRunResponse
    ) =>
    -- | Writer function.
    (Ptr Word8 -> Int64 -> IO ()) ->
    -- | Remaining energy quota.
    Energy ->
    -- | Value to write.
    a ->
    IO (Either e ())
writeProtoResponseEither writer quotaRem response = case toProto resp of
    Left e -> return $ Left e
    Right msg -> do
        BS.unsafeUseAsCStringLen
            (Proto.encodeMessage msg)
            (\(ptr, len) -> writer (castPtr ptr) (fromIntegral len))
        return $ Right ()
  where
    resp =
        DryRunResponse
            { drrResponse = response,
              drrQuotaRemaining = quotaRem
            }

-- | Return codes for the dry run FFI.
data DryRunReturnCode
    = -- | The operation was successful.
      OK
    | -- | An internal error occurred.
      InternalError
    | -- | The operation could not be completed within the remaining energy quota.
      OutOfEnergyQuota
    deriving (Enum)

-- | Convert a 'DryRunReturnCode' to an 'Int64' that is actually sent across the FFI boundary.
returnCode :: DryRunReturnCode -> Int64
returnCode = fromIntegral . fromEnum

-- | The set of constraints that apply to the monad used for dry-run operations.
type StateConstraints m pv =
    ( BlockStateOperations m,
      BlockState m ~ HashedPersistentBlockState pv,
      UpdatableBlockState m ~ PersistentBlockState pv,
      MonadProtocolVersion m,
      MPV m ~ pv,
      MonadLogger m,
      MonadIO m
    )

-- | The current state of a dry-run session.
data DryRunState (pv :: ProtocolVersion) = DryRunState
    { -- | The current block state.
      drsBlockState :: !(PersistentBlockState pv),
      -- | The current timestamp.
      drsTimestamp :: !Timestamp
    }

-- | The block state context.
-- Implementation note: Storing the 'PersistentBlockState' under an 'IORef' is not necessary in
-- principle, as the state is implemented with an 'IORef' and is updated in-place. However, the
-- 'BlockStateOperations' interface does not strictly guarantee that operations are performed
-- in-place, so we use the 'IORef' to allow for that possibility.
data EBlockStateContext finconf
    = forall (pv :: ProtocolVersion).
        (StateConstraints (VersionedSkovV0M finconf pv) pv, IsConsensusV0 pv) =>
      EBlockStateContextV0
        { bsc0Config :: !(VersionedConfigurationV0 finconf pv),
          bscState :: !(IORef (DryRunState pv))
        }
    | forall (pv :: ProtocolVersion).
        (StateConstraints (VersionedSkovV1M finconf pv) pv, IsConsensusV1 pv) =>
      EBlockStateContextV1
        { bsc1Config :: !(VersionedConfigurationV1 finconf pv),
          bscState :: !(IORef (DryRunState pv))
        }

-- | Extract the protocol version from a block state context.
bscProtocolVersion :: EBlockStateContext finconf -> ProtocolVersion
bscProtocolVersion (EBlockStateContextV0 @_ @pv _ _) = demoteProtocolVersion $ protocolVersion @pv
bscProtocolVersion (EBlockStateContextV1 @_ @pv _ _) = demoteProtocolVersion $ protocolVersion @pv

-- | Run an operation on the dry-run state in a block state context.
--  The operation runs in a monad that is abstracted by 'StateConstraints', and can thus run on any
--  consensus version.
runWithEBlockStateContext ::
    MultiVersionRunner finconf ->
    EBlockStateContext finconf ->
    (forall m pv. (StateConstraints m pv) => IORef (DryRunState pv) -> m a) ->
    IO a
runWithEBlockStateContext mvr (EBlockStateContextV0 vc0 drs) operation = do
    st <- readIORef (vc0State vc0)
    runMVR
        ( SkovV0.evalSkovT
            (operation drs)
            (mvrSkovHandlers vc0 mvr)
            (vc0Context vc0)
            st
        )
        mvr
runWithEBlockStateContext mvr (EBlockStateContextV1 vc1 drs) operation = do
    st <- readIORef (vc1State vc1)
    runMVR
        ( SkovV1.evalSkovT
            (operation drs)
            (vc1Context vc1)
            st
        )
        mvr

-- | Handle that identifies a particular dry-run session.
data DryRunHandle
    = forall finconf.
      DryRunHandle
    { -- | Wrap the multi-version runner from the consensus runner.
      drhMVR :: !(MultiVersionRunner finconf),
      -- | Callback for writing to a Rust vector.
      drhWriteToVector :: !CopyToForeignVec,
      -- | Reference to the current block state context.
      drhBlockStateContext :: !(IORef (Maybe (EBlockStateContext finconf))),
      -- | An 'IORef' that records the remaining energy quota for this dry run.
      drhEnergyQuota :: !(IORef Energy)
    }

-- | Start a dry-run session, creating a new dry-run handle.
-- Once completed, the handle must be disposed by calling 'dryRunEnd'.
dryRunStart ::
    -- | Pointer to the consensus runner.
    StablePtr Ext.ConsensusRunner ->
    -- | Callback to use for writing data to a vector.
    FunPtr CopyToForeignVec ->
    -- | The total energy quota to use for the dry-run session.
    Word64 ->
    IO (StablePtr DryRunHandle)
dryRunStart consensusPtr vecCallback energyQuota = do
    Ext.ConsensusRunner mvr <- deRefStablePtr consensusPtr
    initialBSC <- newIORef Nothing
    quotaRef <- newIORef $ Energy energyQuota
    dryRunPtr <-
        newStablePtr $!
            DryRunHandle
                { drhMVR = mvr,
                  drhWriteToVector = callCopyToForeignVecCallback vecCallback,
                  drhBlockStateContext = initialBSC,
                  drhEnergyQuota = quotaRef
                }
    mvLog mvr External LLTrace $ "Dry run start " ++ show (castStablePtrToPtr dryRunPtr)
    return dryRunPtr

-- | Finish a dry-run session, allowing the resources associated with it to be freed.
-- The handle must not be used after calling 'dryRunEnd'.
dryRunEnd ::
    -- | The dry-run handle.
    StablePtr DryRunHandle ->
    IO ()
dryRunEnd dryRunPtr = do
    DryRunHandle{drhMVR = mvr} <- deRefStablePtr dryRunPtr
    mvLog mvr External LLTrace $ "Dry run end " ++ show (castStablePtrToPtr dryRunPtr)
    freeStablePtr dryRunPtr

-- | Deduct the specified amount from the energy quota if sufficient quota is remaining.
--  If it is sufficient, the supplied continuation is executed. Otherwise, 'OutOfEnergyQuota'
--  is returned.
tryTickQuota :: IORef Energy -> Energy -> (Energy -> IO DryRunReturnCode) -> IO DryRunReturnCode
tryTickQuota quotaRef amount cont = do
    (inQuota, remQuota) <- atomicModifyIORef' quotaRef $ \q ->
        if q >= amount then (q - amount, (True, q - amount)) else (q, (False, q))
    if inQuota then cont remQuota else return OutOfEnergyQuota

-- | Load the state of a particular block in the dry-run session, and use its timestamp as the
--  current timestamp for the session.
dryRunLoadBlockState ::
    -- | The dry-run handle.
    StablePtr DryRunHandle ->
    -- | Tag identifying the type of block hash input.
    Word8 ->
    -- | Payload data for the block hash input.
    Ptr Word8 ->
    -- | Vector in which to write the response.
    Ptr ForeignVec ->
    IO Int64
dryRunLoadBlockState dryRunPtr bhiTag hashPtr outVec =
    returnCode <$> do
        DryRunHandle{..} <- deRefStablePtr dryRunPtr
        tryTickQuota drhEnergyQuota costLoadBlockState $ \quotaRem -> do
            input <- decodeBlockHashInput bhiTag hashPtr
            res <-
                runMVR
                    ( liftSkovQueryBHIAndVersion
                        ( \vc0 bp -> do
                            drsTimestamp <- SkovV0.getSlotTimestamp $ SkovV0.blockSlot bp
                            drsBlockState <- thawBlockState =<< blockState bp
                            drs <- liftIO . newIORef $ DryRunState{..}
                            return (drsTimestamp, EBlockStateContextV0 vc0 drs)
                        )
                        ( \vc1 bp _ -> do
                            let drsTimestamp = SkovV1.blockTimestamp bp
                            drsBlockState <- thawBlockState =<< blockState bp
                            drs <- liftIO . newIORef $ DryRunState{..}
                            return (drsTimestamp, EBlockStateContextV1 vc1 drs)
                        )
                        input
                    )
                    drhMVR
            case res of
                BQRNoBlock -> do
                    writeProtoResponse (drhWriteToVector outVec) quotaRem DryRunErrorBlockNotFound
                BQRBlock blkHash (ts, newBSC) -> do
                    oldBlockStateContext <- atomicModifyIORef' drhBlockStateContext (Just newBSC,)
                    forM_ oldBlockStateContext $ \oldBSC ->
                        runWithEBlockStateContext drhMVR oldBSC $ \drs ->
                            liftIO $ writeIORef drs (error "Dry run state dropped")
                    writeProtoResponse
                        (drhWriteToVector outVec)
                        quotaRem
                        ( DryRunSuccessBlockStateLoaded
                            { drsBlockHash = blkHash,
                              drsCurrentTimestamp = ts,
                              drsProtocolVersion = bscProtocolVersion newBSC
                            }
                        )
            return OK

-- | A dummy 'StateHash' value that can be used to create a 'HashedPersistentBlockState' where the
-- actual hash does not matter.
-- This is used to invoke query operations that require a 'HashedPersistentBlockState'.
dummyStateHash :: StateHash
{-# NOINLINE dummyStateHash #-}
dummyStateHash = read "0000000000000000000000000000000000000000000000000000000000000000"

-- | Wrapper for the data passed to the continuation of 'dryRunStateHelper'.
data StateHelperInfo finconf = StateHelperInfo
    { -- | The multi-version runner from the dry run handle.
      shiMVR :: MultiVersionRunner finconf,
      -- | Write bytes at the specified pointer into the output vector.
      shiWriteOut :: Ptr Word8 -> Int64 -> IO (),
      -- | The block state context obtained from the dry run handle.
      shiBSC :: EBlockStateContext finconf,
      -- | Reference used to store the remaining energy quota.
      shiQuotaRef :: IORef Energy,
      -- | The remaining energy quota after charging the base cost.
      shiQuotaRem :: Energy
    }

-- | Helper function for getting the dry run state context and charging the base cost to the
-- energy quota. This first attempts to charge the specified energy cost to the quota. If that
-- fails, the function returns signalling 'OutOfEnergyQuota'. It then attempts to read the current
-- block state context. If that fails, it writes out a 'DryRunErrorNoState' response to the output
-- vector and returns signalling 'OK'. Otherwise, it executes the continuation with the
-- 'StateHelperInfo'. If an exception occurs in doing so, it is caught and logged, and the function
-- returns signalling 'InternalError'. Otherwise, the function returns with the return code of the
-- continuation.
dryRunStateHelper ::
    (HasCallStack) =>
    -- | Pointer to the dry run handle.
    StablePtr DryRunHandle ->
    -- | Pointer to the output vector.
    Ptr ForeignVec ->
    -- | Base energy to charge for this operation.
    Energy ->
    -- | Continuation for executing the operation.
    ( forall finconf.
      (HasCallStack) =>
      StateHelperInfo finconf ->
      IO DryRunReturnCode
    ) ->
    -- | The return code (see 'DryRunReturnCode').
    IO Int64
dryRunStateHelper dryRunPtr outVec baseCost cont =
    returnCode <$> do
        DryRunHandle{..} <- deRefStablePtr dryRunPtr
        let writeOut = drhWriteToVector outVec
        tryTickQuota drhEnergyQuota baseCost $ \quotaRem ->
            readIORef drhBlockStateContext >>= \case
                Nothing -> do
                    writeProtoResponse writeOut quotaRem DryRunErrorNoState
                    return OK
                Just bsc -> do
                    let onExcept :: SomeException -> IO DryRunReturnCode
                        onExcept e = do
                            mvLog drhMVR External LLError $
                                "Error occurred in dry run operation: "
                                    ++ displayException e
                                    ++ "\n"
                                    ++ prettyCallStack callStack
                            return InternalError
                    let shi =
                            StateHelperInfo
                                { shiMVR = drhMVR,
                                  shiWriteOut = writeOut,
                                  shiBSC = bsc,
                                  shiQuotaRef = drhEnergyQuota,
                                  shiQuotaRem = quotaRem
                                }
                    cont shi `catch` onExcept

-- | Look up information on a particular account in the current dry-run state.
dryRunGetAccountInfo ::
    -- | Dry-run handle.
    StablePtr DryRunHandle ->
    -- | Account identifier tag.
    Word8 ->
    -- | Account identifier data.
    Ptr Word8 ->
    -- | Output vector.
    Ptr ForeignVec ->
    -- | The return code (see 'DryRunReturnCode').
    IO Int64
dryRunGetAccountInfo dryRunPtr acctTag acctPtr outVec = dryRunStateHelper dryRunPtr outVec costGetAccountInfo $
    \StateHelperInfo{..} -> do
        account <- decodeAccountIdentifierInput acctTag acctPtr
        macctInfo <- case shiBSC of
            EBlockStateContextV0{..} -> do
                DryRunState{..} <- liftIO $ readIORef bscState
                let fbs = HashedPersistentBlockState drsBlockState dummyStateHash
                st <- readIORef $ vc0State bsc0Config
                runMVR
                    ( SkovV0.evalSkovT
                        (getAccountInfoV0 account fbs)
                        (mvrSkovHandlers bsc0Config shiMVR)
                        (vc0Context bsc0Config)
                        st
                    )
                    shiMVR
            EBlockStateContextV1{..} -> do
                DryRunState{..} <- liftIO $ readIORef bscState
                let fbs = HashedPersistentBlockState drsBlockState dummyStateHash
                st <- readIORef $ vc1State bsc1Config
                runMVR
                    ( SkovV1.evalSkovT
                        (getAccountInfoV1 account fbs)
                        (vc1Context bsc1Config)
                        st
                    )
                    shiMVR
        case macctInfo of
            Nothing -> do
                writeProtoResponse shiWriteOut shiQuotaRem DryRunErrorAccountNotFound
            Just acctInfo ->
                writeProtoResponse shiWriteOut shiQuotaRem (DryRunSuccessAccountInfo acctInfo)
        return OK

-- | Look up information on a particular smart contract instance in the current dry-run state.
dryRunGetInstanceInfo ::
    -- | Dry-run handle.
    StablePtr DryRunHandle ->
    -- | Contract index.
    Word64 ->
    -- | Contract subindex.
    Word64 ->
    -- | Output vector.
    Ptr ForeignVec ->
    IO Int64
dryRunGetInstanceInfo dryRunPtr contractIndex contractSubindex outVec =
    dryRunStateHelper dryRunPtr outVec costGetInstanceInfo $ \StateHelperInfo{..} -> do
        res <- runWithEBlockStateContext shiMVR shiBSC $ \drsRef -> do
            DryRunState{..} <- liftIO $ readIORef drsRef
            let fbs = HashedPersistentBlockState drsBlockState dummyStateHash
            getInstanceInfoHelper ca fbs
        case res of
            Nothing -> writeProtoResponse shiWriteOut shiQuotaRem DryRunErrorInstanceNotFound
            Just instInfo -> writeProtoResponse shiWriteOut shiQuotaRem (DryRunSuccessInstanceInfo instInfo)
        return OK
  where
    ca = ContractAddress (ContractIndex contractIndex) (ContractSubindex contractSubindex)

-- | Invoke an entrypoint on a smart contract instance in the current dry-run state.
-- No changes to the state are retained at the completion of this operation.
dryRunInvokeInstance ::
    StablePtr DryRunHandle ->
    -- | Contract index.
    Word64 ->
    -- | Contract subindex.
    Word64 ->
    -- | Invoker address tag.
    Word8 ->
    -- | Invoker account address pointer.
    Ptr Word8 ->
    -- | Invoker contract index.
    Word64 ->
    -- | Invoker contract subindex.
    Word64 ->
    -- | Amount.
    Word64 ->
    -- | ReceiveName pointer.
    Ptr Word8 ->
    -- | ReceiveName length.
    Word32 ->
    -- | Parameter pointer.
    Ptr Word8 ->
    -- | Parameter length.
    Word32 ->
    -- | Energy
    Word64 ->
    -- | Output vector.
    Ptr ForeignVec ->
    -- | The return code (see 'DryRunReturnCode').
    IO Int64
dryRunInvokeInstance
    dryRunPtr
    contractIndex
    contractSubindex
    invokerAddressType
    invokerAccountAddressPtr
    invokerContractIndex
    invokerContractSubindex
    amount
    receiveNamePtr
    receiveNameLen
    parameterPtr
    parameterLen
    energy
    outVec = dryRunStateHelper dryRunPtr outVec costInvokeInstanceBase $ \StateHelperInfo{..} -> do
        maybeInvoker <- case invokerAddressType of
            0 -> return Nothing
            1 -> Just . AddressAccount <$> decodeAccountAddress invokerAccountAddressPtr
            _ ->
                return $
                    Just $
                        AddressContract $
                            ContractAddress
                                (ContractIndex invokerContractIndex)
                                (ContractSubindex invokerContractSubindex)
        method <- decodeReceiveName receiveNamePtr receiveNameLen
        parameter <- decodeParameter parameterPtr parameterLen

        let (energyLimit, quotaLimiting)
                | Energy energy > shiQuotaRem = (shiQuotaRem, True)
                | otherwise = (Energy energy, False)
        let context =
                InvokeContract.ContractContext
                    { ccInvoker = maybeInvoker,
                      ccContract =
                        ContractAddress
                            (ContractIndex contractIndex)
                            (ContractSubindex contractSubindex),
                      ccAmount = Amount amount,
                      ccMethod = method,
                      ccParameter = parameter,
                      ccEnergy = energyLimit
                    }
        res <- runWithEBlockStateContext shiMVR shiBSC $ \drsRef -> do
            -- We "freeze" the block state using a dummy hash so that we do not recompute the
            -- state hash, since it is not required by invokeContract.
            DryRunState{..} <- liftIO $ readIORef drsRef
            let chainMeta = ChainMetadata drsTimestamp
            let fbs = HashedPersistentBlockState drsBlockState dummyStateHash
            InvokeContract.invokeContract context chainMeta fbs
        -- Charge the energy used to the quota.
        let newQuotaRem = shiQuotaRem - InvokeContract.rcrUsedEnergy res
        writeIORef shiQuotaRef newQuotaRem
        case res of
            -- If we ran out of energy and hit the quota, then we return an error code and
            -- do not produce a result.
            InvokeContract.Failure{rcrReason = OutOfEnergy}
                | quotaLimiting -> return OutOfEnergyQuota
            _ ->
                writeProtoResponseEither shiWriteOut newQuotaRem res >>= \case
                    Left e -> do
                        mvLog shiMVR External LLError $
                            "An error occurred converting the result of a dry run invoke \
                            \instance to protobuf: "
                                ++ show e
                        return InternalError
                    Right () -> return OK

-- | Set the current block time for the dry-run session.
dryRunSetTimestamp ::
    -- | Dry-run handle.
    StablePtr DryRunHandle ->
    -- | The new timestamp (in ms since the epoch).
    Word64 ->
    -- | Output vector.
    Ptr ForeignVec ->
    -- | The return code (see 'DryRunReturnCode').
    IO Int64
dryRunSetTimestamp dryRunPtr newTimestamp outVec = dryRunStateHelper dryRunPtr outVec costSetTimestamp $
    \StateHelperInfo{..} -> do
        runWithEBlockStateContext shiMVR shiBSC $ \st ->
            liftIO $
                atomicModifyIORef' st $
                    \drs -> (drs{drsTimestamp = Timestamp newTimestamp}, ())
        writeProtoResponse shiWriteOut shiQuotaRem DryRunSuccessTimestampSet
        return OK

-- | Mint a specified amount and credit it to the specified account.
dryRunMintToAccount ::
    -- | Dry-run handle.
    StablePtr DryRunHandle ->
    -- | Account address to mint to.
    Ptr Word8 ->
    -- | Amount to mint.
    Word64 ->
    -- | Output vector.
    Ptr ForeignVec ->
    -- | The return code (see 'DryRunReturnCode').
    IO Int64
dryRunMintToAccount dryRunPtr senderPtr mintAmt outVec = dryRunStateHelper dryRunPtr outVec costMintToAccount $
    \StateHelperInfo{..} -> do
        sender <- decodeAccountAddress senderPtr
        runWithEBlockStateContext shiMVR shiBSC $ \drsRef -> do
            drs@DryRunState{..} <- liftIO $ readIORef drsRef
            bsoGetAccountIndex drsBlockState sender >>= \case
                Nothing -> do
                    liftIO $
                        writeProtoResponse
                            shiWriteOut
                            shiQuotaRem
                            DryRunErrorAccountNotFound
                    return OK
                Just account -> do
                    bsoMintToAccount drsBlockState account (Amount mintAmt)
                        >>= liftIO . \case
                            Left safeMintAmount -> do
                                writeProtoResponse shiWriteOut shiQuotaRem $
                                    DryRunErrorAmountOverLimit safeMintAmount
                                return OK
                            Right newState -> do
                                writeIORef drsRef drs{drsBlockState = newState}
                                writeProtoResponse
                                    shiWriteOut
                                    shiQuotaRem
                                    DryRunSuccessMintedToAccount
                                return OK

-- | Run a transaction in the current dry-run state, updating the state if it succeeds.
dryRunTransaction ::
    -- | Dry run handle.
    StablePtr DryRunHandle ->
    -- | Sender account address.
    Ptr Word8 ->
    -- | Energy limit for executing the payload
    Word64 ->
    -- | Payload.
    Ptr Word8 ->
    -- | Payload length.
    Word64 ->
    -- | Array of (credential, key) pairs.
    Ptr Word8 ->
    -- | Number of signatures.
    Word64 ->
    -- | Output vector.
    Ptr ForeignVec ->
    -- | The return code (see 'DryRunReturnCode').
    IO Int64
dryRunTransaction dryRunPtr senderPtr energyLimit payloadPtr payloadLen sigPairs sigCount outVec =
    dryRunStateHelper dryRunPtr outVec costTransactionBase $ \StateHelperInfo{..} -> do
        sender <- decodeAccountAddress senderPtr
        payloadBytes <- BS.packCStringLen (castPtr payloadPtr, fromIntegral payloadLen)
        let payload = EncodedPayload (SBS.toShort payloadBytes)
        sigMap <-
            if sigCount == 0
                then return (Map.singleton 0 (Map.singleton 0 Sig.dummySignatureEd25519))
                else do
                    let addSigs i m
                            | i < fromIntegral sigCount = do
                                cred <- CredentialIndex <$> peekByteOff sigPairs (2 * i)
                                key <- KeyIndex <$> peekByteOff sigPairs (2 * i + 1)
                                addSigs (i + 1) $!
                                    m
                                        & at' cred
                                            . nonEmpty
                                            . at' key
                                            ?~ Sig.dummySignatureEd25519
                            | otherwise = return m
                    addSigs 0 Map.empty
        let dummySignature = TransactionSignature sigMap
        let signatureCount = getTransactionNumSigs dummySignature

        res <- runWithEBlockStateContext shiMVR shiBSC $ \(drsRef :: IORef (DryRunState pv)) -> do
            drs@DryRunState{..} <- liftIO $ readIORef drsRef
            let context =
                    Scheduler.ContextState
                        { _chainMetadata = ChainMetadata drsTimestamp,
                          _maxBlockEnergy = shiQuotaRem,
                          _accountCreationLimit = 0
                        }
            let schedulerState =
                    Scheduler.SchedulerState
                        { _ssNextIndex = 0,
                          _ssExecutionCosts = 0,
                          _ssEnergyUsed = 0,
                          _ssBlockState = drsBlockState
                        }
            let exec = flip runContT return $ do
                    let exit = ContT . const . return . Right . toProto
                    srcAccount <- lift $ Scheduler.getStateAccount sender
                    case srcAccount of
                        Nothing ->
                            return . Right . toProto $
                                DryRunResponse
                                    DryRunErrorAccountNotFound
                                    shiQuotaRem
                        Just src@(_, acc) -> do
                            nextNonce <- lift $ TVer.getNextAccountNonce acc
                            let header =
                                    TransactionHeader
                                        { thSender = sender,
                                          thNonce = nextNonce,
                                          thEnergyAmount = Energy energyLimit,
                                          thPayloadSize = payloadSize payload,
                                          thExpiry = fromIntegral $ drsTimestamp `div` 1000 + 1
                                        }
                            let transaction = makeAccountTransaction dummySignature header payload
                            let cost =
                                    Cost.baseCost
                                        (getTransactionHeaderPayloadSize header)
                                        signatureCount
                            -- Check that the energy amount covers the base cost of checking
                            -- the transaction header.
                            when (thEnergyAmount header < cost) $
                                exit $
                                    DryRunResponse
                                        (DryRunErrorEnergyInsufficient cost)
                                        shiQuotaRem
                            -- Check that the sender account has sufficient funds to cover the
                            -- deposit amount.
                            accBalance <- lift $ TVer.getAccountAvailableAmount acc
                            depositAmount <- lift $ TVer.energyToCcd (thEnergyAmount header)
                            when (accBalance < depositAmount) $
                                exit $
                                    DryRunResponse
                                        DryRunErrorBalanceInsufficient
                                            { dreRequiredAmount = depositAmount,
                                              dreAvailableAmount = accBalance
                                            }
                                        shiQuotaRem

                            lift (Scheduler.dispatchTransactionBody transaction src cost) >>= \case
                                Nothing -> do
                                    lift . lift . liftIO $ writeIORef shiQuotaRef 0
                                    return $ Left OutOfEnergyQuota
                                Just (res :: TransactionSummary' ValidResultWithReturn) -> do
                                    let newQuotaRem = shiQuotaRem - tsEnergyCost res
                                    lift . lift . liftIO $
                                        writeIORef shiQuotaRef newQuotaRem
                                    let mInitParam = do
                                            let spv = protocolVersion @pv
                                            InitContract{..} <- decodePayload spv payload ^? _Right
                                            return icParam
                                    case supplementEvents (addInitializeParameter mInitParam) res of
                                        Nothing -> do
                                            -- This should not occur, since 'mInitParam' is only
                                            -- needed if the transaction is an 'InitContract', in
                                            -- which case, 'mInitParam' will not be 'Nothing'.
                                            lift . logEvent External LLError $
                                                "DryRun: a contract initialization parameter was \
                                                \expected but not found"
                                            return $ Left InternalError
                                        Just res' -> case toProto (DryRunResponse res' newQuotaRem) of
                                            Left err -> do
                                                lift . logEvent External LLError $
                                                    "DryRun: failed to serialize response: "
                                                        ++ show err
                                                return $ Left InternalError
                                            Right r ->
                                                return $ Right r
            (res, ss) <- Scheduler.runSchedulerT exec context schedulerState
            liftIO $ writeIORef drsRef (drs{drsBlockState = ss ^. Scheduler.ssBlockState})
            return res
        case res of
            Left code -> return code
            Right message -> do
                let encoded = Proto.encodeMessage message
                BS.unsafeUseAsCStringLen
                    encoded
                    (\(ptr, len) -> shiWriteOut (castPtr ptr) (fromIntegral len))
                return OK

foreign export ccall
    dryRunStart ::
        StablePtr Ext.ConsensusRunner ->
        FunPtr CopyToForeignVec ->
        Word64 ->
        IO (StablePtr DryRunHandle)

foreign export ccall
    dryRunEnd :: StablePtr DryRunHandle -> IO ()

foreign export ccall
    dryRunLoadBlockState :: StablePtr DryRunHandle -> Word8 -> Ptr Word8 -> Ptr ForeignVec -> IO Int64

foreign export ccall
    dryRunGetAccountInfo ::
        StablePtr DryRunHandle ->
        -- | Account identifier tag.
        Word8 ->
        -- | Account identifier data.
        Ptr Word8 ->
        -- | Output vector.
        Ptr ForeignVec ->
        IO Int64

foreign export ccall
    dryRunGetInstanceInfo ::
        StablePtr DryRunHandle ->
        -- | Contract index.
        Word64 ->
        -- | Contract subindex.
        Word64 ->
        -- | Output vector.
        Ptr ForeignVec ->
        IO Int64

foreign export ccall
    dryRunInvokeInstance ::
        StablePtr DryRunHandle ->
        -- | Contract index.
        Word64 ->
        -- | Contract subindex.
        Word64 ->
        -- | Invoker address tag.
        Word8 ->
        -- | Invoker account address pointer.
        Ptr Word8 ->
        -- | Invoker contract index.
        Word64 ->
        -- | Invoker contract subindex.
        Word64 ->
        -- | Amount.
        Word64 ->
        -- | ReceiveName pointer.
        Ptr Word8 ->
        -- | ReceiveName length.
        Word32 ->
        -- | Parameter pointer.
        Ptr Word8 ->
        -- | Parameter length.
        Word32 ->
        -- | Energy
        Word64 ->
        -- | Output vector.
        Ptr ForeignVec ->
        IO Int64

foreign export ccall
    dryRunSetTimestamp ::
        StablePtr DryRunHandle ->
        -- | The new timestamp.
        Word64 ->
        -- | Output vector.
        Ptr ForeignVec ->
        IO Int64

foreign export ccall
    dryRunMintToAccount ::
        StablePtr DryRunHandle ->
        -- | Account address to mint to.
        Ptr Word8 ->
        -- | Amount to mint.
        Word64 ->
        -- | Output vector.
        Ptr ForeignVec ->
        IO Int64

foreign export ccall
    dryRunTransaction ::
        StablePtr DryRunHandle ->
        -- | Sender account address (32 bytes)
        Ptr Word8 ->
        -- | Energy limit for executing the payload
        Word64 ->
        -- | Payload.
        Ptr Word8 ->
        -- | Payload length.
        Word64 ->
        -- | Array of (credential, key) pairs.
        Ptr Word8 ->
        -- | Number of signatures.
        Word64 ->
        -- | Output vector
        Ptr ForeignVec ->
        IO Int64
