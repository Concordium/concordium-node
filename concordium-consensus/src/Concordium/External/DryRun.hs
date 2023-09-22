{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.External.DryRun where

import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
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
import qualified Concordium.Types.InvokeContract as InvokeContract
import Concordium.Types.Parameters
import Concordium.Types.Queries
import Concordium.Types.Transactions
import qualified Proto.V2.Concordium.Types as Proto

import qualified Concordium.Cost as Cost
import qualified Concordium.External as Ext
import Concordium.External.Helpers
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.TreeState as SkovV0
import Concordium.GlobalState.Types (BlockStateTypes (..))
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
import Concordium.Types.Execution

-- | An opaque rust vector.
data ForeignVec

type CopyToForeignVec = Ptr ForeignVec -> Ptr Word8 -> Int64 -> IO ()

-- | Boilerplate wrapper to invoke C callbacks.
foreign import ccall "dynamic" callCopyToForeignVecCallback :: FunPtr CopyToForeignVec -> CopyToForeignVec

writeProtoResponse ::
    ( ToProto (AsDryRunResponse a),
      Output (AsDryRunResponse a) ~ Proto.DryRunResponse
    ) =>
    (Ptr Word8 -> Int64 -> IO ()) ->
    a ->
    IO ()
writeProtoResponse writer p =
    BS.unsafeUseAsCStringLen encoded (\(ptr, len) -> writer (castPtr ptr) (fromIntegral len))
  where
    encoded = Proto.encodeMessage (toProto (DryRunResponse p))

writeProtoResponseEither ::
    ( ToProto (AsDryRunResponse a),
      Output (AsDryRunResponse a) ~ Either e Proto.DryRunResponse
    ) =>
    (Ptr Word8 -> Int64 -> IO ()) ->
    a ->
    IO (Either e ())
writeProtoResponseEither writer p = case toProto (DryRunResponse p) of
    Left e -> return $ Left e
    Right msg -> do
        BS.unsafeUseAsCStringLen
            (Proto.encodeMessage msg)
            (\(ptr, len) -> writer (castPtr ptr) (fromIntegral len))
        return $ Right ()

data DryRunReturnCode
    = OK
    | InternalError
    | OutOfEnergyQuota
    deriving (Enum)

returnCode :: DryRunReturnCode -> Int64
returnCode = fromIntegral . fromEnum

type StateConstraints m pv =
    ( BlockStateOperations m,
      BlockState m ~ HashedPersistentBlockState pv,
      UpdatableBlockState m ~ PersistentBlockState pv,
      MonadProtocolVersion m,
      MPV m ~ pv,
      MonadLogger m,
      MonadIO m
    )

data DryRunState pv = DryRunState
    { drsBlockState :: !(PersistentBlockState pv),
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

bscProtocolVersion :: EBlockStateContext finconf -> ProtocolVersion
bscProtocolVersion (EBlockStateContextV0 @_ @pv _ _) = demoteProtocolVersion $ protocolVersion @pv
bscProtocolVersion (EBlockStateContextV1 @_ @pv _ _) = demoteProtocolVersion $ protocolVersion @pv

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

data DryRunHandle = forall finconf.
      DryRunHandle
    { drhMVR :: !(MultiVersionRunner finconf),
      drhWriteToVector :: !CopyToForeignVec,
      drhBlockStateContext :: !(IORef (Maybe (EBlockStateContext finconf)))
    }

dryRunStart :: StablePtr Ext.ConsensusRunner -> FunPtr CopyToForeignVec -> IO (StablePtr DryRunHandle)
dryRunStart consensusPtr vecCallback = do
    Ext.ConsensusRunner mvr <- deRefStablePtr consensusPtr
    initialBSC <- newIORef Nothing
    dryRunPtr <-
        newStablePtr $!
            DryRunHandle
                { drhMVR = mvr,
                  drhWriteToVector = callCopyToForeignVecCallback vecCallback,
                  drhBlockStateContext = initialBSC
                }
    mvLog mvr External LLTrace $ "Dry run start " ++ show (castStablePtrToPtr dryRunPtr)
    return dryRunPtr

dryRunEnd :: StablePtr DryRunHandle -> IO ()
dryRunEnd dryRunPtr = do
    DryRunHandle{drhMVR = mvr} <- deRefStablePtr dryRunPtr
    mvLog mvr External LLTrace $ "Dry run end " ++ show (castStablePtrToPtr dryRunPtr)
    freeStablePtr dryRunPtr

dryRunLoadBlockState :: StablePtr DryRunHandle -> Word8 -> Ptr Word8 -> Ptr ForeignVec -> IO ()
dryRunLoadBlockState dryRunPtr bhiTag hashPtr outVec = do
    DryRunHandle{..} <- deRefStablePtr dryRunPtr
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
            writeProtoResponse (drhWriteToVector outVec) DryRunErrorBlockNotFound
        BQRBlock blkHash (ts, newBSC) -> do
            oldBlockStateContext <- atomicModifyIORef' drhBlockStateContext (Just newBSC,)
            forM_ oldBlockStateContext $ \oldBSC ->
                runWithEBlockStateContext drhMVR oldBSC $ \drs ->
                    liftIO $ writeIORef drs (error "Dry run state dropped")
            writeProtoResponse
                (drhWriteToVector outVec)
                ( DryRunSuccessBlockStateLoaded
                    { drsBlockHash = blkHash,
                      drsCurrentTimestamp = ts,
                      drsProtocolVersion = bscProtocolVersion newBSC
                    }
                )

-- | A dummy 'StateHash' value that can be used to create a 'HashedPersistentBlockState' where the
-- actual hash does not matter.
dummyStateHash :: StateHash
{-# NOINLINE dummyStateHash #-}
dummyStateHash = read "0000000000000000000000000000000000000000000000000000000000000000"

dryRunStateHelper ::
    (HasCallStack) =>
    StablePtr DryRunHandle ->
    Ptr ForeignVec ->
    ( forall finconf.
      (HasCallStack) =>
      MultiVersionRunner finconf ->
      (Ptr Word8 -> Int64 -> IO ()) ->
      EBlockStateContext finconf ->
      IO DryRunReturnCode
    ) ->
    IO Int64
dryRunStateHelper dryRunPtr outVec cont =
    returnCode <$> do
        DryRunHandle{..} <- deRefStablePtr dryRunPtr
        let writeOut = drhWriteToVector outVec
        readIORef drhBlockStateContext >>= \case
            Nothing -> do
                writeProtoResponse writeOut DryRunErrorNoState
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
                cont drhMVR writeOut bsc `catch` onExcept

dryRunGetAccountInfo ::
    StablePtr DryRunHandle ->
    -- | Account identifier tag.
    Word8 ->
    -- | Account identifier data.
    Ptr Word8 ->
    -- | Output vector.
    Ptr ForeignVec ->
    IO Int64
dryRunGetAccountInfo dryRunPtr acctTag acctPtr outVec = dryRunStateHelper dryRunPtr outVec $
    \mvr writeOut bsc -> do
        account <- decodeAccountIdentifierInput acctTag acctPtr
        macctInfo <- case bsc of
            EBlockStateContextV0{..} -> do
                DryRunState{..} <- liftIO $ readIORef bscState
                let fbs = HashedPersistentBlockState drsBlockState dummyStateHash
                st <- readIORef $ vc0State bsc0Config
                runMVR
                    ( SkovV0.evalSkovT
                        (getAccountInfoV0 account fbs)
                        (mvrSkovHandlers bsc0Config mvr)
                        (vc0Context bsc0Config)
                        st
                    )
                    mvr
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
                    mvr
        case macctInfo of
            Nothing -> do
                writeProtoResponse writeOut DryRunErrorAccountNotFound
            Just acctInfo ->
                writeProtoResponse writeOut (DryRunSuccessAccountInfo acctInfo)
        return OK

dryRunGetInstanceInfo ::
    StablePtr DryRunHandle ->
    -- | Contract index.
    Word64 ->
    -- | Contract subindex.
    Word64 ->
    -- | Output vector.
    Ptr ForeignVec ->
    IO Int64
dryRunGetInstanceInfo dryRunPtr contractIndex contractSubindex outVec =
    dryRunStateHelper dryRunPtr outVec $ \mvr writeOut bsc -> do
        res <- runWithEBlockStateContext mvr bsc $ \drsRef -> do
            DryRunState{..} <- liftIO $ readIORef drsRef
            let fbs = HashedPersistentBlockState drsBlockState dummyStateHash
            getInstanceInfoHelper ca fbs
        case res of
            Nothing -> writeProtoResponse writeOut DryRunErrorInstanceNotFound
            Just instInfo -> writeProtoResponse writeOut (DryRunSuccessInstanceInfo instInfo)
        return OK
  where
    ca = ContractAddress (ContractIndex contractIndex) (ContractSubindex contractSubindex)

-- | A return value other than 0 indicates that an internal error occurred and no response was
--  produced.
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
    outVec = dryRunStateHelper dryRunPtr outVec $
        \mvr writeOut bsc -> do
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
                          ccEnergy = Energy energy
                        }
            res <- runWithEBlockStateContext mvr bsc $ \drsRef -> do
                -- We "freeze" the block state using a dummy hash so that we do not recompute the
                -- state hash, since it is not required by invokeContract.
                DryRunState{..} <- liftIO $ readIORef drsRef
                let chainMeta = ChainMetadata drsTimestamp
                let fbs = HashedPersistentBlockState drsBlockState dummyStateHash
                InvokeContract.invokeContract context chainMeta fbs
            writeProtoResponseEither writeOut res >>= \case
                Left e -> do
                    mvLog mvr External LLError $
                        "An error occurred converting the result of a dry run invoke \
                        \instance to protobuf: "
                            ++ show e
                    return InternalError
                Right () -> return OK

dryRunSetTimestamp ::
    StablePtr DryRunHandle ->
    -- | The new timestamp.
    Word64 ->
    -- | Output vector.
    Ptr ForeignVec ->
    IO Int64
dryRunSetTimestamp dryRunPtr newTimestamp outVec = dryRunStateHelper dryRunPtr outVec $
    \mvr writeOut bsc -> do
        runWithEBlockStateContext mvr bsc $ \st ->
            liftIO $
                atomicModifyIORef' st $
                    \drs -> (drs{drsTimestamp = Timestamp newTimestamp}, ())
        writeProtoResponse writeOut DryRunSuccessTimestampSet
        return OK

dryRunMintToAccount ::
    StablePtr DryRunHandle ->
    -- | Account address to mint to.
    Ptr Word8 ->
    -- | Amount to mint.
    Word64 ->
    -- | Output vector.
    Ptr ForeignVec ->
    IO Int64
dryRunMintToAccount dryRunPtr senderPtr mintAmt outVec = dryRunStateHelper dryRunPtr outVec $
    \drhMVR writeOut bsc -> do
        sender <- decodeAccountAddress senderPtr
        runWithEBlockStateContext drhMVR bsc $ \drsRef -> do
            drs@DryRunState{..} <- liftIO $ readIORef drsRef
            bsoGetAccountIndex drsBlockState sender >>= \case
                Nothing -> do
                    liftIO $
                        writeProtoResponse
                            writeOut
                            DryRunErrorAccountNotFound
                    return OK
                Just account -> do
                    bsoSafeMintToAccount drsBlockState account (Amount mintAmt)
                        >>= liftIO . \case
                            Left safeMintAmount -> do
                                writeProtoResponse writeOut $
                                    DryRunErrorAmountOverLimit safeMintAmount
                                return OK
                            Right newState -> do
                                writeIORef drsRef drs{drsBlockState = newState}
                                writeProtoResponse writeOut DryRunSuccessMintedToAccount
                                return OK

dryRunTransaction ::
    StablePtr DryRunHandle ->
    -- | Sender account address.
    Ptr Word8 ->
    -- | Energy limit for executing the payload
    Word64 ->
    -- | Payload.
    Ptr Word8 ->
    -- | Payload length.
    Word64 ->
    -- | Output vector.
    Ptr ForeignVec ->
    IO Int64
dryRunTransaction dryRunPtr senderPtr energyLimit payloadPtr payloadLen outVec =
    dryRunStateHelper dryRunPtr outVec $ \mvr writeOut bsc -> do
        sender <- decodeAccountAddress senderPtr
        payloadBytes <- BS.packCStringLen (castPtr payloadPtr, fromIntegral payloadLen)
        let payload = EncodedPayload (SBS.toShort payloadBytes)
        let dummySignature =
                TransactionSignature
                    (Map.singleton 0 (Map.singleton 0 Sig.dummySignatureEd25519))
        let signatureCount = 1

        res <- runWithEBlockStateContext mvr bsc $ \drsRef -> do
            drs@DryRunState{..} <- liftIO $ readIORef drsRef
            let context =
                    Scheduler.ContextState
                        { _chainMetadata = ChainMetadata drsTimestamp,
                          _maxBlockEnergy = maxBound,
                          _accountCreationLimit = 0
                        }
            let schedulerState =
                    Scheduler.SchedulerState
                        { _ssNextIndex = 0,
                          _ssExecutionCosts = 0,
                          _ssEnergyUsed = 0,
                          _ssBlockState = drsBlockState
                        }
            let exec = do
                    srcAccount <- Scheduler.getStateAccount sender
                    case srcAccount of
                        Nothing ->
                            return . Right . toProto . DryRunResponse $
                                DryRunErrorAccountNotFound
                        Just src@(_, acc) -> do
                            nextNonce <- TVer.getNextAccountNonce acc
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
                            Scheduler.dispatchTransactionBody transaction src cost <&> \case
                                Nothing -> Left OutOfEnergyQuota
                                Just (res :: TransactionSummary' ValidResultWithReturn) ->
                                    case toProto (DryRunResponse res) of
                                        Left _ -> Left InternalError
                                        Right r -> Right r
            (res, ss) <- Scheduler.runSchedulerT exec context schedulerState
            liftIO $ writeIORef drsRef (drs{drsBlockState = ss ^. Scheduler.ssBlockState})
            return res
        case res of
            Left code -> return code
            Right message -> do
                let encoded = Proto.encodeMessage message
                BS.unsafeUseAsCStringLen
                    encoded
                    (\(ptr, len) -> writeOut (castPtr ptr) (fromIntegral len))
                return OK

foreign export ccall
    dryRunStart ::
        StablePtr Ext.ConsensusRunner ->
        FunPtr CopyToForeignVec ->
        IO (StablePtr DryRunHandle)

foreign export ccall
    dryRunEnd :: StablePtr DryRunHandle -> IO ()

foreign export ccall
    dryRunLoadBlockState :: StablePtr DryRunHandle -> Word8 -> Ptr Word8 -> Ptr ForeignVec -> IO ()

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
        -- | Output vector
        Ptr ForeignVec ->
        IO Int64
