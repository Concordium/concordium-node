{-# LANGUAGE ForeignFunctionInterface, LambdaCase, RecordWildCards, ScopedTypeVariables, OverloadedStrings, RankNTypes, TypeFamilies #-}
module Concordium.External where

import Foreign
import Foreign.C

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Unsafe as BS
import Data.Serialize
import Data.Serialize.Put as P
import qualified Data.Aeson as AE
import Data.Foldable(forM_)
import Text.Read(readMaybe)
import Control.Exception
import Control.Monad.State.Class(MonadState)

import qualified Data.Text.Lazy as LT
import qualified Data.Aeson.Text as AET
import Data.Aeson(Value(Null))

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Data.FixedByteString as FBS

import Concordium.Types
import Concordium.ID.Types
import qualified Concordium.Types.Acorn.Core as Core
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.Block
import Concordium.GlobalState.Basic.Block hiding (getBlock)
import qualified Concordium.GlobalState.Basic.Block as BasicBlock
import Concordium.GlobalState.Finalization(FinalizationIndex(..),FinalizationRecord)
import Concordium.GlobalState.Persistent.BlockState(PersistentBlockState)
import Concordium.GlobalState.Persistent.TreeState(PersistentBlockPointer, _bpBlock)
import qualified Concordium.GlobalState.TreeState as TS
import qualified Concordium.GlobalState.BlockState as TS


import Concordium.Scheduler.Utils.Init.Example (initialPersistentState)

import Concordium.Runner
import Concordium.Skov hiding (receiveTransaction, getBirkParameters)
import Concordium.Afgjort.Finalize (FinalizationPseudoMessage(..))
import Concordium.Logger
import Concordium.TimeMonad
import Concordium.Skov.CatchUp (CatchUpStatus)

import qualified Concordium.Getters as Get
import qualified Concordium.Startup as S

-- |A 'PeerID' identifies peer at the p2p layer.
type PeerID = Word64

-- |A 'BlockReference' is a pointer to a block hash as a sequence of 32 bytes.
type BlockReference = Ptr Word8

jsonValueToCString :: Value -> IO CString
jsonValueToCString = newCString . LT.unpack . AET.encodeToLazyText

-- |Use a 'BlockHash' as a 'BlockReference'.  The 'BlockReference' may not
-- be valid after the function has returned.
withBlockReference :: BlockHash -> (BlockReference -> IO a) -> IO a
withBlockReference (Hash.Hash fbs) = FBS.withPtrReadOnly fbs

-- |Use a 'TransactionHash' as a 'Ptr Word8'. The pointer may not be valid after
-- the function has returned.
withTxReference :: TransactionHash -> (Ptr Word8 -> IO a) -> IO a
withTxReference (Hash.Hash fbs) = FBS.withPtrReadOnly fbs

-- |Create a 'BlockHash' from a 'BlockReference'.  This creates a copy
-- of the block hash.
blockReferenceToBlockHash :: BlockReference -> IO BlockHash
blockReferenceToBlockHash src = Hash.Hash <$> FBS.create cp
    where
        cp dest = copyBytes dest src (FBS.fixedLength (undefined :: Hash.DigestSize))


-- |Callback for handling genesis data.
type GenesisDataCallback = CString -> Int64 -> IO ()
foreign import ccall "dynamic" invokeGenesisDataCallback :: FunPtr GenesisDataCallback -> GenesisDataCallback
callGenesisDataCallback :: FunPtr GenesisDataCallback -> BS.ByteString -> IO ()
callGenesisDataCallback cb bs = BS.useAsCStringLen bs $ \(cdata, clen) -> (invokeGenesisDataCallback cb) cdata (fromIntegral clen)

-- |Callback for handling a generated baker identity.
-- The first argument is the identity of the baker (TODO: this should really by a 'Word64').
-- The second argument is the pointer to the data, and the third is the length of the data.
type BakerIdentityCallback = Int64 -> CString -> Int64 -> IO ()
foreign import ccall "dynamic" invokeBakerIdentityCallback :: FunPtr BakerIdentityCallback -> BakerIdentityCallback
callBakerIdentityCallback :: FunPtr BakerIdentityCallback -> Word64 -> BS.ByteString -> IO ()
callBakerIdentityCallback cb bix bs = BS.useAsCStringLen bs $ \(cdata, clen) -> invokeBakerIdentityCallback cb (fromIntegral bix) cdata (fromIntegral clen)

-- |Generate genesis data given a genesis time (in seconds since the UNIX epoch) and
-- number of bakers.  This function is deterministic: calling with the same genesis
-- time and number of bakers should generate the same genesis data and baker identities.
-- Returns
-- - 9 if cryptographic parameters could not be loaded,
-- - 10 if identity providers could not be loaded
-- - 0 if data was generated.
makeGenesisData ::
    Timestamp -- ^Genesis time
    -> Word64 -- ^Number of bakers
    -> CString -- ^File name of the data with cryptographic providers. Null-terminated.
    -> CString -- ^File name of the data with identity providers. Null-terminated.
    -> FunPtr GenesisDataCallback -- ^Function to process the generated genesis data.
    -> FunPtr BakerIdentityCallback -- ^Function to process each baker identity. Will be called repeatedly with different baker ids.
    -> IO CInt
makeGenesisData genTime nBakers cryptoParamsFile idProvidersFile cbkgen cbkbaker = do
    mCryptoParams <- readCryptographicParameters <$>
      catch (BSL.readFile =<< peekCString cryptoParamsFile) (\(_ :: IOException) -> return BSL.empty)
    mIdProviders <- readIdentityProviders <$>
      catch (BSL.readFile =<< peekCString idProvidersFile) (\(_ :: IOException) -> return BSL.empty)
    case mCryptoParams of
      Nothing -> return 9
      Just cryptoParams ->
        case mIdProviders of
          Nothing -> return 10
          Just idProviders -> do
            let (genData, bakers) = S.makeGenesisData genTime (fromIntegral nBakers) 10 0.5 9 cryptoParams idProviders
            let bakersPrivate = map fst bakers
            callGenesisDataCallback cbkgen (encode genData)
            mapM_ (\(bid, bkr) -> callBakerIdentityCallback cbkbaker bid (encode bkr)) (zip [0..] bakersPrivate)
            return 0

-- | External function that logs in Rust a message using standard Rust log output
--
-- The first argument represents the Identifier which shows in which module the message has been emited.
-- The current mapping is as follows:
--
-- +----------+---------+
-- |Identifier|Module   |
-- +==========+=========+
-- |1         |Runner   |
-- +----------+---------+
-- |2         |Afgjort  |
-- +----------+---------+
-- |3         |Birk     |
-- +----------+---------+
-- |4         |Crypto   |
-- +----------+---------+
-- |5         |Kontrol  |
-- +----------+---------+
-- |6         |Skov     |
-- +----------+---------+
-- |7         |Baker    |
-- +----------+---------+
-- |8         |External |
-- +----------+---------+
--
-- The second argument represents the Log Level which is interpreted as follows:
--
-- +-----+--------+
-- |Value|LogLevel|
-- +=====+========+
-- |1    |Error   |
-- +-----+--------+
-- |2    |Warning |
-- +-----+--------+
-- |3    |Info    |
-- +-----+--------+
-- |4    |Debug   |
-- +-----+--------+
-- |Other|Trace   |
-- +-----+--------+
--
-- The third argument is the log message that is emited.
type LogCallback = Word8 -> Word8 -> CString -> IO ()
foreign import ccall "dynamic" callLogCallback :: FunPtr LogCallback -> LogCallback


type LogTransferCallback = Word8 -- ^Type of transfer (see documentation of 'toLogTransferMethod' below)
                           -> Ptr Word8 -- ^Pointer to block hash (32 bytes)
                           -> Slot -- ^Slot number of the block (unsigned 64 bit int)
                           -> Ptr Word8 -- ^Pointer to transaction hash (32 bytes) or NULL for some types.
                           -> Amount -- ^Amount (unsigned 64 bit integer)
                           -> CSize -- ^Size of the rest of the data
                           -> Ptr Word8 -- ^Type dependent serialized data.
                           -> IO ()

foreign import ccall "dynamic" callLogTransferCallback :: FunPtr LogTransferCallback -> LogTransferCallback

-- |Wrap a log callback as a log method.
toLogMethod :: FunPtr LogCallback -> LogMethod IO
toLogMethod logCallbackPtr = le
    where
        logCallback = callLogCallback logCallbackPtr
        le src lvl msg = BS.useAsCString (BS.pack msg) $
                            logCallback (logSourceId src) (logLevelId lvl)

unsafeWithBSLen :: BS.ByteString -> (CSize -> Ptr Word8 -> IO ()) -> IO ()
unsafeWithBSLen bs f = BS.unsafeUseAsCStringLen bs $ \(ptr, len) -> f (fromIntegral len) (castPtr ptr) 

-- |Create a method for logging transfers.
-- The output format is the following.
-- |--------+---------------------------------+--------------------------------------------------------|
-- |   Type | Serialized data                 | Comment                                                |
-- | ====== | ====================            | ===========                                            |
-- |      0 | source account, target account  | Direct transfer                                        |
-- |      1 | source account, target contract | Transfer from account to contract                      |
-- |      2 | source contract, target account | Transfer from contract to account                      |
-- |      3 | source account, target baker id | Execution cost of transaction                          |
-- |      4 | baker id, baker account         | Total block reward, transaction hash is a NUll pointer |
-- |      5 | source contract, target contract| Transfer from contract to contract                     |
-- |      6 | from acc, to acc, JSON object   | Credential deployed, amount field is a dummy value     |
-- |--------+---------------------------------+--------------------------------------------------------|

-- * Account address serialiation is 21 bytes in length
-- * Contract address serialization is 16 bytes, consisting of two 64-bit unsigned integers in big-endian format 
-- * Baker Id serialization is a 64 bit unsigned integer in big-endian format.

toLogTransferMethod :: FunPtr LogTransferCallback -> TS.LogTransferMethod IO
toLogTransferMethod logtCallBackPtr = logTransfer
    where logit = callLogTransferCallback logtCallBackPtr
          logTransfer bh slot reason =
            withBlockReference bh $ \block ->
              case reason of
                TS.DirectTransfer{..} ->
                  withTxReference trdtId $ \txRef ->
                    let rest = runPut (put trdtSource <> put trdtTarget)
                    in unsafeWithBSLen rest $ logit 0 block slot txRef trdtAmount
                TS.AccountToContractTransfer{..} ->
                  withTxReference tractId $ \txRef ->
                    let rest = runPut (put tractSource <> put tractTarget)
                    in unsafeWithBSLen rest $ logit 1 block slot txRef tractAmount
                TS.ContractToAccountTransfer{..} ->
                  withTxReference trcatId $ \txRef ->
                    let rest = runPut (put trcatSource <> put trcatTarget)
                    in unsafeWithBSLen rest $ logit 2 block slot txRef trcatAmount
                TS.ExecutionCost{..} ->
                  withTxReference trecId $ \txRef ->
                    let rest = runPut (put trecSource <> put trecBaker)
                    in unsafeWithBSLen rest $ logit 3 block slot txRef trecAmount
                TS.BakingRewardTransfer{..} ->
                  let rest = runPut (put trbrBaker <> put trbrAccount)
                  in unsafeWithBSLen rest $ logit 4 block slot nullPtr trbrAmount
                TS.ContractToContractTransfer{..} ->
                  withTxReference trcctId $ \txRef ->
                    let rest = runPut (put trcctSource <> put trcctTarget)
                    in unsafeWithBSLen rest $ logit 5 block slot txRef trcctAmount
                TS.CredentialDeployment{..} ->
                  withTxReference trcdId $ \txRef ->
                    let rest = runPut (put trcdSource <> put trcdAccount) <> BSL.toStrict (AE.encode trcdCredentialValues)
                    in unsafeWithBSLen rest $ logit 6 block slot txRef 0

-- |Callback for broadcasting a message to the network.
-- The first argument indicates the message type.
-- The second argument is a pointer to the data to broadcast.
-- The third argument is the length of the data in bytes.
type BroadcastCallback = Int64 -> CString -> Int64 -> IO ()
foreign import ccall "dynamic" invokeBroadcastCallback :: FunPtr BroadcastCallback -> BroadcastCallback
data MessageType 
    = MTBlock
    | MTFinalization
    | MTFinalizationRecord
    | MTCatchUpStatus
callBroadcastCallback :: FunPtr BroadcastCallback -> MessageType -> BS.ByteString -> IO ()
callBroadcastCallback cbk mt bs = BS.useAsCStringLen bs $ \(cdata, clen) -> invokeBroadcastCallback cbk mti cdata (fromIntegral clen)
    where
        mti = case mt of
            MTBlock -> 0
            MTFinalization -> 1
            MTFinalizationRecord -> 2
            MTCatchUpStatus -> 3

broadcastCallback :: LogMethod IO -> FunPtr BroadcastCallback -> SimpleOutMessage -> IO ()
broadcastCallback logM bcbk = handleB
    where
        handleB (SOMsgNewBlock block) = do
            let blockbs = runPut (putBlock block)
            logM External LLDebug $ "Broadcasting block [size=" ++ show (BS.length blockbs) ++ "]"
            callBroadcastCallback bcbk MTBlock blockbs
        handleB (SOMsgFinalization finMsg) = do
            let finbs = encode finMsg
            logM External LLDebug $ "Broadcasting finalization message [size=" ++ show (BS.length finbs) ++ "]: " ++ show finMsg
            callBroadcastCallback bcbk MTFinalization finbs
        handleB (SOMsgFinalizationRecord finRec) = do
            let msgbs = encode finRec
            logM External LLDebug $ "Broadcasting finalization record [size=" ++ show (BS.length msgbs) ++ "]: " ++ show finRec
            callBroadcastCallback bcbk MTFinalizationRecord msgbs

-- |A 'ConsensusRunner' encapsulates an instance of the consensus, and possibly a baker thread.
data ConsensusRunner = BakerRunner {
        bakerSyncRunner :: SyncRunner,
        bakerBroadcast :: SimpleOutMessage -> IO ()
    }
    | PassiveRunner {
        passiveSyncRunner :: SyncPassiveRunner
    }

consensusLogMethod :: ConsensusRunner -> LogMethod IO
consensusLogMethod BakerRunner{bakerSyncRunner=SyncRunner{syncLogMethod=logM}} = logM
consensusLogMethod PassiveRunner{passiveSyncRunner=SyncPassiveRunner{syncPLogMethod=logM}} = logM

genesisState :: GenesisData -> PersistentBlockState
genesisState genData = initialPersistentState (genesisBirkParameters genData) (genesisCryptographicParameters genData) (genesisBakerAccounts genData) (genesisIdentityProviders genData) 2

-- |Start up an instance of Skov without starting the baker thread.
startConsensus ::
           Word64 -- ^Maximum block size.
           -> CString -> Int64 -- ^Serialized genesis data (c string + len)
           -> CString -> Int64 -- ^Serialized baker identity (c string + len)
           -> FunPtr BroadcastCallback -- ^Handler for generated messages
           -> FunPtr LogCallback -- ^Handler for log events
           -> Word8 -- ^Whether to enable logging of transfer events (/= 0) or not (value 0).
           -> FunPtr LogTransferCallback -- ^Handler for logging transfer events
           -> IO (StablePtr ConsensusRunner)
startConsensus maxBlock gdataC gdataLenC bidC bidLenC bcbk lcbk enableTransferLogging ltcbk = do
        gdata <- BS.packCStringLen (gdataC, fromIntegral gdataLenC)
        bdata <- BS.packCStringLen (bidC, fromIntegral bidLenC)
        case (decode gdata, decode bdata) of
            (Right genData, Right bid) -> do
                bakerSyncRunner <- makeSyncRunner logM logT bid (RuntimeParameters (fromIntegral maxBlock)) genData (genesisState genData) bakerBroadcast
                newStablePtr BakerRunner{..}
            _ -> ioError (userError $ "Error decoding serialized data.")
    where
        logM = toLogMethod lcbk
        logT = if enableTransferLogging /= 0 then Just (toLogTransferMethod ltcbk) else Nothing
        bakerBroadcast = broadcastCallback logM bcbk

-- |Start consensus without a baker identity.
startConsensusPassive ::
           Word64 -- ^Maximum block size.
           -> CString -> Int64 -- ^Serialized genesis data (c string + len)
           -> FunPtr LogCallback -- ^Handler for log events
            -> IO (StablePtr ConsensusRunner)
startConsensusPassive maxBlock gdataC gdataLenC lcbk = do
        gdata <- BS.packCStringLen (gdataC, fromIntegral gdataLenC)
        case (decode gdata) of
            (Right genData) -> do
                passiveSyncRunner <- makeSyncPassiveRunner logM (RuntimeParameters (fromIntegral maxBlock)) genData (genesisState genData)
                newStablePtr PassiveRunner{..}
            _ -> ioError (userError $ "Error decoding serialized data.")
    where
        logM = toLogMethod lcbk

-- |Shuts down consensus, stopping any baker thread if necessary.
-- The pointer is not valid after this function returns.
stopConsensus :: StablePtr ConsensusRunner -> IO ()
stopConsensus cptr = mask_ $ do
    deRefStablePtr cptr >>= \case
        BakerRunner{..} -> shutdownSyncRunner bakerSyncRunner
        PassiveRunner{..} -> shutdownSyncPassiveRunner passiveSyncRunner
    freeStablePtr cptr

-- |Start the baker thread.  Calling this more than once
-- should not start additional baker threads.
startBaker :: StablePtr ConsensusRunner -> IO ()
startBaker cptr = mask_ $
    deRefStablePtr cptr >>= \case
        BakerRunner{..} -> startSyncRunner bakerSyncRunner
        c -> consensusLogMethod c External LLError "Attempted to start baker thread, but consensus was started without baker credentials"

-- |Stop a baker thread.
stopBaker :: StablePtr ConsensusRunner -> IO ()
stopBaker cptr = mask_ $ do
    deRefStablePtr cptr >>= \case
        BakerRunner{..} -> stopSyncRunner bakerSyncRunner
        c -> consensusLogMethod c External LLError "Attempted to stop baker thread, but consensus was started without baker credentials"

{- | Result values for receive functions.

+=======+====================================+========================================================================================+==========+
| Value |                Name                |                                      Description                                       | Forward? |
+=======+====================================+========================================================================================+==========+
|     0 | ResultSuccess                      | Message received, validated and processed                                              | Yes      |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     1 | ResultSerializationFail            | Message deserialization failed                                                         | No       |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     2 | ResultInvalid                      | The message was determined to be invalid                                               | No       |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     3 | ResultPendingBlock                 | The message was received, but is awaiting a block to complete processing               | Yes      |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     4 | ResultPendingFinalization          | The message was received, but is awaiting a finalization record to complete processing | Yes      |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     5 | ResultAsync                        | The message was received, but is being processed asynchronously                        | Yes      |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     6 | ResultDuplicate                    | The message duplicates a previously received message                                   | No       |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     7 | ResultStale                        | The message may have been valid in the past, but is no longer relevant                 | No       |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     8 | ResultIncorrectFinalizationSession | The message refers to a different/unknown finalization session                         | No(?)    |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
|     9 | ResultUnverifiable                 | The message could not be verified in the current state (initiate catch-up with peer)   | No       |
+-------+------------------------------------+----------------------------------------------------------------------------------------+----------+
-}
type ReceiveResult = Int64

toReceiveResult :: UpdateResult -> ReceiveResult
toReceiveResult ResultSuccess = 0
toReceiveResult ResultSerializationFail = 1
toReceiveResult ResultInvalid = 2
toReceiveResult ResultPendingBlock = 3
toReceiveResult ResultPendingFinalization = 4
toReceiveResult ResultAsync = 5
toReceiveResult ResultDuplicate = 6
toReceiveResult ResultStale = 7
toReceiveResult ResultIncorrectFinalizationSession = 8
toReceiveResult ResultUnverifiable = 9


handleSkovFinalizationEvents :: (SimpleOutMessage -> IO ()) -> [FinalizationOutputEvent] -> IO ()
handleSkovFinalizationEvents broadcast = mapM_ handleEvt
    where
        handleEvt (BroadcastFinalizationMessage finMsg) = broadcast (SOMsgFinalization (FPMMessage finMsg))
        handleEvt (BroadcastFinalizationRecord finRec) = broadcast (SOMsgFinalizationRecord finRec)

-- |Handle receipt of a block.
-- The possible return codes are @ResultSuccess@, @ResultSerializationFail@, @ResultInvalid@,
-- @ResultPendingBlock@, @ResultPendingFinalization@, @ResultAsync@, @ResultDuplicate@,
-- and @ResultStale@.
-- 'receiveBlock' may invoke the callbacks for new finalization messages and finalization records,
-- and missing blocks and finalization records.
receiveBlock :: StablePtr ConsensusRunner -> CString -> Int64 -> IO ReceiveResult
receiveBlock bptr cstr l = do
    c <- deRefStablePtr bptr
    let logm = consensusLogMethod c
    logm External LLDebug $ "Received block data size = " ++ show l ++ ". Decoding ..."
    blockBS <- BS.packCStringLen (cstr, fromIntegral l)
    now <- currentTime
    toReceiveResult <$> case runGet (BasicBlock.getBlock now) blockBS of
        Left _ -> do
          logm External LLDebug "Block deserialization failed. Ignoring the block."
          return ResultSerializationFail
        Right (GenesisBlock _) -> do
            logm External LLDebug $ "Genesis block deserialized. Ignoring the block."
            return ResultSerializationFail
        Right (NormalBlock block0) -> do
                        logm External LLInfo $ "Block deserialized. Sending to consensus."
                        let block = makePendingBlock block0 now
                        case c of
                            BakerRunner{..} -> do
                                (res, evts) <- syncReceiveBlock bakerSyncRunner block
                                handleSkovFinalizationEvents bakerBroadcast evts
                                return res
                            PassiveRunner{..} -> syncPassiveReceiveBlock passiveSyncRunner block


-- |Handle receipt of a finalization message.
-- The possible return codes are @ResultSuccess@, @ResultSerializationFail@, @ResultInvalid@,
-- @ResultPendingFinalization@, @ResultDuplicate@, @ResultStale@, @ResultIncorrectFinalizationSession@ and
-- @ResultUnverifiable@.
-- 'receiveFinalization' may invoke the callbacks for new finalization messages and finalization records,
-- and missing blocks and finalization records.
receiveFinalization :: StablePtr ConsensusRunner -> CString -> Int64 -> IO ReceiveResult
receiveFinalization bptr cstr l = do
    c <- deRefStablePtr bptr
    let logm = consensusLogMethod c
    logm External LLDebug $ "Received finalization message size = " ++ show l ++ ".  Decoding ..."
    bs <- BS.packCStringLen (cstr, fromIntegral l)
    toReceiveResult <$> case runGet get bs of
        Left _ -> do
            logm External LLDebug "Deserialization of finalization message failed."
            return ResultSerializationFail
        Right finMsg -> do
            logm External LLDebug "Finalization message deserialized."
            case c of
                BakerRunner{..} -> do
                    (res, evts) <- syncReceiveFinalizationMessage bakerSyncRunner finMsg
                    handleSkovFinalizationEvents bakerBroadcast evts
                    return res
                PassiveRunner{..} -> syncPassiveReceiveFinalizationMessage passiveSyncRunner finMsg bs

-- |Handle receipt of a finalization record.
-- The possible return codes are @ResultSuccess@, @ResultSerializationFail@, @ResultInvalid@,
-- @ResultPendingBlock@, @ResultPendingFinalization@, @ResultDuplicate@, and @ResultStale@.
-- (Currently, @ResultDuplicate@ cannot happen, although it may be supported in future.)
-- 'receiveFinalizationRecord' may invoke the callbacks for new finalization messages and
-- finalization records, and missing blocks and finalization records.
receiveFinalizationRecord :: StablePtr ConsensusRunner -> CString -> Int64 -> IO ReceiveResult
receiveFinalizationRecord bptr cstr l = do
    c <- deRefStablePtr bptr
    let logm = consensusLogMethod c
    logm External LLDebug $ "Received finalization record data size = " ++ show l ++ ". Decoding ..."
    finRecBS <- BS.packCStringLen (cstr, fromIntegral l)
    toReceiveResult <$> case runGet get finRecBS of
        Left _ -> do
          logm External LLDebug "Deserialization of finalization record failed."
          return ResultSerializationFail
        Right finRec -> do
            logm External LLDebug "Finalization record deserialized."
            case c of
                BakerRunner{..} -> do
                    (res, evts) <- syncReceiveFinalizationRecord bakerSyncRunner finRec
                    handleSkovFinalizationEvents bakerBroadcast evts
                    return res
                PassiveRunner{..} -> syncPassiveReceiveFinalizationRecord passiveSyncRunner finRec

-- |Handle receipt of a transaction.
-- The possible return codes are @ResultSuccess@, @ResultSerializationFail@, @ResultDuplicate@, and @ResultStale@
receiveTransaction :: StablePtr ConsensusRunner -> CString -> Int64 -> IO ReceiveResult
receiveTransaction bptr tdata len = do
    c <- deRefStablePtr bptr
    let logm = consensusLogMethod c
    logm External LLInfo $ "Received transaction, data size=" ++ show len ++ ". Decoding ..."
    tbs <- BS.packCStringLen (tdata, fromIntegral len)
    now <- currentTime
    toReceiveResult <$> case runGet (getVerifiedTransaction now) tbs of
        Left err -> do
            logm External LLDebug $ "Could not decode transaction into header + body because " ++ err
            return ResultSerializationFail
        Right tr -> do
            logm External LLInfo $ "Transaction decoded. Its header is: " ++ show (btrHeader (trBareTransaction tr))
            case c of
                BakerRunner{..} -> do
                    (res, _) <- syncReceiveTransaction bakerSyncRunner tr
                    -- Currently, no events can occur as a result of receiving a transaction
                    return res
                PassiveRunner{..} -> syncPassiveReceiveTransaction passiveSyncRunner tr

runConsensusQuery :: ConsensusRunner -> (forall z m s. (Get.SkovStateQueryable z m, TS.TreeStateMonad m, MonadState s m, TS.PendingBlock m ~ PendingBlock, TS.BlockPointer m ~ PersistentBlockPointer) => z -> a) -> a
runConsensusQuery BakerRunner{..} f = f bakerSyncRunner
runConsensusQuery PassiveRunner{..} f = f passiveSyncRunner


-- |Returns a null-terminated string with a JSON representation of the current status of Consensus.
getConsensusStatus :: StablePtr ConsensusRunner -> IO CString
getConsensusStatus cptr = do
    c <- deRefStablePtr cptr
    status <- runConsensusQuery c Get.getConsensusStatus
    jsonValueToCString status

-- |Given a null-terminated string that represents a block hash (base 16), returns a null-terminated
-- string containing a JSON representation of the block.
getBlockInfo :: StablePtr ConsensusRunner -> CString -> IO CString
getBlockInfo cptr blockcstr = do
    c <- deRefStablePtr cptr
    block <- peekCString blockcstr
    blockInfo <- runConsensusQuery c Get.getBlockInfo block
    jsonValueToCString blockInfo

-- |Given a null-terminated string that represents a block hash (base 16), and a number of blocks,
-- returns a null-terminated string containing a JSON list of the ancestors of the node (up to the
-- given number, including the block itself).
getAncestors :: StablePtr ConsensusRunner -> CString -> Word64 -> IO CString
getAncestors cptr blockcstr depth = do
    c <- deRefStablePtr cptr
    block <- peekCString blockcstr
    ancestors <- runConsensusQuery c Get.getAncestors block (fromIntegral depth :: BlockHeight)
    jsonValueToCString ancestors

-- |Returns a null-terminated string with a JSON representation of the current branches from the
-- last finalized block (inclusive).
getBranches :: StablePtr ConsensusRunner -> IO CString
getBranches cptr = do
    c <- deRefStablePtr cptr
    branches <- runConsensusQuery c Get.getBranches
    jsonValueToCString branches



byteStringToCString :: BS.ByteString -> IO CString
byteStringToCString bs = do
  let bsp = BS.concat [P.runPut (P.putWord32be (fromIntegral (BS.length bs))), bs]
  -- This use of unsafe is fine because bsp is a non-null string.
  BS.unsafeUseAsCStringLen bsp $ \(cstr, len) -> do dest <- mallocBytes len
                                                    copyBytes dest cstr len
                                                    return dest

withBlockHash :: CString -> (String -> IO ()) -> (BlockHash -> IO CString) -> IO CString
withBlockHash blockcstr logm f = 
  readMaybe <$> (peekCString blockcstr) >>=
    \case Nothing -> do
            logm "Block hash invalid. Returning error value."
            newCString "\"Invalid block hash.\""
          Just hash -> f hash

-- |Get the list of account addresses in the given block. The block must be
-- given as a null-terminated base16 encoding of the block hash. The return
-- value is a null-terminated JSON-encoded list of addresses.
-- The returned string should be freed by calling 'freeCStr'.
getAccountList :: StablePtr ConsensusRunner -> CString -> IO CString
getAccountList cptr blockcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLInfo "Received account list request."
    withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
      alist <- runConsensusQuery c (Get.getAccountList hash)
      logm External LLDebug $ "Replying with the list: " ++ show alist
      jsonValueToCString alist

-- |Get the list of contract instances (their addresses) in the given block. The
-- block must be given as a null-terminated base16 encoding of the block hash.
-- The return value is a null-terminated JSON-encoded list of addresses.
-- The returned string should be freed by calling 'freeCStr'.
getInstances :: StablePtr ConsensusRunner -> CString -> IO CString
getInstances cptr blockcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLInfo "Received instance list request."
    withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
      istances <- runConsensusQuery c (Get.getInstances hash)
      logm External LLDebug $ "Replying with the list: " ++ (show istances)
      jsonValueToCString istances


-- |Get account information for the given block and instance. The block must be
-- given as a null-terminated base16 encoding of the block hash and the account
-- address (second CString) must be given as a null-terminated string in Base58
-- encoding (same format as returned by 'getAccountList'). The return value is a
-- null-terminated, json encoded information.
-- The returned string should be freed by calling 'freeCStr'.
getAccountInfo :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
getAccountInfo cptr blockcstr cstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLInfo "Received account info request."
    bs <- BS.packCString cstr
    case addressFromBytes bs of 
      Nothing -> do
        logm External LLInfo "Could not decode address."
        jsonValueToCString Null
      Just acc -> do
        logm External LLInfo $ "Decoded address to: " ++ show acc
        withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
          ainfo <- runConsensusQuery c (Get.getAccountInfo hash) acc
          logm External LLDebug $ "Replying with: " ++ show ainfo
          jsonValueToCString ainfo

-- |Get the status of the rewards parameters for the given block. The block must
-- be given as a null-terminated base16 encoding of the block hash.
-- The return value is a null-terminated, JSON encoded value.
-- The returned string should be freed by calling 'freeCStr'.
getRewardStatus :: StablePtr ConsensusRunner -> CString -> IO CString
getRewardStatus cptr blockcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLInfo "Received request for bank status."
    withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
      reward <- runConsensusQuery c (Get.getRewardStatus hash)
      logm External LLDebug $ "Replying with: " ++ show reward
      jsonValueToCString reward


-- |Get the list of modules in the given block. The block must be given as a
-- null-terminated base16 encoding of the block hash.
-- The return value is a null-terminated JSON-encoded list.
-- The returned string should be freed by calling 'freeCStr'.
getModuleList :: StablePtr ConsensusRunner -> CString -> IO CString
getModuleList cptr blockcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLInfo "Received request for list of modules."
    withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
      mods <- runConsensusQuery c (Get.getModuleList hash)
      logm External LLDebug $ "Replying with" ++ show mods
      jsonValueToCString mods

-- |Get birk parameters for the given block. The block must be given as a
-- null-terminated base16 encoding of the block hash.
-- The return value is a null-terminated JSON-encoded value.
-- The returned string should be freed by calling 'freeCStr'.
getBirkParameters :: StablePtr ConsensusRunner -> CString -> IO CString
getBirkParameters cptr blockcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLInfo "Received request Birk parameters."
    withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
      bps <- runConsensusQuery c (Get.getBlockBirkParameters hash)
      logm External LLDebug $ "Replying with" ++ show bps
      jsonValueToCString bps


-- |Get instance information the given block and instance. The block must be
-- given as a null-terminated base16 encoding of the block hash and the address
-- (second CString) must be given as a null-terminated JSON-encoded value.
-- The return value is a null-terminated, json encoded information.
-- The returned string should be freed by calling 'freeCStr'.
getInstanceInfo :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
getInstanceInfo cptr blockcstr cstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLInfo "Received account info request."
    bs <- BS.packCString cstr
    case AE.decodeStrict bs :: Maybe ContractAddress of
      Nothing -> do
        logm External LLDebug "Could not decode address."
        jsonValueToCString Null
      Just ii -> do
        logm External LLDebug $ "Decoded address to: " ++ show ii
        withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
          iinfo <- runConsensusQuery c (Get.getContractInfo hash) ii
          logm External LLDebug $ "Replying with: " ++ show iinfo
          jsonValueToCString iinfo


-- |NB: The return value is __NOT__ JSON encoded but rather it is a binary
-- serialization. The first 4 bytes are the length of the rest of the string, and
-- the string is __NOT__ null terminated and can contain null characters.
-- The returned string should be freed by calling 'freeCStr'.
getModuleSource :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
getModuleSource cptr blockcstr cstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLInfo "Received request for a module."
    bs <- peekCString cstr -- null terminated
    case readMaybe bs of
      Nothing -> do
        logm External LLInfo "Cannot decode module reference."
        byteStringToCString BS.empty
      Just mrefhash -> do
        let mref = ModuleRef mrefhash
        logm External LLInfo $ "Decoded module hash to : " ++ show mref -- base 16
        withBlockHash blockcstr (logm External LLDebug) $ \hash -> do
          mmodul <- runConsensusQuery c (Get.getModuleSource hash) mref
          case mmodul :: Maybe (Core.Module Core.UA) of
            Nothing -> do
              logm External LLDebug "Module not available."
              byteStringToCString BS.empty
            Just modul ->
              let reply = P.runPut (Core.putModule modul)
              in do
                logm External LLDebug $ "Replying with data size = " ++ show (BS.length reply)
                byteStringToCString reply

-- |Query consensus about a specific transaction, installing a hook to
-- observe when the transaction is added to a block.
-- The transaction hash is passed as a null-terminated base-16 encoded string.
-- The return value is a null-terminated JSON object representing the state
-- of the transaction, which should be freed with 'freeCStr'.
hookTransaction :: StablePtr ConsensusRunner -> CString -> IO CString
hookTransaction cptr trcstr = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    logm External LLInfo "Received transaction hook request."
    withBlockHash trcstr (logm External LLDebug) $ \hash -> do
        hookRes <- case c of
            BakerRunner{..} -> syncHookTransaction bakerSyncRunner hash
            PassiveRunner{..} -> syncPassiveHookTransaction passiveSyncRunner hash
        let v = AE.toJSON hookRes
        logm External LLDebug $ "Replying with: " ++ show v
        jsonValueToCString v

freeCStr :: CString -> IO ()
freeCStr = free

-- |Get a block for the given block hash.
-- The block hash is passed as a pointer to a fixed length (32 byte) string.
-- The return value is a length encoded string: the first 4 bytes are
-- the length (encoded big-endian), followed by the data itself.
-- The string should be freed by calling 'freeCStr'.
-- The string may be empty (length 0) if the finalization record is not found.
getBlock :: StablePtr ConsensusRunner -> BlockReference -> IO CString
getBlock cptr blockRef = do
        c <- deRefStablePtr cptr
        let logm = consensusLogMethod c
        bh <- blockReferenceToBlockHash blockRef
        logm External LLInfo $ "Received request for block: " ++ show bh
        b <- runConsensusQuery c Get.getBlockData bh
        case b :: Maybe Block of
            Nothing -> do
                logm External LLInfo $ "Block not available"
                byteStringToCString BS.empty
            Just block -> do
                logm External LLInfo $ "Block found"
                byteStringToCString $ P.runPut $ putBlock block

-- |Get a block that is descended from the given block hash by the given number of generations.
-- The block hash is passed as a pointer to a fixed length (32 byte) string.
-- The return value is a length encoded string: the first 4 bytes are
-- the length (encoded big-endian), followed by the data itself.
-- The string should be freed by calling 'freeCStr'.
-- The string may be empty (length 0) if the finalization record is not found.
getBlockDelta :: StablePtr ConsensusRunner -> BlockReference -> Word64 -> IO CString
getBlockDelta bptr blockRef 0 = getBlock bptr blockRef
getBlockDelta cptr blockRef delta = do
        c <- deRefStablePtr cptr
        let logm = consensusLogMethod c
        bh <- blockReferenceToBlockHash blockRef
        logm External LLInfo $ "Received request for descendent of block " ++ show bh ++ " with delta " ++ show delta
        b <- runConsensusQuery c Get.getBlockDescendant bh (BlockHeight delta)
        case b :: Maybe Block of
            Nothing -> do
                logm External LLInfo $ "Block not available"
                byteStringToCString BS.empty
            Just block -> do
                logm External LLInfo $ "Block found"
                byteStringToCString . P.runPut . putBlock $ block

-- |Get a finalization record for the given block.
-- The block hash is passed as a pointer to a fixed length (32 byte) string.
-- The return value is a length encoded string: the first 4 bytes are
-- the length (encoded big-endian), followed by the data itself.
-- The string should be freed by calling 'freeCStr'.
-- The string may be empty (length 0) if the finalization record is not found.
getBlockFinalization :: StablePtr ConsensusRunner -> BlockReference -> IO CString
getBlockFinalization cptr blockRef = do
        c <- deRefStablePtr cptr
        let logm = consensusLogMethod c
        bh <- blockReferenceToBlockHash blockRef
        logm External LLInfo $ "Received request for finalization record for block: " ++ show bh
        f <- runConsensusQuery c Get.getBlockFinalization bh
        case f :: Maybe FinalizationRecord of
            Nothing -> do
                logm External LLInfo $ "Finalization record not available"
                byteStringToCString BS.empty
            Just finRec -> do
                logm External LLInfo $ "Finalization record found"
                byteStringToCString $ P.runPut $ put finRec

-- |Get a finalization record for the given finalization index.
-- The return value is a length encoded string: the first 4 bytes are
-- the length (encoded big-endian), followed by the data itself.
-- The string should be freed by calling 'freeCStr'.
-- The string may be empty (length 0) if the finalization record is not found.
getIndexedFinalization :: StablePtr ConsensusRunner -> Word64 -> IO CString
getIndexedFinalization cptr finInd = do
        c <- deRefStablePtr cptr
        let logm = consensusLogMethod c
        logm External LLInfo $ "Received request for finalization record at index " ++ show finInd
        f <- runConsensusQuery c Get.getIndexedFinalization (fromIntegral finInd :: FinalizationIndex)
        case f :: Maybe FinalizationRecord of
            Nothing -> do
                logm External LLInfo $ "Finalization record not available"
                byteStringToCString BS.empty
            Just finRec -> do
                logm External LLInfo $ "Finalization record found"
                byteStringToCString $ P.runPut $ put finRec

-- |Get a catch-up status message for requesting catch-up with peers.
-- The return value is a length encoded string: the first 4 bytes are
-- the length (encoded big-endian), followed by the data itself.
-- The string should be freed by calling 'freeCStr'.
getCatchUpStatus :: StablePtr ConsensusRunner -> IO CString
getCatchUpStatus cptr = do
        c <- deRefStablePtr cptr
        let logm = consensusLogMethod c
        logm External LLInfo $ "Received request for catch-up status"
        cus <- runConsensusQuery c Get.getCatchUpStatus
        logm External LLDebug $ "Replying with catch-up status = " ++ show cus
        byteStringToCString $ encode (cus :: CatchUpStatus)

-- |Callback for sending a message to a peer.
-- The first argument is the peer to send to.
-- The second argument indicates the message type.
-- The third argument is a pointer to the data to broadcast.
-- The fourth argument is the length of the data in bytes.
type DirectMessageCallback = PeerID -> Int64 -> CString -> Int64 -> IO ()
foreign import ccall "dynamic" invokeDirectMessageCallback :: FunPtr DirectMessageCallback -> DirectMessageCallback
callDirectMessageCallback :: FunPtr DirectMessageCallback -> PeerID -> MessageType -> BS.ByteString -> IO ()
callDirectMessageCallback cbk peer mt bs = BS.useAsCStringLen bs $ \(cdata, clen) -> invokeDirectMessageCallback cbk peer mti cdata (fromIntegral clen)
    where
        mti = case mt of
            MTBlock -> 0
            MTFinalization -> 1
            MTFinalizationRecord -> 2
            MTCatchUpStatus -> 3

-- |Handle receiving a catch-up status message.
-- If the message is a request, then the supplied callback will be used to
-- send the requested data for the peer.
-- The response code can be:
-- * @ResultSerializationFail@
-- * @ResultInvalid@ -- the catch-up message is inconsistent with the skov
-- * @ResultPendingBlock@ -- the sender has some data I am missing, and should be marked pending
-- * @ResultSuccess@ -- I do not require additional data from the sender, so mark it as up-to-date
receiveCatchUpStatus :: StablePtr ConsensusRunner -> PeerID -> CString -> Int64 -> FunPtr DirectMessageCallback -> IO ReceiveResult
receiveCatchUpStatus cptr src cstr l cbk = do
    c <- deRefStablePtr cptr
    let logm = consensusLogMethod c
    bs <- BS.packCStringLen (cstr, fromIntegral l)
    toReceiveResult <$> case decode bs :: Either String CatchUpStatus of
        Left _ -> do
            logm External LLDebug "Deserialization of catch-up status message failed."
            return ResultSerializationFail
        Right cus -> do
            logm External LLDebug $ "Catch-up status message deserialized: " ++ show cus
            res <- runConsensusQuery c Get.handleCatchUpStatus cus
            case res :: (Either String (Maybe ([Either FinalizationRecord (PersistentBlockPointer)], CatchUpStatus), Bool)) of
                Left emsg -> logm Skov LLWarning emsg >> return ResultInvalid
                Right (d, flag) -> do
                    let
                        sendMsg = callDirectMessageCallback cbk src
                        sendBlock = sendMsg MTBlock . runPut . putBlock . _bpBlock
                        sendFinRec = sendMsg MTFinalizationRecord . encode
                        send [] = return ()
                        send (Left fr:r) = sendFinRec fr >> send r
                        send (Right b:r) = sendBlock b >> send r
                    forM_ d $ \(frbs, rcus) -> do
                        logm Skov LLDebug $ "Catch up response data: " ++ show frbs
                        send frbs
                        logm Skov LLDebug $ "Catch up response status message: " ++ show rcus
                        sendMsg MTCatchUpStatus $ encode rcus
                    return $! if flag then ResultPendingBlock else ResultSuccess
                        


foreign export ccall makeGenesisData :: Timestamp -> Word64 -> CString -> CString -> FunPtr GenesisDataCallback -> FunPtr BakerIdentityCallback -> IO CInt
foreign export ccall startConsensus :: Word64 -> CString -> Int64 -> CString -> Int64 -> FunPtr BroadcastCallback -> FunPtr LogCallback -> Word8 -> FunPtr LogTransferCallback -> IO (StablePtr ConsensusRunner)
foreign export ccall startConsensusPassive :: Word64 -> CString -> Int64 -> FunPtr LogCallback -> IO (StablePtr ConsensusRunner)
foreign export ccall stopConsensus :: StablePtr ConsensusRunner -> IO ()
foreign export ccall startBaker :: StablePtr ConsensusRunner -> IO ()
foreign export ccall stopBaker :: StablePtr ConsensusRunner -> IO ()
foreign export ccall receiveBlock :: StablePtr ConsensusRunner -> CString -> Int64 -> IO Int64
foreign export ccall receiveFinalization :: StablePtr ConsensusRunner -> CString -> Int64 -> IO Int64
foreign export ccall receiveFinalizationRecord :: StablePtr ConsensusRunner -> CString -> Int64 -> IO Int64
foreign export ccall receiveTransaction :: StablePtr ConsensusRunner -> CString -> Int64 -> IO Int64

foreign export ccall getConsensusStatus :: StablePtr ConsensusRunner -> IO CString
foreign export ccall getBlockInfo :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getAncestors :: StablePtr ConsensusRunner -> CString -> Word64 -> IO CString
foreign export ccall getBranches :: StablePtr ConsensusRunner -> IO CString
foreign export ccall freeCStr :: CString -> IO ()

foreign export ccall getBlock :: StablePtr ConsensusRunner -> BlockReference -> IO CString
foreign export ccall getBlockDelta :: StablePtr ConsensusRunner -> BlockReference -> Word64 -> IO CString
foreign export ccall getBlockFinalization :: StablePtr ConsensusRunner -> BlockReference -> IO CString
foreign export ccall getIndexedFinalization :: StablePtr ConsensusRunner -> Word64 -> IO CString

foreign export ccall getCatchUpStatus :: StablePtr ConsensusRunner -> IO CString
foreign export ccall receiveCatchUpStatus :: StablePtr ConsensusRunner -> PeerID -> CString -> Int64 -> FunPtr DirectMessageCallback -> IO ReceiveResult

-- report global state information will be removed in the future when global
-- state is handled better
foreign export ccall getAccountList :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getInstances :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getAccountInfo :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
foreign export ccall getInstanceInfo :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
foreign export ccall getRewardStatus :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getBirkParameters :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getModuleList :: StablePtr ConsensusRunner -> CString -> IO CString
foreign export ccall getModuleSource :: StablePtr ConsensusRunner -> CString -> CString -> IO CString
foreign export ccall hookTransaction :: StablePtr ConsensusRunner -> CString -> IO CString
