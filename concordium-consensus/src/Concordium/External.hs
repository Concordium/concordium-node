{-# LANGUAGE ForeignFunctionInterface, LambdaCase #-}
module Concordium.External where

import Foreign
import Foreign.C

import Control.Concurrent.Chan
import Control.Concurrent
import qualified Data.ByteString.Char8 as BS
import Data.Serialize
import Data.IORef

import qualified Data.Text.Lazy as LT
import qualified Data.Aeson.Text as AET
import qualified Data.Aeson as AE


import Concordium.Types
import Concordium.Birk.Bake
import Concordium.Payload.Transaction
import Concordium.Runner
import Concordium.Show

import qualified Concordium.Getters as Get
import qualified Concordium.Startup as S

-- Test functions

-- triple :: Int64 -> IO ()
-- triple x = print x

-- foreign export ccall triple :: Int64 -> IO ()

-- foreign import ccall "dynamic" mkCallback :: FunPtr (I32 -> IO I32) -> I32 -> IO I32

-- callbackTwice :: FunPtr (I32 -> IO I32) -> IO I32
-- callbackTwice fun = do
--     let f = mkCallback fun
--     n <- f 0
--     f n


-- foreign export ccall callbackTwice :: FunPtr (I32 -> IO I32) -> IO I32

-- printCString :: CString -> IO ()
-- printCString cs = do
--     s <- peekCString cs
--     putStrLn s

-- foreign export ccall printCString :: CString -> IO ()

-- foreign import ccall "dynamic" mkCStringCallback :: FunPtr (CString -> IO ()) -> CString -> IO ()

-- callbackWithCString :: FunPtr (CString -> IO ()) -> IO ()
-- callbackWithCString cb =
--     withCString "Hello cruel world!" (mkCStringCallback cb)

-- foreign export ccall callbackWithCString :: FunPtr (CString -> IO ()) -> IO ()

data BakerRunner = BakerRunner {
    bakerInChan :: Chan InMessage,
    bakerOutChan :: Chan OutMessage,
    bakerBestBlock :: IORef Get.BlockInfo
}

type CStringCallback = CString -> Int64 -> IO ()
foreign import ccall "dynamic" callCStringCallback :: FunPtr CStringCallback -> CStringCallback

foreign import ccall "dynamic" callCStringCallbackInstance :: FunPtr (Int64 -> CStringCallback) -> Int64 -> CStringCallback

makeGenesisData ::
    Timestamp -- ^Genesis time
    -> Word64 -- ^Number of bakers
    -> FunPtr CStringCallback -- ^Function to process the generated genesis data.
    -> FunPtr (Int64 -> CStringCallback) -- ^Function to process each baker identity. Will be called repeatedly with different baker ids.
    -> IO ()
makeGenesisData genTime nBakers cbkgen cbkbaker = do
    BS.useAsCStringLen (encode genData) $ \(cdata, clen) -> callCStringCallback cbkgen cdata (fromIntegral clen)
    mapM_ (\bkr@(BakerIdentity bid _ _ _ _) -> BS.useAsCStringLen (encode bkr) $ \(cdata, clen) -> callCStringCallbackInstance cbkbaker (fromIntegral bid) cdata (fromIntegral clen)) bakersPrivate
    where
        bakers = S.makeBakers (fromIntegral nBakers)
        genData = S.makeGenesisData genTime bakers
        bakersPrivate = map fst bakers

type BlockCallback = Int64 -> CString -> Int64 -> IO ()

-- | External function that logs in Rust a message using standard Rust log output
--
-- The first argument represents the Identifier which shows in which module the message has been emited.
-- The current mapping is as follows:
--
-- +----------+-------+
-- |Identifier|Module |
-- +==========+=======+
-- |1         |Runner |
-- +----------+-------+
-- |2         |Afgjort|
-- +----------+-------+
-- |3         |Birk   |
-- +----------+-------+
-- |4         |Crypto |
-- +----------+-------+
-- |5         |Kontrol|
-- +----------+-------+
-- |6         |Skov   |
-- +----------+-------+
-- |Other     |Baker  |
-- +----------+-------+
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
-- |Other|Debug   |
-- +-----+--------+
--
-- The third argument is the log message that is emited.

type LogCallback = Word8 -> Word8 -> CString -> IO()

foreign import ccall "dynamic" callBlockCallback :: FunPtr BlockCallback -> BlockCallback

outLoop :: Chan OutMessage -> BlockCallback -> IO ()
outLoop chan cbk = do
    readChan chan >>= \case
        MsgNewBlock block -> do
            let bbs = runPut (serializeBlock block)
            BS.useAsCStringLen bbs $ \(cstr, l) -> cbk 0 cstr (fromIntegral l)
        MsgFinalization finMsg -> do
            BS.useAsCStringLen finMsg $ \(cstr, l) -> cbk 1 cstr (fromIntegral l)
        MsgFinalizationRecord finRec -> do
            let bs = runPut (put finRec)
            BS.useAsCStringLen bs $ \(cstr, l) -> cbk 2 cstr (fromIntegral l)
    outLoop chan cbk

startBaker ::
           CString -> Int64 -- ^Serialized genesis data (c string + len)
           -> CString -> Int64 -- ^Serialized baker identity (c string + len)
           -> FunPtr BlockCallback -> FunPtr LogCallback -> IO (StablePtr BakerRunner)
startBaker gdataC gdataLenC bidC bidLenC bcbk lcbk = do
  gdata <- BS.packCStringLen (gdataC, fromIntegral gdataLenC)
  bdata <- BS.packCStringLen (bidC, fromIntegral bidLenC)
  case (decode gdata, decode bdata) of
    (Right genData, Right bid) -> do
      (cin, cout, out) <- makeRunner bid genData
      forkIO $ outLoop cout (callBlockCallback bcbk)
      newStablePtr (BakerRunner cin cout out)
    _   -> ioError (userError $ "Error decoding serialized data.")

stopBaker :: StablePtr BakerRunner -> IO ()
stopBaker bptr = do
    BakerRunner cin _ _ <- deRefStablePtr bptr
    freeStablePtr bptr
    writeChan cin MsgShutdown

receiveBlock :: StablePtr BakerRunner -> CString -> Int64 -> IO ()
receiveBlock bptr cstr l = do
    BakerRunner cin _ _ <- deRefStablePtr bptr
    blockBS <- BS.packCStringLen (cstr, fromIntegral l)
    case runGet deserializeBlock blockBS of
        Left _ -> return ()
        Right block -> writeChan cin $ MsgBlockReceived block

receiveFinalization :: StablePtr BakerRunner -> CString -> Int64 -> IO ()
receiveFinalization bptr cstr l = do
    BakerRunner cin _ _ <- deRefStablePtr bptr
    bs <- BS.packCStringLen (cstr, fromIntegral l)
    writeChan cin $ MsgFinalizationReceived bs

receiveFinalizationRecord :: StablePtr BakerRunner -> CString -> Int64 -> IO ()
receiveFinalizationRecord bptr cstr l = do
    BakerRunner cin _ _ <- deRefStablePtr bptr
    finRecBS <- BS.packCStringLen (cstr, fromIntegral l)
    case runGet get finRecBS of
        Left _ -> return ()
        Right finRec -> writeChan cin $ MsgFinalizationRecordReceived finRec

printBlock :: CString -> Int64 -> IO ()
printBlock cstr l = do
    blockBS <- BS.packCStringLen (cstr, fromIntegral l)
    case runGet deserializeBlock blockBS of
        Left _ -> putStrLn "<Bad Block>"
        Right block -> putStrLn $ showsBlock block ""

receiveTransaction :: StablePtr BakerRunner -> CString -> IO Int64
receiveTransaction bptr tdata = do
    BakerRunner cin _ _ <- deRefStablePtr bptr
    tbs <- BS.packCString tdata
    case Get.txOutToTransaction =<< (AE.decodeStrict tbs) of
      Nothing -> return 1
      Just tx ->
        writeChan cin (MsgTransactionReceived tx) >> return 0

getBestBlockInfo :: StablePtr BakerRunner -> IO CString
getBestBlockInfo bptr = do
  blockInfo <- readIORef =<< bakerBestBlock <$> deRefStablePtr bptr
  let outStr = LT.unpack . AET.encodeToLazyText $ blockInfo
  newCString outStr

freeCStr :: CString -> IO ()
freeCStr = free

foreign export ccall makeGenesisData :: Timestamp -> Word64 -> FunPtr CStringCallback -> FunPtr (Int64 -> CStringCallback) -> IO ()
foreign export ccall startBaker :: CString -> Int64 -> CString -> Int64 -> FunPtr BlockCallback -> FunPtr LogCallback -> IO (StablePtr BakerRunner)
foreign export ccall stopBaker :: StablePtr BakerRunner -> IO ()
foreign export ccall receiveBlock :: StablePtr BakerRunner -> CString -> Int64 -> IO ()
foreign export ccall receiveFinalization :: StablePtr BakerRunner -> CString -> Int64 -> IO ()
foreign export ccall receiveFinalizationRecord :: StablePtr BakerRunner -> CString -> Int64 -> IO ()
foreign export ccall printBlock :: CString -> Int64 -> IO ()
foreign export ccall receiveTransaction :: StablePtr BakerRunner -> CString -> IO Int64
foreign export ccall getBestBlockInfo :: StablePtr BakerRunner -> IO CString
foreign export ccall freeCStr :: CString -> IO ()
