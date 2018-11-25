{-# LANGUAGE ForeignFunctionInterface #-}
module Concordium.External where

import Foreign
import Foreign.C
import Control.Concurrent.Chan
import Control.Concurrent
import System.Random
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as Map
import Data.Serialize

import qualified Concordium.Crypto.DummySignature as Sig
import qualified Concordium.Crypto.DummyVRF as VRF

import Concordium.Types
import Concordium.Birk.Bake
import Concordium.Payload.Transaction
import Concordium.Runner
import Concordium.Show


type I32 = Int32

-- Test functions

-- triple :: I32 -> I32
-- triple x = 3 * x

-- foreign export ccall triple :: I32 -> I32

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
    bakerOutChan :: Chan OutMessage
}

makeBakerIdentity :: BakerId -> BakerIdentity
makeBakerIdentity bid = mbs (mkStdGen 17)
    where
        mbs gen = BakerIdentity bid ssk esk
            where
                (VRF.KeyPair esk epk, gen') = random gen
                (Sig.KeyPair ssk spk, gen'') = random gen'

makeBakerInfos :: Word64 -> [BakerInfo]
makeBakerInfos nBakers = take (fromIntegral nBakers) $ mbs (mkStdGen 17)
    where
        mbs gen = BakerInfo epk spk lot:mbs gen''
            where
                (VRF.KeyPair esk epk, gen') = random gen
                (Sig.KeyPair ssk spk, gen'') = random gen'
        lot = 1.0 / fromIntegral nBakers

type CStringCallback = CString -> Int64 -> IO ()
foreign import ccall "dynamic" callCStringCallback :: FunPtr CStringCallback -> CStringCallback

makeGenesisData :: 
    Timestamp -- ^Genesis time
    -> Word64 -- ^Number of bakers
    -> FunPtr CStringCallback -- ^Function to process the generated genesis data.
    -> IO ()
makeGenesisData genTime nBakers cbk = do
    BS.useAsCStringLen (encode genData) $ \(cdata, clen) -> callCStringCallback cbk cdata (fromIntegral clen)
    where
        bps = BirkParameters (BS.pack "LeadershipElectionNonce")
                             0.5 -- voting power
                             (Map.fromList $ zip [0..] (makeBakerInfos nBakers))
        fps = FinalizationParameters Map.empty
        genData = GenesisData genTime
                              10 -- slot time in seconds
                              bps
                              fps 

-- makeStartupData ::
--     Timestamp   -- ^Genesis time
--     -> Word64   -- ^Number of bakers
--     -> Word64   -- ^Index of desired baker (0 <= _ < number of bakers)
--     -> (GenesisData, BakerIdentity)
-- makeStartupData genTime nBakers bIndex = (genData, bid)
--     where
--         bakeShare = (1.0 / (fromIntegral nBakers))
--         bakers = take (fromIntegral nBakers) $ zip [0..] (makeBakers bakeShare)
--         bps = BirkParameters (BS.pack "LeadershipElectionNonce")
--             0.5 -- Voting power
--             (Map.fromList [(i,b) | (i, (b, _)) <- bakers])
--         fps = FinalizationParameters Map.empty
--         genData = GenesisData genTime
--                 10 -- Slot time in seconds
--                 bps
--                 fps
--         bid = snd $ snd (bakers !! (fromIntegral bIndex))

type BlockCallback = CString -> Int64 -> IO ()

foreign import ccall "dynamic" callBlockCallback :: FunPtr BlockCallback -> BlockCallback

outLoop :: Chan OutMessage -> BlockCallback -> IO ()
outLoop chan cbk = do
    (MsgNewBlock block) <- readChan chan
    let bbs = runPut (serializeBlock block)
    BS.useAsCStringLen bbs $ \(cstr, l) -> cbk cstr (fromIntegral l)
    outLoop chan cbk

startBaker :: Word64 -- ^Baker id
           -> CString -> Int64 -- ^Serialized genesis data (c string + len)
           -> FunPtr BlockCallback -> IO (StablePtr BakerRunner)
startBaker bIndex gdataC gdataLenC bcbk = do
    let bid = makeBakerIdentity bIndex
    gdata <- BS.packCStringLen (gdataC, fromIntegral gdataLenC)
    case decode gdata of
      Left err -> ioError (userError err)
      Right genData -> do
        (cin, cout) <- makeRunner bid genData
        forkIO $ outLoop cout (callBlockCallback bcbk)
        newStablePtr (BakerRunner cin cout)

stopBaker :: StablePtr BakerRunner -> IO ()
stopBaker bptr = do
    BakerRunner cin _ <- deRefStablePtr bptr
    freeStablePtr bptr
    writeChan cin MsgShutdown

receiveBlock :: StablePtr BakerRunner -> CString -> Int64 -> IO ()
receiveBlock bptr cstr l = do
    BakerRunner cin _ <- deRefStablePtr bptr
    blockBS <- BS.packCStringLen (cstr, fromIntegral l)
    case runGet deserializeBlock blockBS of
        Left _ -> return ()
        Right block -> writeChan cin $ MsgBlockReceived block

printBlock :: CString -> Int64 -> IO ()
printBlock cstr l = do
    blockBS <- BS.packCStringLen (cstr, fromIntegral l)
    case runGet deserializeBlock blockBS of
        Left _ -> putStrLn "<Bad Block>"
        Right block -> putStrLn $ showsBlock block ""

receiveTransaction :: StablePtr BakerRunner -> Word64 -> Word64 -> Word64 -> Word64 -> CString -> Int64 -> IO ()
receiveTransaction bptr n0 n1 n2 n3 tdata tlen = do
    BakerRunner cin _ <- deRefStablePtr bptr
    tbs <- BS.packCStringLen (tdata, fromIntegral tlen)
    writeChan cin $ MsgTransactionReceived (Transaction (TransactionNonce n0 n1 n2 n3) tbs)

foreign export ccall makeGenesisData :: Timestamp -> Word64 -> FunPtr CStringCallback -> IO ()
foreign export ccall startBaker :: Word64 -> CString -> Int64 -> FunPtr BlockCallback -> IO (StablePtr BakerRunner)
foreign export ccall stopBaker :: StablePtr BakerRunner -> IO ()
foreign export ccall receiveBlock :: StablePtr BakerRunner -> CString -> Int64 -> IO ()
foreign export ccall printBlock :: CString -> Int64 -> IO ()
foreign export ccall receiveTransaction :: StablePtr BakerRunner -> Word64 -> Word64 -> Word64 -> Word64 -> CString -> Int64 -> IO ()
