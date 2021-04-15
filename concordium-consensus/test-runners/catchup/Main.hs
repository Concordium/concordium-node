{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |This module defines a test runner for consensus that spawns multiple baker instances and allows
-- them to communicate. It produces a graph-viz formatted graph as output showing the blocks.
module Main where

import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import qualified Data.Map as Map
import Data.Serialize
import Data.Time.Clock.POSIX
import Data.Word
import System.Directory
import System.FilePath
import System.IO
import System.Random

import Concordium.Afgjort.Finalize (FinalizationInstance (..))
import Concordium.Birk.Bake
import qualified Concordium.Crypto.DummyData as Dummy
import Concordium.GlobalState
import Concordium.GlobalState.Block
import Concordium.GlobalState.DummyData (dummyAuthorizations)
import qualified Concordium.GlobalState.DummyData as Dummy
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.Kontrol (currentTimestamp)
import Concordium.Logger
import Concordium.MultiVersion
import qualified Concordium.ProtocolUpdate.P1.Reboot as P1.Reboot
import Concordium.Skov hiding (receiveTransaction)
import Concordium.Startup
import Concordium.TimerMonad
import Concordium.Types
import qualified Concordium.Types.DummyData as Dummy
import Concordium.Types.Transactions
import Concordium.Types.Updates

-- |Protocol version
type PV = 'P1

-- |Number of bakers to create.
numberOfBakers :: Num n => n
numberOfBakers = 10

-- |Finalization parameters to use for the test.
myFinalizationParameters :: FinalizationParameters
myFinalizationParameters =
    defaultFinalizationParameters
        { finalizationCommitteeMaxSize = 3 * numberOfBakers + 1,
          finalizationSkipShrinkFactor = 0.8,
          finalizationSkipGrowFactor = 1.25,
          finalizationAllowZeroDelay = True
        }

-- |A protocol update payload that will restart the chain (still at protocol version P1), but
-- with an updated slot duration and election difficulty.
updatePayload :: ProtocolUpdate
updatePayload =
    ProtocolUpdate
        { puMessage = "Rebooting consensus",
          puSpecificationURL = "https://concordium.com",
          puSpecificationHash = P1.Reboot.updateHash,
          puSpecificationAuxiliaryData =
            encode $
                P1.Reboot.UpdateData
                    { updateSlotDuration = 2000,
                      updateElectionDifficulty = makeElectionDifficulty 50000,
                      updateEpochLength = 20,
                      updateMaxBlockEnergy = Energy maxBound,
                      updateFinalizationParameters = myFinalizationParameters
                    }
        }

-- |Produces a list consisting of a single protocol update transaction.
-- The transaction is scheduled for one minute after the suppied timestamp, and will
-- time out 30 seconds prior to this.
protocolUpdateTransactions :: Timestamp -> [BlockItem]
protocolUpdateTransactions (Timestamp ts) = [ui]
  where
    ui =
        addMetadata id 0 $
            ChainUpdate $
                makeUpdateInstruction
                    RawUpdateInstruction
                        { ruiSeqNumber = 1,
                          ruiEffectiveTime = fromIntegral (ts `div` 1000) + 60,
                          ruiTimeout = fromIntegral (ts `div` 1000) + 30,
                          ruiPayload = ProtocolUpdatePayload updatePayload
                        }
                    (Map.singleton 0 Dummy.dummyAuthorizationKeyPair)

-- |Generates an infinite list of transfer transactions from a single account to itself.
transferTransactions :: StdGen -> [BlockItem]
transferTransactions gen = trs (0 :: Nonce) (randoms gen :: [Word8])
  where
    trs n (amnt : amnts) =
        Dummy.makeTransferTransaction
            (Dummy.mateuszKP, Dummy.mateuszAccount)
            Dummy.mateuszAccount
            (fromIntegral amnt)
            n :
        trs (n + 1) amnts
    trs _ _ = error "Ran out of transaction data"

-- |Notification of a block being baked, or a protocol update occurring, that should be rendered
-- as part of the produced graph.
data MonitorEvent
    = MEBlock GenesisIndex BS.ByteString
    | MERegenesis BlockHash
    | METoggle PeerId Bool

-- |A loop that continuously reads the monitor channel and prints a graphviz directed graph to
-- stdout.
monitorLoop :: Chan MonitorEvent -> IO ()
monitorLoop chan =
    bracket_ (putStrLn "digraph {") (putStrLn "}") $
        forever $
            readChan chan >>= \case
                MEBlock _ bs -> do
                    now <- getCurrentTime
                    case deserializeExactVersionedPendingBlock (protocolVersion @PV) bs now of
                        Left err -> error $ "Failed to deserialize a block: " ++ err
                        Right pb -> do
                            let bh = pbHash pb
                            let block = pbBlock pb
                            let ts = blockTransactions block
                            let finInfo = case bfBlockFinalizationData (bbFields block) of
                                    NoFinalizationData -> ""
                                    BlockFinalizationData fr -> "\\lfin.wits:" ++ show (finalizationProofParties $ finalizationProof fr)
                            putStrLn $ " n" ++ show bh ++ " [label=\"" ++ show (blockBaker block) ++ ": " ++ show (blockSlot block) ++ " [" ++ show (length ts) ++ "]" ++ finInfo ++ "\"];"
                            putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockPointer block) ++ ";"
                            case blockFinalizationData block of
                                NoFinalizationData -> return ()
                                BlockFinalizationData fr ->
                                    putStrLn $ " n" ++ show bh ++ " -> n" ++ show (finalizationBlockPointer fr) ++ " [style=dotted];"
                            hFlush stdout
                MERegenesis bh -> do
                    putStrLn $ " n" ++ show bh ++ " [label=\"" ++ show bh ++ "\",shape=rectangle,style=filled,color=blue];"
                    hFlush stdout
                METoggle peer newState -> do
                    putStrLn $
                        " // Baker " ++ show peer ++ " is now "
                            ++ if newState then "connected" else "disconnected"
                    hFlush stdout

-- |A random distribution
type Distribution x = StdGen -> (x, StdGen)

-- |Delay for a random time sampled from a distribution (in microseconds).
randomDelay :: Distribution Int -> IO ()
randomDelay distr = do
    delay <- getStdRandom distr
    threadDelay delay

-- |Fork a thread to perform an action after a random delay.
eventually :: Distribution Int -> IO a -> IO ()
eventually distr act = void $
    forkIO $ do
        randomDelay distr
        void act

-- | 'quadDelay f' is a distribution of up to roughly 'f' minutes (in microseconds).
-- Smaller delays are more likely, because this is based on squaring a uniform distribution.
quadDelay :: Double -> Distribution Int
quadDelay factor g = (r, g')
  where
    (r0, g') = randomR (0, 7800) g
    r = truncate $ factor * fromInteger (r0 * r0 `div` 10)

-- |Identifier for a peer. Since all peers are bakers, we just use 'BakerId'.
type PeerId = BakerId

-- |Representation of a peer, including its 'MultiVersionRunner' and catch-up information.
data Peer gsconfig finconfig = Peer
    { -- |Runner for the peer.
      peerMVR :: MultiVersionRunner gsconfig finconfig,
      -- |List of peers that are pending catch-up.
      peerCatchUp :: MVar [PeerId],
      -- |'MVar' is written to signal that catch-up is required.
      peerCatchUpSignal :: MVar (),
      -- |The peer's 'PeerId'.
      peerId :: PeerId,
      -- |The peer's connection status
      peerConnectionStatus :: IORef Bool
    }

-- |Construct a peer from its 'PeerId' and 'MultiVersionRunner'. Does not start any threads.
makePeer :: PeerId -> MultiVersionRunner gsconfig finconfig -> IO (Peer gsconfig finconfig)
makePeer peerId peerMVR = do
    peerCatchUp <- newMVar []
    peerCatchUpSignal <- newEmptyMVar
    peerConnectionStatus <- newIORef True
    return Peer{..}

-- |For a given 'Peer', consider the 'PeerId' to be a pending peer.
markPeerPending :: Peer g f -> PeerId -> IO ()
markPeerPending Peer{..} newPending = unless (newPending == peerId) $ do
    cul <- takeMVar peerCatchUp
    if newPending `elem` cul
        then putMVar peerCatchUp cul
        else do
            putMVar peerCatchUp (cul ++ [newPending])
            void $ tryPutMVar peerCatchUpSignal ()

-- |Interval (microseconds) at which to process the catch-up queue
catchUpInterval :: Int
catchUpInterval = 5_000_000

-- |Start the catch-up thread for a peer.
startCatchUpThread :: Peer g f -> Map.Map PeerId (Peer g f) -> IO ThreadId
startCatchUpThread myPeer peers = forkIO $
    forever $ do
        threadDelay catchUpInterval
        takeMVar (peerCatchUpSignal myPeer)
        cu <- takeMVar (peerCatchUp myPeer)
        case cu of
            [] -> return ()
            (nextPeer : rest) -> do
                putMVar (peerCatchUp myPeer) rest
                unless (null rest) $ void $ tryPutMVar (peerCatchUpSignal myPeer) ()
                forM_ (Map.lookup nextPeer peers) $ \peer -> eventually (quadDelay 0.02) $ do
                    (gi, curBS) <- runMVR getCatchUpRequest (peerMVR myPeer)
                    randomDelay (quadDelay 0.02)
                    mvLog (peerMVR myPeer) External LLDebug $ "Sending catch-up request to " ++ show (peerId peer)
                    peerReceive peer myPeer MessageCatchUpStatus gi (LBS.toStrict curBS)

-- |Handle an incoming message at the given target from the given source.
peerReceive :: Peer g f -> Peer g f -> MessageType -> GenesisIndex -> BS.ByteString -> IO ()
peerReceive target src MessageBlock genIndex msg = do
    mvLog (peerMVR target) External LLDebug $ "Received block from " ++ show (peerId src) ++ " genesisIndex:" ++ show genIndex
    res <- runMVR (receiveBlock genIndex msg) (peerMVR target)
    when (isPending res) $ markPeerPending target (peerId src)
peerReceive target src MessageFinalizationRecord genIndex msg = do
    mvLog (peerMVR target) External LLDebug $ "Received finalization record from " ++ show (peerId src) ++ " genesisIndex:" ++ show genIndex
    res <- runMVR (receiveFinalizationRecord genIndex msg) (peerMVR target)
    when (isPending res) $ markPeerPending target (peerId src)
peerReceive target src MessageFinalization genIndex msg = do
    mvLog (peerMVR target) External LLDebug $ "Received finalization message from " ++ show (peerId src) ++ " genesisIndex:" ++ show genIndex
    -- Note: according to spec, we should potentially mark the peer as pending here.
    void $ runMVR (receiveFinalizationMessage genIndex msg) (peerMVR target)
peerReceive target src MessageCatchUpStatus genIndex msg = do
    mvLog (peerMVR target) External LLDebug $ "Received catch-up status message from " ++ show (peerId src) ++ " genesisIndex:" ++ show genIndex
    let catchUpCallback mt bs = do
            randomDelay (quadDelay 0.01)
            peerReceive src target mt genIndex bs
        cuc = CatchUpConfiguration{catchUpMessageLimit = 5, ..}
    res <- runMVR (receiveCatchUpStatus genIndex msg cuc) (peerMVR target)
    when (isPending res || res == ResultContinueCatchUp) $ markPeerPending target (peerId src)

-- |Check if an 'UpdateResult' indicates a pending status.
isPending :: UpdateResult -> Bool
isPending ResultPendingBlock = True
isPending ResultPendingFinalization = True
isPending ResultIncorrectFinalizationSession = True
isPending _ = False

-- |Send a given message to all peers.
toAllPeers ::
    PeerId ->
    IORef (Map.Map PeerId (Peer gsconfig finconfig)) ->
    MessageType ->
    GenesisIndex ->
    BS.ByteString ->
    IO ()
toAllPeers src peersRef mt genIndex msg = do
    peers <- readIORef peersRef
    forM_ (Map.lookup src peers) $ \myPeer -> do
        isMyPeerConnected <- readIORef (peerConnectionStatus myPeer)
        when isMyPeerConnected $ do
            mvLog (peerMVR myPeer) External LLDebug $ "Broadcasting message of type " ++ show mt
            forM_ peers $ \peer ->
                unless (peerId peer == src) $ do
                    isPeerConnected <- readIORef (peerConnectionStatus peer)
                    when isPeerConnected $
                        eventually (quadDelay 0.05) $
                            peerReceive peer myPeer mt genIndex msg

-- |Instance of 'Callbacks' for a specific peer.
callbacks ::
    PeerId ->
    IORef (Map.Map PeerId (Peer gsconfig finconfig)) ->
    Chan MonitorEvent ->
    Callbacks
callbacks myPeerId peersRef monitorChan = Callbacks{..}
  where
    broadcastBlock gi bs = do
        writeChan monitorChan $ MEBlock gi bs
        toAllPeers myPeerId peersRef MessageBlock gi bs
    broadcastFinalizationMessage gi bs =
        toAllPeers myPeerId peersRef MessageFinalization gi bs
    broadcastFinalizationRecord gi bs =
        toAllPeers myPeerId peersRef MessageFinalizationRecord gi bs
    notifyCatchUpStatus gi bs =
        toAllPeers myPeerId peersRef MessageCatchUpStatus gi bs
    notifyRegenesis gbh = do
        writeChan monitorChan $ MERegenesis gbh
        peers <- readIORef peersRef
        forM_ (Map.lookup myPeerId peers) $ \myPeer ->
            forM_ peers $ \peer -> markPeerPending myPeer (peerId peer)

-- |Construct a 'MultiVersionConfiguration' to use for each baker node.
config :: FilePath -> BakerIdentity -> MultiVersionConfiguration DiskTreeDiskBlockConfig (BufferedFinalization ThreadTimer)
config dataPath bid = MultiVersionConfiguration{..}
  where
    mvcTXLogConfig = ()
    mvcStateConfig = DiskStateConfig dataPath
    mvcFinalizationConfig =
        BufferedFinalization
            ( FinalizationInstance
                (bakerSignKey bid)
                (bakerElectionKey bid)
                (bakerAggregationKey bid)
            )
    mvcRuntimeParameters = defaultRuntimeParameters

-- |Start a thread sending transactions to a particular node.
startTransactionThread :: [BlockItem] -> MultiVersionRunner gsconfig finconfig -> IO ThreadId
startTransactionThread trs0 mvr = forkIO $ transactionLoop trs0
  where
    transactionLoop [] = return ()
    transactionLoop (tr : trs) = do
        threadDelay 500
        _ <- runMVR (receiveTransaction (runPut $ putVersionedBlockItemV0 tr)) mvr
        transactionLoop trs

startTogglePeersThread :: Chan MonitorEvent -> Map.Map PeerId (Peer gsconfig finconfig) -> IO ThreadId
startTogglePeersThread monitorChan peers = forkIO $
    forever $ do
        randomDelay (quadDelay 10)
        ind <- randomRIO (0, Map.size peers - 1)
        let (_, peer) = Map.elemAt ind peers
        oldStatus <- readIORef (peerConnectionStatus peer)
        writeIORef (peerConnectionStatus peer) (not oldStatus)
        unless oldStatus $ forM_ peers $ \otherPeer -> do
            otherStatus <- readIORef (peerConnectionStatus otherPeer)
            when otherStatus $ markPeerPending peer (peerId otherPeer)
        writeChan monitorChan $ METoggle (peerId peer) (not oldStatus)

-- |Main runner that starts the bakers and monitors the results.
main :: IO ()
main = do
    -- Set genesis at the next whole second
    now <- (\(Timestamp t) -> Timestamp $ ((t `div` 1000) + 1) * 1000) <$> currentTimestamp
    let (genesisData, bakerIdentities, _) =
            makeGenesisData @PV
                now
                numberOfBakers
                2000
                myFinalizationParameters
                Dummy.dummyCryptographicParameters
                Dummy.dummyIdentityProviders
                Dummy.dummyArs
                [Dummy.createCustomAccount 1000000000000 Dummy.mateuszKP Dummy.mateuszAccount]
                (Energy maxBound)
                dummyAuthorizations
                (makeChainParameters (makeElectionDifficulty 20000) 1 1 4 10 Dummy.dummyRewardParameters numberOfBakers 300000000000)
    peersRef <- newIORef Map.empty
    monitorChan <- newChan
    peers <-
        Map.fromList
            <$> forM
                bakerIdentities
                ( \(bid, _) -> do
                    let s = show now ++ "-" ++ show (bakerId bid)
                        d = "data" </> ("db" ++ s)
                    createDirectoryIfMissing True d
                    let bconfig = config d bid
                    logFile <- openFile ("consensus-" ++ s ++ ".log") WriteMode
                    let blogger src lvl msg = {- when (lvl == LLInfo) $ -} do
                            timestamp <- getCurrentTime
                            hPutStrLn logFile $ "[" ++ show timestamp ++ "] " ++ show lvl ++ " - " ++ show src ++ ": " ++ msg
                            hFlush logFile
                    let cbks = callbacks (bakerId bid) peersRef monitorChan
                    mvr <- makeMultiVersionRunner bconfig cbks (Just bid) blogger (PVGenesisData genesisData)
                    (bakerId bid,) <$> makePeer (bakerId bid) mvr
                )

    writeIORef peersRef peers
    -- Start the bakers
    forM_ peers $ \peer -> do
        _ <- startCatchUpThread peer peers
        startBaker (peerMVR peer)
    _ <- startTogglePeersThread monitorChan peers
    trTrans <- transferTransactions <$> newStdGen
    let transactions = protocolUpdateTransactions now ++ trTrans
    forM_ peers $ startTransactionThread transactions . peerMVR
    monitorLoop monitorChan