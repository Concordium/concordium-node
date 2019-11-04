{-# LANGUAGE TupleSections, LambdaCase, OverloadedStrings, TypeFamilies #-}
{-# LANGUAGE LambdaCase, CPP #-}
module Main where

import Control.Concurrent
import Control.Monad
import System.Random
import Data.Time.Clock.POSIX
import System.IO
import Data.IORef
import Data.Serialize

import Concordium.TimerMonad
import Concordium.Types.HashableTo
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Transactions
import Concordium.GlobalState.Block
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Instances

import Concordium.GlobalState.Basic.Block
import qualified Concordium.GlobalState.Basic.BlockState as Basic
import Concordium.GlobalState.BlockState
import Concordium.GlobalState
#ifdef RUST
import qualified Concordium.GlobalState.Implementation as Rust
#endif

import Concordium.Scheduler.Utils.Init.Example as Example

import Concordium.Types
import Concordium.Runner
import Concordium.Logger
import Concordium.Skov
-- import Concordium.Skov.CatchUp
import Concordium.Getters
import Concordium.Afgjort.Finalize (FinalizationPseudoMessage(..),FinalizationInstance(..))
import Concordium.Birk.Bake

import Concordium.Startup

#ifdef RUST
type TreeConfig = DiskTreeDiskBlockConfig
makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> IO TreeConfig
makeGlobalStateConfig rt genData = do
    gsptr <- Rust.makeEmptyGlobalState genData
    return $ DTDBConfig rt genData (genesisState genData) gsptr
#else
type TreeConfig = MemoryTreeDiskBlockConfig
makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> IO TreeConfig
makeGlobalStateConfig rt genData = return $ MTDBConfig rt genData (genesisState genData)
#endif

type ActiveConfig = SkovConfig TreeConfig (BufferedFinalization ThreadTimer) HookLogHandler


data Peer = Peer {
    -- peerState :: MVar SkovBufferedHookedState,
    peerChan :: Chan (InMessage Peer)
}


nContracts :: Int
nContracts = 2

transactions :: StdGen -> [BareTransaction]
transactions gen = trs (0 :: Nonce) (randoms gen :: [Int])
    where
        contr i = ContractAddress (fromIntegral $ i `mod` nContracts) 0
        trs n (a : b : rs) = Example.makeTransaction (a `mod` 9 /= 0) (contr b) n : trs (n+1) rs
        trs _ _ = error "Ran out of transaction data"

sendTransactions :: Chan (InMessage Peer) -> [BareTransaction] -> IO ()
sendTransactions chan (t : ts) = do
        writeChan chan (MsgTransactionReceived $ runPut $ put t)
        -- r <- randomRIO (5000, 15000)
        threadDelay 100000
        sendTransactions chan ts
sendTransactions _ _ = return ()

{-
relayIn :: Chan InEvent -> Chan (InMessage Peer) -> MVar SkovBufferedHookedState -> IORef Bool -> IO ()
relayIn msgChan bakerChan sfsRef connectedRef = loop
    where
        loop = do
            msg <- readChan msgChan
            connected <- readIORef connectedRef
            when connected $ case msg of
                IEMessage imsg -> writeChan bakerChan imsg
                IECatchUpRequired peer -> undefined
{-}                IECatchupFinalization fpBS reciprocate chan -> do
                    case runGet get fpBS of
                        Right fp -> do
                            finMsgs <- Get.getFinalizationMessages sfsRef fp
                            forM_ finMsgs $ writeChan chan . IEMessage . MsgFinalizationReceived sfsRef . runPut . put
                            when reciprocate $ do
                                myFp <- runPut . put <$> Get.getFinalizationPoint sfsRef
                                writeChan chan $ IECatchupFinalization myFp False msgChan
                        Left _ -> return () -}
            loop
-}


relay :: Peer -> Chan (OutMessage Peer) -> SyncRunner ActiveConfig -> IORef Bool -> Chan (Either (BlockHash, BakedBlock, [Instance]) FinalizationRecord) -> Chan (InMessage Peer) -> [Chan (InMessage Peer)] -> IO ()
relay myPeer inp sr connectedRef monitor loopback outps = loop
    where
        chooseDelay = do
            factor <- (/2) . (+1) . sin . (*(pi/240)) . fromRational . toRational <$> getPOSIXTime
            truncate . (*(factor :: Double)) . fromInteger . (`div` 10) . (^(2::Int)) <$> randomRIO (0, 7800)
        delayed a = void $ forkIO $ do
            threadDelay =<< chooseDelay
            a
        usually a = do
            -- Do the action most of the time, but randomly don't do it.
            r <- randomRIO (0,9::Int)
            unless (r == 0) a
        loop = do
            msg <- readChan inp
            connected <- readIORef connectedRef
            now <- getTransactionTime
            -- If we're connected, relay the message as required
            if connected then case msg of
                MsgNewBlock blockBS -> do
                    case runGet (getBlock now) blockBS of
                        Right (NormalBlock block) -> do
                            let bh = getHash block :: BlockHash
                            bi <- runStateQuery sr (bInsts bh)
                            writeChan monitor (Left (bh, block, bi))
                        _ -> return ()
                    forM_ outps $ \outp -> usually $ delayed $
                        writeChan outp (MsgBlockReceived myPeer blockBS)
                MsgFinalization bs ->
                    -- We drop all finalization messages except catch-ups
                    case decode bs of
                        Right (FPMCatchUp _) ->
                            forM_ outps $ \outp -> delayed $
                                writeChan outp (MsgFinalizationReceived myPeer bs)
                        _ -> return ()
                MsgFinalizationRecord fr -> do
                    case runGet get fr of
                        Right fr' -> writeChan monitor (Right fr')
                        _ -> return ()
                    forM_ outps $ \outp -> usually $ delayed $
                        writeChan outp (MsgFinalizationRecordReceived myPeer fr)
                MsgCatchUpRequired target -> do
                    cur <- getCatchUpStatus sr
                    delayed $ writeChan (peerChan target) (MsgCatchUpStatusReceived myPeer (encode cur))
                MsgDirectedBlock target b -> usually $ delayed $ writeChan (peerChan target) (MsgBlockReceived myPeer b)
                MsgDirectedFinalizationRecord target fr -> usually $ delayed $ writeChan (peerChan target) (MsgFinalizationReceived myPeer fr)
                MsgDirectedCatchUpStatus target cu -> usually $ delayed $ writeChan (peerChan target) (MsgCatchUpStatusReceived myPeer cu)
            -- If we're not connected, don't relay, but still send to the monitor channel
            else case msg of
                MsgNewBlock blockBS -> do
                    case runGet (getBlock now) blockBS of
                        Right (NormalBlock block) -> do
                            let bh = getHash block :: BlockHash
                            bi <- runStateQuery sr (bInsts bh)
                            writeChan monitor (Left (bh, block, bi))
                        _ -> return ()
                MsgFinalizationRecord fr -> case runGet get fr of
                        Right fr' -> writeChan monitor (Right fr')
                        _ -> return ()
                _ -> return ()
            loop
        bInsts bh = do
            bst <- resolveBlock bh
            case bst of
                Nothing -> return []
                Just bs -> getContractInstanceList (bpState bs)

toggleConnection :: LogMethod IO -> SyncRunner ActiveConfig -> IORef Bool -> Chan (InMessage Peer) -> [Chan (InMessage Peer)] -> IO ()
toggleConnection logM sr connectedRef loopback outps = readIORef connectedRef >>= loop
    where
        loop connected = do
            delay <- (^(2::Int)) <$> randomRIO (if connected then (3200,7800) else (0,4500))
            threadDelay delay
            tid <- myThreadId
            if connected then do
                putStrLn $ "// " ++ show tid ++ ": toggle off"
                logM External LLInfo $ "Disconnected"
                writeIORef connectedRef False
                loop False
            else do
                -- Reconnect
                putStrLn $ "// " ++ show tid ++ ": toggle on"
                logM External LLInfo $ "Reconnected"
                writeIORef connectedRef True
                -- TODO: could force catch up with peers here
                {-
                fp <- Get.getFinalizationPoint sfsRef
                forM_ outps $ \outp -> writeChan outp (IECatchupFinalization (runPut $ put fp) True loopback)
                -}
                -- FIXME: here
                loop True


removeEach :: [a] -> [(a,[a])]
removeEach = re []
    where
        re l (x:xs) = (x,l++xs) : re (x:l) xs
        re _ [] = []

{-
gsToString :: PersistentBlockState -> String
gsToString gs = intercalate "\\l" . map show $ keys
    where
        ca n = ContractAddress (fromIntegral n) 0
        keys = map (\n -> (n, instanceModel <$> getInstance (ca n) (gs ^. blockInstances))) $ enumFromTo 0 (nContracts-1)
-}

dummyIdentityProviders :: [IpInfo]
dummyIdentityProviders = []

genesisState :: GenesisData -> Basic.BlockState
genesisState genData = Basic.initialState
                       (genesisBirkParameters genData)
                       (genesisCryptographicParameters genData)
                       (genesisAccounts genData ++ genesisSpecialBetaAccounts genData)
                       (genesisIdentityProviders genData)
                       (genesisMintPerSlot genData)

main :: IO ()
main = do
    let n = 3
    now <- truncate <$> getPOSIXTime
    let (gen, bis) = makeGenesisData now n 1 0.5 1 dummyCryptographicParameters dummyIdentityProviders []
    trans <- transactions <$> newStdGen
    chans <- mapM (\(bakerId, (bid, _)) -> do
        let logFile = "consensus-" ++ show now ++ "-" ++ show bakerId ++ ".log"
        logChan <- newChan
        let logLoop = do
                logMsg <- readChan logChan
                appendFile logFile logMsg
                logLoop
        _ <- forkIO logLoop
        let logM src lvl msg = do
                                    timestamp <- getCurrentTime
                                    writeChan logChan $ "[" ++ show timestamp ++ "] " ++ show lvl ++ " - " ++ show src ++ ": " ++ msg ++ "\n"
        let logTransferFile = "transfer-log-" ++ show now ++ "-" ++ show bakerId ++ ".transfers"
        let logT bh slot reason = do
              appendFile logTransferFile (show (bh, slot, reason))
              appendFile logTransferFile "\n"
        gsconfig <- makeGlobalStateConfig defaultRuntimeParameters gen
        let
            finconfig = BufferedFinalization (FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid)) gen
            hconfig = HookLogHandler (Just logT)
            config = SkovConfig gsconfig finconfig hconfig
        (cin, cout, sr) <- makeAsyncRunner logM bid config
        -- cin' <- newChan
        connectedRef <- newIORef True
        -- _ <- forkIO $ relayIn cin' cin stateRef connectedRef
        _ <- forkIO $ sendTransactions cin trans
        return (cin, cout, sr, connectedRef, logM)) (zip [0::Int ..] bis)
    monitorChan <- newChan
    forM_ (removeEach chans) $ \((cin, cout, sr, connectedRef, logM), cs) -> do
        let cs' = ((\(c, _, _, _, _) -> c) <$> cs)
        when False $ do
            _ <- forkIO $ toggleConnection logM sr connectedRef cin cs'
            return ()
        forkIO $ relay (Peer cin) cout sr connectedRef monitorChan cin cs'
    let loop = do
            readChan monitorChan >>= \case
                Left (bh, block, gs') -> do
                    let ts = blockTransactions block
                    let stateStr = show gs' {-case gs' of
                                    Nothing -> ""
                                    Just gs -> gsToString gs -}
                    putStrLn $ " n" ++ show bh ++ " [label=\"" ++ show (blockBaker $ bbFields block) ++ ": " ++ show (blockSlot block) ++ " [" ++ show (length ts) ++ "]\\l" ++ stateStr ++ "\\l\"];"
                    putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockPointer $ bbFields block) ++ ";"
                    putStrLn $ " n" ++ show bh ++ " -> n" ++ show (blockLastFinalized $ bbFields block) ++ " [style=dotted];"
                    hFlush stdout
                    loop
                Right fr -> do
                    putStrLn $ " n" ++ show (finalizationBlockPointer fr) ++ " [color=green];"
                    loop
    loop
