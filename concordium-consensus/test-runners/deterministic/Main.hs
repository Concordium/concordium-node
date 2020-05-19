{-# LANGUAGE ViewPatterns, TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module Main where

import qualified Data.Vector as Vec
import Control.Monad
import Control.Monad.IO.Class
import Lens.Micro.Platform
import Data.Time.Clock.POSIX
import Data.Time.Clock
import Control.Monad.Trans.State (StateT(..),execStateT)
import Control.Monad.State.Class
import qualified Data.PQueue.Min as MinPQ

import Concordium.Afgjort.Finalize.Types
import Concordium.Types
import qualified Concordium.GlobalState.Basic.BlockState as BState
import qualified Concordium.GlobalState.BlockPointer as BS
import Concordium.Types.Transactions
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.Block
import Concordium.GlobalState

import qualified Concordium.Scheduler.Utils.Init.Example as Example
import Concordium.Skov.Monad
import Concordium.Skov.MonadImplementations
import Concordium.Afgjort.Finalize
import Concordium.Logger
import Concordium.Birk.Bake
import Concordium.TimerMonad
import Concordium.Kontrol

import Concordium.Startup

import Concordium.Crypto.DummyData
import Concordium.Types.DummyData (mateuszAccount)

data DummyTimer = DummyTimer Integer

type BakerConfig = SkovConfig DiskTreeDiskBlockConfig (ActiveFinalization DummyTimer) NoHandler

dummyIdentityProviders :: [IpInfo]
dummyIdentityProviders = []

genesisState :: GenesisData -> BState.BlockState
genesisState GenesisData{..} = Example.initialState
                       (BState.BasicBirkParameters genesisElectionDifficulty genesisBakers genesisBakers genesisBakers genesisSeedState)
                       genesisCryptographicParameters
                       genesisAccounts
                       genesisIdentityProviders
                       2
                       genesisControlAccounts

makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> IO DiskTreeDiskBlockConfig
makeGlobalStateConfig rt genData = return $ DTDBConfig rt genData (genesisState genData)

type MyHandlers = SkovHandlers DummyTimer BakerConfig (StateT SimState LogIO)

type BakerM a = SkovT MyHandlers BakerConfig (StateT SimState LogIO) a

data Event
    = EBake Slot
    | EBlock BakedBlock
    | ETransaction BlockItem
    | EFinalization FinalizationPseudoMessage
    | EFinalizationRecord FinalizationRecord
    | ETimer Integer (BakerM ())

instance Show Event where
    show (EBake sl) = "Bake in slot " ++ show sl
    show (EBlock _) = "Receive block"
    show (ETransaction _) = "Receive transaction"
    show (EFinalization _) = "Receive finalization message"
    show (EFinalizationRecord _ ) = "Receive finalization record"
    show (ETimer _ _) = "Timer event"

data GEvent
    = BakerEvent Int Event
    -- ^An event for a particular baker
    | TransactionEvent (Nonce -> (BlockItem, GEvent))
    -- ^Spawn a transaction to send to bakers

data PEvent = PEvent Integer GEvent
instance Eq PEvent where
    (PEvent i1 _) == (PEvent i2 _) = i1 == i2
instance Ord PEvent where
    compare (PEvent i1 _) (PEvent i2 _) = compare i1 i2

data BakerState = BakerState {
    _bsIdentity :: BakerIdentity,
    _bsInfo :: BakerInfo,
--    _bsKeyPair :: SigScheme.KeyPair,
    _bsContext :: SkovContext BakerConfig,
    _bsState :: SkovState BakerConfig
}

data SimState = SimState {
    _ssBakers :: Vec.Vector BakerState,
    _ssEvents :: MinPQ.MinQueue PEvent,
    _ssNextTransactionNonce :: Nonce,
    _ssNextTimer :: Integer
}

makeLenses ''BakerState
makeLenses ''SimState



maxBakerId :: (Integral a) => a
maxBakerId = 19

allBakers :: (Integral a) => [a]
allBakers = [0..maxBakerId]

initialState :: IO SimState
initialState = do
    now <- currentTimestamp
    _ssBakers <- Vec.fromList <$> mapM (mkBakerState now) (zip [0..] bakers)
    return SimState {..}
    where
        (genData, bakers) = makeGenesisData
                                0 (maxBakerId + 1) 1000 0.2 defaultFinalizationParameters dummyCryptographicParameters dummyIdentityProviders
                                [Example.createCustomAccount 1000000000000 mateuszKP mateuszAccount] (Energy maxBound)
        mkBakerState :: Timestamp -> (BakerId, (BakerIdentity, BakerInfo)) -> IO BakerState
        mkBakerState now (bakerId, (_bsIdentity, _bsInfo)) = do
            gsconfig <- makeGlobalStateConfig (defaultRuntimeParameters { rpTreeStateDir = "data/treestate-" ++ show now ++ "-" ++ show bakerId, rpBlockStateFile = "data/blockstate-" ++ show now ++ "-" ++ show bakerId }) genData --dbConnString
            let
                finconfig = ActiveFinalization (FinalizationInstance (bakerSignKey _bsIdentity) (bakerElectionKey _bsIdentity) (bakerAggregationKey _bsIdentity))
                hconfig = NoHandler
                config = SkovConfig gsconfig finconfig hconfig
            (_bsContext, _bsState) <- runLoggerT (initialiseSkov config) (logFor (fromIntegral bakerId))
            return BakerState{..}
        _ssEvents = MinPQ.fromList [PEvent 0 (BakerEvent i (EBake 0)) | i <- allBakers]
        _ssNextTransactionNonce = 1
        _ssNextTimer = 0

logFor :: Int -> LogMethod IO
logFor i src lvl msg = putStrLn $ "[" ++ show i ++ ":" ++ show src ++ ":" ++ show lvl ++ "] " ++ show msg

layerLogger :: StateT s LogIO a -> LogMethod IO -> StateT s IO a
layerLogger a logm = do
    s <- get
    (r, s') <- liftIO $ runLoggerT (runStateT a s) logm
    put s'
    return r

runBaker :: Integer -> Int -> BakerM a -> StateT SimState IO a
runBaker curTime bid a = do
        bakerState <- (Vec.! bid) <$> use ssBakers
        (r, bsState') <- layerLogger (runSkovT a handlers (_bsContext bakerState) (_bsState bakerState)) (logFor bid) 
        ssBakers . ix bid . bsState .= bsState'
        return r
    where
        handlers = SkovHandlers {..}
        shBroadcastFinalizationMessage = broadcastEvent curTime . EFinalization
        shBroadcastFinalizationRecord = broadcastEvent curTime . EFinalizationRecord
        shOnTimeout timeout action = do
            tmr <- ssNextTimer <<%= (1+)
            t <- case timeout of
                DelayFor delay -> return $ curTime + round delay
                DelayUntil z -> do
                    now <- liftIO getCurrentTime
                    return $ curTime + round (diffUTCTime z now)
            ssEvents %= MinPQ.insert (PEvent t (BakerEvent bid (ETimer tmr (action >> return ()))))
            return $ DummyTimer tmr
        shCancelTimer (DummyTimer tmr) = ssEvents %= MinPQ.filter f
            where
                f (PEvent _ (BakerEvent _ (ETimer tmr' _))) = tmr' /= tmr
                f _ = True
        shPendingLive = return ()

broadcastEvent :: MonadState SimState m =>
                    Integer -> Event -> m ()
broadcastEvent curTime ev = ssEvents %= \e -> foldr MinPQ.insert e [PEvent curTime (BakerEvent i ev) | i <- allBakers]

stepConsensus :: StateT SimState IO ()
stepConsensus =
    (MinPQ.minView <$> use ssEvents) >>= \case
        Nothing -> return ()
        Just (nextEv, evs') -> do
            ssEvents .= evs'
            case nextEv of
                (PEvent t (TransactionEvent mktr)) -> do
                    nonce <- ssNextTransactionNonce <<%= (1+)
                    let (bi, nxt) = mktr nonce
                    ssEvents %= \e -> MinPQ.insert (PEvent (t+1) nxt) (foldr MinPQ.insert e [PEvent t (BakerEvent bid (ETransaction bi)) | bid <- allBakers])
                (PEvent t (BakerEvent i ev)) -> (liftIO $ putStrLn $ show i ++ "> " ++ show ev ) >> case ev of
                    EBake sl -> do
                        bakerIdentity <- (^. bsIdentity) . (Vec.! i) <$> use ssBakers
                        let doBake =
                                bakeForSlot bakerIdentity sl
                        mb <- runBaker t i doBake
                        forM_ (BS._bpBlock <$> mb) $ \case
                            GenesisBlock{} -> return ()
                            NormalBlock b -> broadcastEvent t (EBlock b)
                        ssEvents %= MinPQ.insert (PEvent (t+1) (BakerEvent i (EBake (sl+1))))
                    EBlock bb -> do
                        let pb = makePendingBlock bb (posixSecondsToUTCTime (fromIntegral t))
                        _ <- runBaker t i (storeBlock pb)
                        return ()
                    ETransaction tr -> do
                        _ <- runBaker t i (receiveTransaction tr)
                        return ()
                    EFinalization fm -> do
                        _ <- runBaker t i (finalizationReceiveMessage fm)
                        return ()
                    EFinalizationRecord fr -> do
                        _ <- runBaker t i (finalizationReceiveRecord False fr)
                        return ()
                    ETimer _ a -> runBaker t i a

main :: IO ()
main = do
        b 10000
    where
        loop 0 _ = return ()
        loop n s = do
            s' <- execStateT stepConsensus s
            loop (n-1) s'
        b steps = loop steps =<< initialState