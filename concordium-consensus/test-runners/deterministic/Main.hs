{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- |This module simulates running multiple copies of consensus together in a
-- deterministic fashion, without consideration for real time.  The goal is to
-- have a faster and more reproducible way of testing/profiling/benchmarking
-- performance-related issues.
--
-- Note that it is expected that you will edit this file depending on what
-- you wish to test.
module Main where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Trans.State.Strict (StateT (..), execStateT)
import qualified Data.PQueue.Min as MinPQ
import qualified Data.Sequence as Seq
import Data.Time.Clock
import Data.Time.Clock.POSIX
import qualified Data.Vector as Vec
import Data.Word
import Lens.Micro.Platform
import System.IO
import System.Mem
import System.Random

import Concordium.Afgjort.Finalize.Types
import Concordium.GlobalState
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Block
import qualified Concordium.GlobalState.BlockPointer as BS
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import qualified Concordium.GlobalState.TreeState as TS
import Concordium.Types
import Concordium.Types.AnonymityRevokers
import Concordium.Types.IdentityProviders
import Concordium.Types.Transactions

import Concordium.Afgjort.Finalize
import Concordium.Birk.Bake
import Concordium.Kontrol
import Concordium.Logger
import Concordium.Skov.MonadImplementations
import Concordium.Startup
import Concordium.TimeMonad
import Concordium.TimerMonad

import qualified Concordium.Crypto.DummyData as Dummy
import Concordium.GlobalState.DummyData (dummyKeyCollection)
import qualified Concordium.GlobalState.DummyData as Dummy
import qualified Concordium.Types.DummyData as Dummy

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Serialize as S
import System.Directory

-- |Protocol version
type PV = 'P5

type TreeConfig = DiskTreeDiskBlockConfig

-- |Construct the global state configuration.
-- Can be customised if changing the configuration.
makeGlobalStateConfig :: RuntimeParameters -> FilePath -> FilePath -> IO TreeConfig
makeGlobalStateConfig rt treeStateDir blockStateFile = return $ DTDBConfig rt treeStateDir blockStateFile

{-
type TreeConfig = PairGSConfig MemoryTreeMemoryBlockConfig DiskTreeDiskBlockConfig

-- |Construct the global state configuration.
-- Can be customised if changing the configuration.
makeGlobalStateConfig :: RuntimeParameters -> FilePath -> FilePath -> IO TreeConfig
makeGlobalStateConfig rp treeStateDir blockStateFile =
   return $ PairGSConfig (MTMBConfig rp, DTDBConfig rp treeStateDir blockStateFile)
-}

-- |A timer is represented as an integer identifier.
-- Timers are issued with increasing identifiers.
newtype DummyTimer = DummyTimer Integer
    deriving (Num, Eq, Ord)

-- |Configuration to use for bakers.
-- Can be customised for different global state configurations (disk/memory/paired)
-- or to enable/disable finalization buffering.
type BakerConfig = SkovConfig PV TreeConfig (ActiveFinalization DummyTimer) NoHandler

-- |The identity providers to use.
dummyIdentityProviders :: IdentityProviders
dummyIdentityProviders = emptyIdentityProviders

dummyArs :: AnonymityRevokers
dummyArs = emptyAnonymityRevokers

-- |Monad that provides a deterministic implementation of 'TimeMonad' -- i.e. that is
-- not dependent on real time.
newtype DeterministicTime m a = DeterministicTime {runDeterministic' :: ReaderT UTCTime m a}
    deriving (Functor, Applicative, Monad, MonadIO)

instance Monad m => TimeMonad (DeterministicTime m) where
    currentTime = DeterministicTime ask

-- |Run the 'DeterministicTime' action with the given time.
runDeterministic :: DeterministicTime m a -> UTCTime -> m a
runDeterministic (DeterministicTime a) t = runReaderT a t

-- |The base monad (with logging and time).
type LogBase = LoggerT (DeterministicTime IO)

-- |How 'SkovHandlers' should be instantiated for our setting.
type MyHandlers = SkovHandlers PV DummyTimer BakerConfig (StateT SimState LogBase)

-- |The monad for bakers to run in.
type BakerM = SkovT PV MyHandlers BakerConfig (StateT SimState LogBase)

-- |Events that trigger actions by bakers.
data Event
    = -- |Attempt to bake a block in the given slot; generates a new event for the next slot
      EBake !Slot
    | -- |Receive a block
      EBlock !BakedBlock
    | -- |Receive a transaction
      ETransaction !BlockItem
    | -- |Receive a finalization message
      EFinalization !FinalizationPseudoMessage
    | -- |Receive a finalization record
      EFinalizationRecord !FinalizationRecord
    | -- |Trigger a timer event
      ETimer !DummyTimer !(BakerM ())

instance Show Event where
    show (EBake sl) = "Bake in slot " ++ show sl
    show (EBlock _) = "Receive block"
    show (ETransaction _) = "Receive transaction"
    show (EFinalization _) = "Receive finalization message"
    show (EFinalizationRecord _) = "Receive finalization record"
    show (ETimer _ _) = "Timer event"

-- |Both baker-specific and generic events.
data GEvent
    = -- |An event for a particular baker
      BakerEvent !Int !Event
    | -- |Spawn the next transaction to send to bakers
      TransactionEvent [(Integer, BlockItem)]

-- |An event with a time at which it should occur.
-- The time is used for determining priority.
data PEvent = PEvent !Integer !GEvent

instance Eq PEvent where
    (PEvent i1 _) == (PEvent i2 _) = i1 == i2
instance Ord PEvent where
    compare (PEvent i1 _) (PEvent i2 _) = compare i1 i2

-- |The state of a particular baker.
data BakerState = BakerState
    { _bsIdentity :: !BakerIdentity,
      _bsInfo :: !FullBakerInfo,
      _bsContext :: !(SkovContext BakerConfig),
      _bsState :: !(SkovState BakerConfig)
    }

-- |Typeclass of a datastructure that collects events.
-- The instance of this structure determines how events
-- are ordered.
class Events e where
    -- |Add an event to the collection.
    addEvent :: PEvent -> e -> e

    -- |Filter the collection.
    filterEvents :: (PEvent -> Bool) -> e -> e

    -- |Make a collection from a list (in a default manner).
    makeEvents :: [PEvent] -> e

    -- |Extract the next event from the collection.
    nextEvent :: e -> Maybe (PEvent, e)

instance Events (MinPQ.MinQueue PEvent) where
    addEvent = MinPQ.insert
    filterEvents = MinPQ.filter
    makeEvents = MinPQ.fromList
    nextEvent = MinPQ.minView

-- |An instance of 'Events' that selects events randomly
-- (without regard for time).  The randomness is determined
-- by the 'StdGen', which is used to pick the next element
-- from the sequence.
data RandomisedEvents = RandomisedEvents
    { _reEvents :: !(Seq.Seq PEvent),
      _reGen :: !StdGen
    }

-- |State of the simulation.
data SimState = SimState
    { _ssBakers :: !(Vec.Vector BakerState),
      _ssEvents :: !(MinPQ.MinQueue PEvent),
      --  _ssEvents :: RandomisedEvents,
      _ssNextTimer :: !DummyTimer,
      _ssCurrentTime :: !UTCTime
    }

makeLenses ''RandomisedEvents
makeLenses ''BakerState
makeLenses ''SimState

-- |Pick an element from a sequence, returning the element
-- and the sequence with that element removed.
selectFromSeq :: (RandomGen g) => g -> Seq.Seq a -> (a, Seq.Seq a, g)
selectFromSeq g s =
    let (n, g') = randomR (0, length s - 1) g
    in  (Seq.index s n, Seq.deleteAt n s, g')

-- |Seed to use for randomness in 'RandomisedEvents'.
-- This can be varied to generate different runs (when 'RandomisedEvents'
-- is used.)
randomisedEventsSeed :: Int
randomisedEventsSeed = 0

instance Events RandomisedEvents where
    addEvent e = reEvents %~ (Seq.|> e)
    filterEvents f = reEvents %~ Seq.filter f
    makeEvents l = RandomisedEvents (Seq.fromList l) (mkStdGen randomisedEventsSeed)
    nextEvent RandomisedEvents{..} = case _reEvents of
        Seq.Empty -> Nothing
        _ ->
            let (v, s', g') = selectFromSeq _reGen _reEvents
            in  Just (v, RandomisedEvents s' g')

-- |Maximal baker ID.
maxBakerId :: (Integral a) => a
maxBakerId = 0 -- 9

-- |List of all baker IDs.
allBakers :: (Integral a) => [a]
allBakers = [0 .. maxBakerId]

transactions :: StdGen -> [(Integer, BlockItem)]
transactions gen = trs (1 :: Nonce) (randoms gen :: [Word8])
  where
    trs (Nonce n) (amnt : amnts) = (toInteger n `div` 100, Dummy.makeTransferTransaction (Dummy.mateuszKP, Dummy.mateuszAccount) Dummy.mateuszAccount (fromIntegral amnt) (Nonce n)) : trs (Nonce (n + 1)) amnts
    trs _ _ = error "Ran out of transaction data"

-- |Transactions from the extra accounts.
extraAccountTransactions :: Int -> [(Integer, BlockItem)]
extraAccountTransactions numAccts = trs 1
  where
    maxAcc = numAccts
    trs (Nonce n) =
        [ ( toInteger maxAcc * toInteger (n - 1) + toInteger i,
            Dummy.makeTransferTransaction
                (Dummy.alesKP, Dummy.accountAddressFrom i)
                Dummy.mateuszAccount
                1
                (Nonce n) -- \$ fromInteger $ toInteger maxAcc * toInteger (n - 1) + toInteger i)
          )
          | i <- [1 .. maxAcc]
        ]
            ++ trs (Nonce (n + 1))

-- |Genesis accounts. For convenience, these all use the same keys.
extraAccounts :: Int -> [GenesisAccount]
extraAccounts numAccts = [Dummy.createCustomAccount 1000000 Dummy.alesKP (Dummy.accountAddressFrom i) | i <- [1 .. numAccts]]

-- |Number of execution steps between blocks.
-- Note: this can be used to control the number of transactions per block, in preference to
-- having multiple transactions in a single execution step.
ticksPerSlot :: Num a => a
ticksPerSlot = 100

-- |The initial state of the simulation.
initialState :: Int -> IO SimState
initialState numAccts = do
    -- This timestamp is only used for naming the database files.
    now <- currentTimestamp
    -- Change the following line to write the genesis to a file, if desired.
    when False $
        LBS.writeFile ("data/genesis-" ++ show now ++ ".dat") $
            S.runPutLazy (putPVGenesisData (PVGenesisData genData))
    _ssBakers <- Vec.fromList <$> mapM (mkBakerState now) (zip [0 ..] bakers)
    return SimState{..}
  where
    chainParams =
        Dummy.dummyChainParameters
            { _cpElectionDifficulty = makeElectionDifficulty 50000,
              _cpExchangeRates = makeExchangeRates 1 1,
              _cpFoundationAccount = maxBakerId + 1
            }
    -- The genesis parameters could be changed.
    -- The slot duration is set to 'ticksPerSlot' seconds, since the deterministic time
    -- advances 1 second per tick and baking is set to occur once every 'ticksPerSlot' ticks.
    (genData, bakers, _) =
        makeGenesisData
            0 -- Start at time 0, to match time
            (maxBakerId + 1) -- Number of bakers
            (ticksPerSlot * 1000) -- Slot time is 100 seconds, for baking blocks every 100 ticks
            defaultFinalizationParameters
            Dummy.dummyCryptographicParameters
            dummyIdentityProviders
            dummyArs
            ([Dummy.createCustomAccount 1000000000000 Dummy.mateuszKP Dummy.mateuszAccount] ++ extraAccounts numAccts)
            (Energy maxBound)
            dummyKeyCollection
            chainParams
    mkBakerState :: Timestamp -> (BakerId, (BakerIdentity, FullBakerInfo)) -> IO BakerState
    mkBakerState now (bakerId, (_bsIdentity, _bsInfo)) = do
        createDirectoryIfMissing True "data"
        gsconfig <-
            makeGlobalStateConfig
                defaultRuntimeParameters{rpAccountsCacheSize = 5000}
                ("data/treestate-" ++ show now ++ "-" ++ show bakerId)
                ("data/blockstate-" ++ show now ++ "-" ++ show bakerId ++ ".dat")
        let
            finconfig = ActiveFinalization (FinalizationInstance (bakerSignKey _bsIdentity) (bakerElectionKey _bsIdentity) (bakerAggregationKey _bsIdentity))
            hconfig = NoHandler
            config = SkovConfig gsconfig finconfig hconfig
        (_bsContext, _bsState) <- runLoggerT (initialiseSkov genData config) (logFor (fromIntegral bakerId))
        return BakerState{..}
    _ssEvents = makeEvents $ (PEvent 0 (TransactionEvent (extraAccountTransactions numAccts))) : [PEvent 0 (BakerEvent i (EBake 0)) | i <- allBakers]
    _ssNextTimer = 0
    _ssCurrentTime = posixSecondsToUTCTime 0

-- |Log an event for a particular baker.
logFor :: (MonadIO m) => Int -> LogMethod m
logFor _ _ _ _ = return ()

-- logFor i src lvl msg = liftIO $ do
--     putStrLn $ "[" ++ show i ++ ":" ++ show src ++ ":" ++ show lvl ++ "] " ++ show msg
--     hFlush stdout

-- |Run a baker action in the state monad.
runBaker :: Integer -> Int -> BakerM a -> StateT SimState IO a
runBaker curTime bid a = do
    bakerState <- (Vec.! bid) <$> use ssBakers
    (r, bsState') <- runBase (runSkovT a handlers (_bsContext bakerState) (_bsState bakerState))
    ssBakers . ix bid . bsState .= bsState'
    return r
  where
    runBase z = do
        s <- get
        (r, s') <- liftIO $ runDeterministic (runLoggerT (runStateT z s) (logFor bid)) curTimeUTC
        put s'
        return r
    curTimeUTC = posixSecondsToUTCTime (fromIntegral curTime)
    handlers = SkovHandlers{..}
    shBroadcastFinalizationMessage = broadcastEvent curTime . EFinalization
    shOnTimeout timeout action = do
        tmr <- ssNextTimer <<%= (1 +)
        let t = case timeout of
                DelayFor delay -> curTime + round delay
                DelayUntil z -> curTime + max 1 (round (diffUTCTime z curTimeUTC))
        ssEvents %= addEvent (PEvent t (BakerEvent bid (ETimer tmr (void action))))
        return tmr
    shCancelTimer tmr = ssEvents %= filterEvents f
      where
        f (PEvent _ (BakerEvent _ (ETimer tmr' _))) = tmr' /= tmr
        f _ = True
    shPendingLive = return ()

-- |Send an event to all bakers.
broadcastEvent ::
    (MonadState SimState m) =>
    Integer ->
    Event ->
    m ()
broadcastEvent curTime ev = ssEvents %= \e -> foldr addEvent e [PEvent curTime (BakerEvent i ev) | i <- allBakers]

-- |Display an event for a baker.
displayBakerEvent :: (MonadIO m) => Int -> Event -> m ()
displayBakerEvent i ev = liftIO $ putStrLn $ show i ++ "> " ++ show ev

bpBlock :: TS.BlockPointerType BakerM -> Block PV
-- bpBlock (PairBlockData (l, _)) = BS._bpBlock l
bpBlock = BS._bpBlock

-- |Run a step of the consensus. This takes the next event and
-- executes that.
stepConsensus :: StateT SimState IO ()
stepConsensus =
    (nextEvent <$> use ssEvents) >>= \case
        Nothing -> return ()
        Just (nextEv, evs') -> do
            ssEvents .= evs'
            case nextEv of
                (PEvent _ (TransactionEvent trs)) -> case trs of
                    [] -> return ()
                    ((te, ev) : r) -> do
                        ssEvents %= \e -> foldr addEvent e [PEvent te (BakerEvent bid (ETransaction ev)) | bid <- allBakers]
                        case r of
                            [] -> return ()
                            ((t', _) : _) -> ssEvents %= addEvent (PEvent t' (TransactionEvent r))
                (PEvent t (BakerEvent i ev)) -> {- displayBakerEvent i ev >> -} case ev of
                    EBake sl -> do
                        bIdentity <- (^. bsIdentity) . (Vec.! i) <$> use ssBakers
                        let doBake =
                                bakeForSlot bIdentity sl
                        mb <- runBaker t i doBake
                        forM_ (bpBlock <$> mb) $ \case
                            GenesisBlock{} -> return ()
                            NormalBlock b -> broadcastEvent t (EBlock b)
                        ssEvents %= addEvent (PEvent (t + ticksPerSlot) (BakerEvent i (EBake (sl + 1))))
                    EBlock bb -> do
                        let pb = makePendingBlock bb (posixSecondsToUTCTime (fromIntegral t))
                        runBaker t i (receiveBlock pb) >>= \case
                            (recvRes, Nothing) -> error $ "Receive block failed with result " ++ show recvRes
                            (_, Just cont) -> do
                                _ <- runBaker t i (executeBlock cont)
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

-- |Main runner. Simply runs consensus for a certain number of steps.
main :: IO ()
main = do
    putStr "Number of accounts: "
    hFlush stdout
    numAccts <- readLn
    putStrLn "Initialising"
    b numAccts (1000000 :: Int)
  where
    loop 0 _ = return ()
    loop n s = do
        s' <- execStateT stepConsensus s
        loop (n - 1) s'
    b numAccts steps = do
        !s0 <- initialState numAccts
        putStrLn "Initialisation complete; press Enter to start"
        performGC
        _ <- getLine
        putStrLn "Starting"
        loop steps s0
