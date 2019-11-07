{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving, TupleSections, OverloadedStrings, InstanceSigs, BangPatterns #-}
module Main where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Vector as Vec
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import Lens.Micro.Platform
import Data.Bits
import Data.Monoid
import Data.Time.Clock.POSIX
import qualified Data.PQueue.Prio.Min as MPQ
import Criterion.Main
import System.Random
import Data.Serialize
import Data.Word

import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.BlockState(BlockPointerData(..))
import qualified Concordium.GlobalState.TreeState as TreeState
import Concordium.GlobalState.Basic.TreeState
import Concordium.Types.Transactions
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block
import Concordium.GlobalState.Bakers

import qualified Data.FixedByteString as FBS
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Scheduler.Utils.Init.Example as Example
import Concordium.Skov
import Concordium.Afgjort.Freeze
import Concordium.Afgjort.WMVBA
import Concordium.Afgjort.Finalize
import Concordium.Logger
import Concordium.Birk.Bake
import Concordium.TimeMonad
import Concordium.Startup

import Debug.Trace

data DummyM a = DummyM {runDummy :: a}

instance Functor DummyM where
    fmap f (DummyM a) = DummyM (f a)

instance Applicative DummyM where
    pure = DummyM
    (DummyM f) <*> (DummyM v) = DummyM (f v)

instance Monad DummyM where
    -- Bind is slightly stricter than identity monad
    (DummyM m) >>= k = k m

instance TimeMonad DummyM where
    currentTime = return (posixSecondsToUTCTime 1)

instance LoggerMonad DummyM where
    logEvent src LLError msg = error $ show src ++ ": " ++ msg
    logEvent _ _ _ = return () -- trace (show src ++ ": " ++ msg) $ return ()

data Event
    = EBake !Slot
    | EBlock !BakedBlock
    | ETransaction !Transaction
    | EFinalization !FinalizationMessage
    | EFinalizationRecord !FinalizationRecord

instance Show Event where
    show (EBake sl) = "bake for " ++ show sl
    show (EBlock b) = "block: " ++ show (getHash b :: BlockHash)
    show (ETransaction tr) = "transaction: " ++ show tr
    show (EFinalization _) = "finalization message"
    show (EFinalizationRecord fr) = "finalize: " ++ show (finalizationBlockPointer fr)

type EventPool = Seq (Int, Event)

{-
-- |Pick an element from a seqeunce, returning the element
-- and the sequence with that element removed.
selectFromSeq :: Seq a -> Gen (a, Seq a)
selectFromSeq s = select <$> choose (0, length s - 1)
    where
        select n = (Seq.index s n, Seq.deleteAt n s)
-}

data BakerState = BakerState !BakerIdentity !FinalizationInstance !SkovActiveState

type States = Vec.Vector BakerState

runKonsensusTest :: Int -> States -> EventPool -> IO States
runKonsensusTest !steps !states !events
        | steps <= 0 = return states
        | null events = return states
        | otherwise = do
            let
                (rcpt, ev) Seq.:<| events' = events
                (BakerState bkr fi fs) = states Vec.! rcpt
                btargets = [x | x <- [0..length states - 1], x /= rcpt]
            (fs', events'') <- case ev of
                    EBake sl -> do
                        (mb, fs', Endo evs) <- runLoggerT (runSkovActiveM (bakeForSlot bkr sl) fi fs) (\_ _ _ -> return ())
                        let
                            blockEvents = case mb of
                                            Nothing -> Seq.empty
                                            Just b -> Seq.fromList [(r, EBlock b) | r <- btargets]
                            events'' = blockEvents <> handleMessages btargets (evs []) Seq.|> (rcpt, EBake (sl + 1))
                        return (fs', events'')
                    EBlock block -> runAndHandle (storeBlock block) fi fs btargets
                    ETransaction tr -> runAndHandle (receiveTransaction tr) fi fs btargets
                    EFinalization fmsg -> runAndHandle (receiveFinalizationMessage fmsg) fi fs btargets
                    EFinalizationRecord frec -> runAndHandle (finalizeBlock frec) fi fs btargets
            let
                states' = states & ix rcpt .~ BakerState bkr fi fs'
            runKonsensusTest (steps - 1) states' (events' <> events'')
    where
        handleMessages :: [Int] -> [SkovFinalizationEvent] -> EventPool
        handleMessages _ [] = Seq.empty
        handleMessages targets (SkovFinalization (BroadcastFinalizationMessage fmsg) : r) = Seq.fromList [(rcpt, EFinalization fmsg) | rcpt <- targets] <> handleMessages targets r
        handleMessages targets (SkovFinalization (BroadcastFinalizationRecord frec) : r) = Seq.fromList [(rcpt, EFinalizationRecord frec) | rcpt <- targets] <> handleMessages targets r
        handleMessages targets (_ : r) = handleMessages targets r
        runAndHandle a fi fs btargets = do
            (_, fs', Endo evs) <- runLoggerT (runSkovActiveM a fi fs) (\_ _ _ -> return ())
            return (fs', handleMessages btargets (evs []))

{-
runKonsensusTest' :: Int -> States -> EventPool -> States
runKonsensusTest' steps states events
        | steps <= 0 = states
        | null events = states
        | otherwise =
            let
                (rcpt, ev) Seq.:<| events' = events
                (bkr, fi, fs) = states Vec.! rcpt
                btargets = [x | x <- [0..length states - 1], x /= rcpt]
                (fs', events'') = case ev of
                    EBake sl ->
                        let (mb, fs', Endo evs) = runDummy (runFSM' (bakeForSlot bkr sl) fi fs)
                            blockEvents = case mb of
                                            Nothing -> Seq.empty
                                            Just b -> trace (show b) $ Seq.fromList [(r, EBlock b) | r <- btargets]
                            events'' = blockEvents <> handleMessages btargets (evs []) Seq.|> (rcpt, EBake (sl + 1))
                        in (fs', events'')
                    EBlock block -> runAndHandle (storeBlock block) fi fs btargets
                    ETransaction tr -> runAndHandle (receiveTransaction tr) fi fs btargets
                    EFinalization fmsg -> runAndHandle (receiveFinalizationMessage fmsg) fi fs btargets
                    EFinalizationRecord frec -> runAndHandle (finalizeBlock frec) fi fs btargets
                states' = states & ix rcpt . _3 .~ fs'
            in runKonsensusTest' (steps - 1) states' (events' <> events'')
    where
        handleMessages :: [Int] -> [SkovFinalizationEvent] -> EventPool
        handleMessages _ [] = Seq.empty
        handleMessages targets (SkovFinalization (BroadcastFinalizationMessage fmsg) : r) = Seq.fromList [(rcpt, EFinalization fmsg) | rcpt <- targets] <> handleMessages targets r
        handleMessages targets (SkovFinalization (BroadcastFinalizationRecord frec) : r) = Seq.fromList [(rcpt, EFinalizationRecord frec) | rcpt <- targets] <> handleMessages targets r
        handleMessages targets (_ : r) = handleMessages targets r
        runAndHandle a fi fs btargets =
            let (_, fs', Endo evs) = runDummy (runFSM' a fi fs)
            in (fs', handleMessages btargets (evs []))
-}

nAccounts :: Int
nAccounts = 2

{-
genTransactions :: Int -> Gen [Transaction]
genTransactions n = mapM gent (take n [minNonce..])
    where
        gent nnce = do
            f <- arbitrary
            g <- arbitrary
            return $ Example.makeTransaction f (ContractAddress (fromIntegral $ g `mod` nAccounts) 0) nnce
-}

transactions :: Int -> [Transaction]
transactions n = [Example.makeTransaction True (ContractAddress (fromIntegral $ i `mod` fromIntegral nAccounts) 0) (Nonce i) | i <- [(1 :: Word64) ..fromIntegral n] ]

initialEvents :: States -> EventPool
initialEvents states = Seq.fromList [(x, EBake 1) | x <- [0..length states -1]]

{-
-- |Make a baker in a deterministic way.
makeBaker :: BakerId -> LotteryPower -> (BakerInfo, BakerIdentity)
makeBaker bid lot = (BakerInfo epk spk lot, BakerIdentity bid sk spk ek epk)
    where
        gen0 = mkStdGen (fromIntegral bid)
        (ek, gen1) = VRF.randomKeyPair gen0
        (sk, _) = Sig.randomKeyPair gen1
        epk = VRF.publicKey ek
        spk = Sig.verifyKey sk
-}

initialiseStates :: Int -> States
initialiseStates n = Vec.fromList [BakerState bid fininst (initialSkovActiveState fininst gen genState) | (bid, _) <- bis, let fininst = FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid)] 
    where
        (gen, bis) = makeGenesisData 0 (fromIntegral n) 1 0.5 0 dummyCryptographicParameters []
        genState = Example.initialState
                (genesisBirkParameters gen)
                (genesisCryptographicParameters gen)
                (genesisBakerAccounts gen)
                (genesisIdentityProviders gen)
                2

summariseStates :: States -> String
summariseStates s = show ((\(BakerState _ _ z) -> (z ^. skov, z ^. finState)) <$> Vec.toList s)-- ++ show ((s Vec.! 1 ) ^. _3 . 

{-
testBlock :: Block
testBlock = NormalBlock $ BakedBlock {
            bbSlot = 1,
            bbFields = BlockFields {
                blockPointer = Hash.hash "block pointer",
                blockBaker = 0,
                blockProof = VRF.prove vrfk "Block Proof",
                blockNonce = let p = VRF.prove vrfk "Block Nonce" in (VRF.proofToHash p, p),
                blockLastFinalized = Hash.hash "Last finalized"
            },
            bbTransactions = BlockTransactions $ [], -- transactions 1000,
            bbSignature = Sig.sign sigk "block signature"
        }
    where
        gen0 = mkStdGen 0
        (vrfk, gen1) = VRF.randomKeyPair gen0
        (sigk, _) = Sig.randomKeyPair gen1


hashToByteString (Hash.Hash h) = FBS.toByteString h

blockHashBenchStrict :: Benchmark
blockHashBenchStrict = bench "hash block strict" $ nf (hashToByteString . Hash.hash . runPut . put) testBlock

blockHashBenchLazy :: Benchmark
blockHashBenchLazy = bench "hash block lazy" $ nf (hashToByteString . Hash.hashLazy . runPutLazy . put) testBlock
-}

{-
main :: IO ()
main = defaultMain [blockHashBenchLazy, blockHashBenchStrict]
-}

{-
main :: IO ()
main = defaultMain [
        bench "run 5000 steps lazy state" $ nf (summariseStates . runKonsensusTest 5000 istates) events,
        bench "run 5000 steps strict state" $ nf (summariseStates . runKonsensusTest' 5000 istates) events
        ]
    where
        istates = initialiseStates 2
        events = initialEvents istates
-}

main :: IO ()
main = do
        ostates <- runKonsensusTest 100000 istates events
        putStrLn $ summariseStates ostates
    where
        istates = initialiseStates 20
        events = initialEvents istates

{-
main :: IO ()
main = do
        -- kp <- Sig.newKeyPair
        let dsig = Hash.hash "Dummy"
        print $ length $ filter (==dsig) [Hash.hash (CBS.pack (show n)) | n <- [1..20000000]]
-}

{-
main :: IO ()
main = do
        print signedBlock
        print $ runPut $ blockBody (signedBlock)
    where
        --signedBlock = signBlock sigk 1 (Hash.hash "parent") 0 bpf bnon (Hash.hash "lastFin") []
        NormalBlock signedBlock = testBlock
        gen0 = mkStdGen 0
        (vrfk, gen1) = VRF.randomKeyPair gen0
        (sigk, _) = Sig.randomKeyPair gen1
        bpf = VRF.prove vrfk "Block Proof"
        bnon = let p = VRF.prove vrfk "Block Nonce" in (VRF.proofToHash p, p)
-}