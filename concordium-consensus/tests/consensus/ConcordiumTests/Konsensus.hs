{-# LANGUAGE RecordWildCards, GeneralizedNewtypeDeriving #-}
module ConcordiumTests.Konsensus where

import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.Vector as Vec
import qualified Data.HashMap.Strict as HM
import Control.Monad
import Control.Monad.IO.Class
import Data.Functor.Identity
import qualified Data.ByteString as BS

import Concordium.Payload.Transaction
import Concordium.Types
import Concordium.MonadImplementation
import Concordium.Afgjort.Finalize
import Concordium.Logger

import Test.QuickCheck
import Test.Hspec

invariantSkovData :: SkovData -> Either String ()
invariantSkovData SkovData{..} = do
        when (Seq.null _skovFinalizationList) $ Left "Finalization list is empty"
        (finMap, lastFin, _) <- foldM checkFin (HM.empty, _skovGenesisBlockPointer, 0) _skovFinalizationList
        (liveFinMap, _) <- foldM checkLive (finMap, [lastFin]) _skovBranches
        unless (HM.filter notDead _skovBlockTable == liveFinMap) $ Left "non-dead blocks do not match finalized and branch blocks"
        -- TODO: Pending blocks
        -- TODO: Finalization pool
        -- TODO: Transactions
        return ()
    where
        checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"
        checkFin (finMap, lastFin, i) (fr, bp) = do
            checkBinary (==) (fromIntegral (finalizationIndex fr)) i "==" "record finalization index" "index in sequence"
            if i == 0 then
                checkBinary (==) bp _skovGenesisBlockPointer "==" "first finalized block" "genesis block"
            else do
                unless (verifyFinalProof finSes finCom fr) $ Left $ "Could not verify finalization record at index " ++ show i
            let overAncestors a m
                    | a == lastFin = return m
                    | a == _skovGenesisBlockPointer = Left $ "Finalized block" ++ show bp ++ "does not descend from previous finalized block " ++ show lastFin
                    | otherwise = overAncestors (bpParent a) (HM.insert (bpHash a) (BlockFinalized a fr) m)
            finMap' <- overAncestors (bpParent bp) (HM.insert (bpHash bp) (BlockFinalized bp fr) finMap)
            return (finMap', bp, i+1)
        checkLive (liveMap, parents) l = do
            forM_ l $ \b -> do
                unless (bpParent b `elem` parents) $ Left $ "Block in branches with invalid parent: " ++ show b
                checkBinary (==) (bpHeight b) (bpHeight (bpParent b) + 1) "==" "block height" "1 + parent height"
            let liveMap' = foldr (\b -> HM.insert (bpHash b) (BlockAlive b)) liveMap l
            return (liveMap', l)
        finSes = FinalizationSessionId (bpHash _skovGenesisBlockPointer) 0
        finCom = makeFinalizationCommittee (genesisFinalizationParameters _skovGenesisData)
        notDead BlockDead = False
        notDead _ = True

newtype DummyM a = DummyM (Identity a)
    deriving (Monad, Functor, Applicative)

instance MonadIO DummyM where
    liftIO = return (error "dummy IO")

instance LoggerMonad DummyM where
    logEvent _ _ _ = return ()

data Event
    = EBake Slot
    | EBlock Block
    | ETransaction Transaction
    | EFinalization BS.ByteString
    | EFinalizationRecord FinalizationRecord

type EventPool = Seq (Int, Event)

-- |Pick an element from a seqeunce, returning the element
-- and the sequence with that element removed.
selectFromSeq :: Seq a -> Gen (a, Seq a)
selectFromSeq s = select <$> choose (0, length s - 1)
    where
        select n = (Seq.index s n, Seq.deleteAt n s)

type States = Vec.Vector (FinalizationInstance, SkovFinalizationState)

runKonsensusTest :: States -> EventPool -> Gen Property
runKonsensusTest states events
    | null events = return $ property True
    | otherwise = do
        ((rcpt, ev), events') <- selectFromSeq events
        let (fi, fs) = states Vec.! rcpt
        case ev of
            EBake sl -> do
                undefined
            _ -> undefined

tests :: Spec
tests = do
    return ()