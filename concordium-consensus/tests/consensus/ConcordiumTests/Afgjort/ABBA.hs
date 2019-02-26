{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables #-}
module ConcordiumTests.Afgjort.ABBA where

import qualified Data.Set as Set
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import qualified Data.ByteString as BS
import qualified Data.Vector as Vec
import Data.Monoid
import Control.Monad
import Lens.Micro.Platform
import System.Random
import qualified Data.Serialize as Ser

import qualified Concordium.Crypto.VRF as VRF
import Concordium.Afgjort.ABBA
import Concordium.Afgjort.Lottery

import Test.QuickCheck
import Test.Hspec

import Debug.Trace

instance Arbitrary VRF.KeyPair where
    arbitrary = fst . VRF.randomKeyPair . mkStdGen <$> arbitrary

invariantABBAState :: (Ord party) => ABBAInstance party -> ABBAState party -> Either String ()
invariantABBAState (ABBAInstance baid tw cw pw pk me sk) ABBAState{..} = do
        unless (_currentGrade <= 2) $ Left $ "Invalid grade" ++ show _currentGrade
        checkBinary (==) _topWeAreDoneWeight (sum $ fmap pw $ Set.toList $ _topWeAreDone) "==" "weight of WeAreDone for Top" "calculated value"
        checkBinary (==) _botWeAreDoneWeight (sum $ fmap pw $ Set.toList $ _botWeAreDone) "==" "weight of WeAreDone for Bottom" "calculated value"
    where
        checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"

data ABBAInput party
    = JustifyABBAChoice Choice
    | ReceiveABBAMessage party (ABBAMessage party)
    | BeginABBA Choice
    deriving (Eq,Show)

makeInput :: Ord party => ABBAInput party -> ABBA party ()
makeInput (JustifyABBAChoice c) = justifyABBAChoice c
makeInput (ReceiveABBAMessage p m) = receiveABBAMessage p m
makeInput (BeginABBA c) = beginABBA c

-- |Pick an element from a seqeunce, returning the element
-- and the sequence with that element removed.
selectFromSeq :: Seq a -> Gen (a, Seq a)
selectFromSeq s = select <$> choose (0, length s - 1)
    where
        select n = (Seq.index s n, Seq.deleteAt n s)

runABBATest :: BS.ByteString -> Int -> Int -> Vec.Vector VRF.KeyPair -> Seq.Seq (Int, ABBAInput Int) -> Gen Property
runABBATest baid nparties allparties vrfkeys = go iStates iResults
    where
        iResults = Vec.replicate nparties (First Nothing)
        iStates = Vec.replicate nparties (initialABBAState)
        checkSucceed = (allparties - nparties) * 3 < allparties
        corruptWeight = (allparties - 1) `div` 3
        inst i = ABBAInstance baid allparties corruptWeight (const 1) (VRF.publicKey . (vrfkeys Vec.!)) i (vrfkeys Vec.! i)
        go sts ress msgs
            | null msgs = return $ counterexample ("Outcome: " ++ show ress) $ not checkSucceed || all (checkRes (ress Vec.! 0)) ress
            | otherwise = do
                ((rcpt, inp), msgs') <- selectFromSeq msgs
                let s = (sts Vec.! rcpt)
                let (_, s', out) = runABBA (makeInput inp) (inst rcpt) s
                let lbl = if _currentPhase s /= _currentPhase s' then label ("reached phase " ++ show (_currentPhase s')) else id
                lbl <$> case invariantABBAState (inst rcpt) s' of
                    Left err -> return $ counterexample ("Invariant failed: " ++ err ++ "\n" ++ show s') False
                    Right _ -> do
                        let sts' = sts & ix rcpt .~ s'
                        let (msgs'', c') = mconcat $ fromOut rcpt <$> out
                        go sts' (ress & ix rcpt %~ (<> c')) (msgs' <> msgs'')
        checkRes _ (First Nothing) = False
        checkRes g r = r == g
        fromOut src (SendABBAMessage msg) = (Seq.fromList [(i,ReceiveABBAMessage src msg)|i <- parties], mempty)
        fromOut _ (ABBAComplete c) = (mempty, First (Just c))
        parties = [0..nparties-1]

makeKeys :: Int -> Gen (Vec.Vector VRF.KeyPair)
makeKeys = fmap Vec.fromList . vector

superCorruptKeys :: Int -> Int -> Int -> Gen (Vec.Vector VRF.KeyPair)
superCorruptKeys good bad ugly = makeKeys (good + bad) `suchThat` areSuperCorrupt ugly
    where
        areSuperCorrupt phase keys
            | phase < 0 = True
            | otherwise = maximum [valAtPhase phase (keys Vec.! k) | k <- [0..good-1]] < maximum [valAtPhase phase (keys Vec.! k) | k <- [good..good+bad-1]] 
                        && areSuperCorrupt (phase - 1) keys
        valAtPhase phase k = ticketValue (proofToTicket (makeTicketProof (lotteryId phase) k) 1 (good + bad))
        baid = "test" :: BS.ByteString
        lotteryId phase = Ser.runPut $ Ser.put baid >> Ser.put phase
makeBegins :: Int -> Gen (Seq.Seq (Int, ABBAInput Int))
makeBegins = fmap toBegins . vector
    where
        toBegins = Seq.fromList . zip [0..] . fmap BeginABBA

justifyAll :: Int -> Seq.Seq (Int, ABBAInput Int)
justifyAll n = Seq.fromList [(i, JustifyABBAChoice c) | i <- [0..n-1], c <- [False, True]]

allHonest :: Int -> Property
allHonest n = property $ do
    begins <- makeBegins n
    keys <- makeKeys n
    runABBATest "test" n n keys (justifyAll n <> begins)

multiWithInactive :: Int -> Int -> Property
multiWithInactive active inactive = property $ do
    begins <- makeBegins active
    keys <- makeKeys (active + inactive)
    runABBATest "test" active (active + inactive) keys (justifyAll active <> begins)

multiWithCorrupt :: Int -> Int -> Property
multiWithCorrupt active corrupt = property $ do
        keys <- makeKeys (active + corrupt)
        return $ multiWithCorruptKeys keys active corrupt

multiWithCorruptKeys :: Vec.Vector VRF.KeyPair -> Int -> Int -> Property
multiWithCorruptKeys keys active corrupt = property $ do
    begins <- makeBegins active
    let baid :: BS.ByteString = "test"
    let lotteryId phase = Ser.runPut $ Ser.put baid >> Ser.put phase
    let corruptMsgs = Seq.fromList $
            [(a, ReceiveABBAMessage src $ Justified phase c (makeTicketProof (lotteryId phase) (keys Vec.! src))) | a <- [0..active-1], src <- [active..active+corrupt-1], phase<-[0..5], c <- [False, True]] ++
            [(a, ReceiveABBAMessage src $ CSSSeen phase p c) | a <- [0..active-1], src <- [active..active+corrupt-1], phase<-[0..5], p <-[0..active+corrupt-1], c <- [False, True]] ++
            [(a, ReceiveABBAMessage src $ WeAreDone c ) | a <- [0..active-1], src <- [active..active+corrupt-1], c <- [False, True]]
    runABBATest baid active (active + corrupt) keys (justifyAll active <> begins <> corruptMsgs)

{-
-- This test is not valid, because it involves messages that are only selectively delivered.
multiWithCorruptKeysEvil :: Vec.Vector VRF.KeyPair -> Int -> Int -> Property
multiWithCorruptKeysEvil keys active corrupt = property $ do
    let begins = Seq.fromList $ [(p, BeginABBA (p `mod` 2 == 0)) | p <- [0..active-1]]
    let baid :: BS.ByteString = "test"
    let lotteryId phase = Ser.runPut $ Ser.put baid >> Ser.put phase
    let corruptMsgs = Seq.fromList $
            [(a, ReceiveABBAMessage src $ Justified phase (a `mod` 2 == 0) (makeTicketProof (lotteryId phase) (keys Vec.! src))) | a <- [0..active-1], src <- [active..active+corrupt-1], phase<-[0..5]] ++
            [(a, ReceiveABBAMessage src $ CSSSeen phase p (a `mod` 2 == 0)) | a <- [0..active-1], src <- [active..active+corrupt-1], phase<-[0..5], p <-[0..active+corrupt-1]] ++
            [(a, ReceiveABBAMessage src $ WeAreDone (a `mod` 2 == 0)) | a <- [0..active-1], src <- [active..active+corrupt-1]]
    runABBATest baid active (active + corrupt) keys (justifyAll active <> begins <> corruptMsgs)
-}


tests :: Spec
tests = describe "Concordium.Afgjort.ABBA" $ do
    beforeAll (generate (superCorruptKeys 3 1 6)) $ do
        it "3 parties + 1 super corrupt" $ \k -> (withMaxSuccess 5000 $ multiWithCorruptKeys k 3 1)
    beforeAll (generate (superCorruptKeys 5 2 6)) $ do    
        it "5 parties + 2 super corrupt" $ \k -> (withMaxSuccess 5000 $ multiWithCorruptKeys k 5 2)
    it "3 parties + 1 corrupt" $ withMaxSuccess 500 $ multiWithCorrupt 3 1
    it "5 parties + 2 corrupt" $ withMaxSuccess 500 $ multiWithCorrupt 5 2
    it "Two parties" $ withMaxSuccess 10000 $ allHonest 2
    it "Three parties" $ withMaxSuccess 10000 $ allHonest 3
    it "5 parties" $ withMaxSuccess 1000 $ allHonest 5
    it "25 parties" $ withMaxSuccess 100 $ allHonest 25
    it "3 parties + 1 inactive" $ withMaxSuccess 1000 $ multiWithInactive 3 1
    it "5 parties + 2 inactive" $ withMaxSuccess 300 $ multiWithInactive 5 2
    it "7 parties + 2 inactive" $ withMaxSuccess 100 $ multiWithInactive 7 2
    it "17 parties + 8 inactive" $ withMaxSuccess 100 $ multiWithInactive 17 8