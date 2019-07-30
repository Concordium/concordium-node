{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables, RankNTypes, BangPatterns #-}
module ConcordiumTests.Afgjort.ABBA where

import System.IO.Unsafe
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
import Data.List
import Control.Monad.Trans

import qualified Concordium.Crypto.VRF as VRF
import Concordium.Afgjort.Types
import Concordium.Afgjort.ABBA
import Concordium.Afgjort.Lottery
import Concordium.Afgjort.CSS.NominationSet
import qualified Concordium.Afgjort.CSS.BitSet as BitSet

import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec

invariantABBAState :: ABBAInstance -> ABBAState -> Either String ()
invariantABBAState (ABBAInstance _ tw cw pw _ _ _ _) ABBAState{..} = do
        unless (_currentGrade <= 2) $ Left $ "Invalid grade" ++ show _currentGrade
        checkBinary (==) _topWeAreDoneWeight (sum $ fmap pw $ Set.toList $ _topWeAreDone) "==" "weight of WeAreDone for Top" "calculated value"
        checkBinary (<=) _topWeAreDoneWeight tw "<=" "weight of WeAreDone for Top" "total weight"
        checkBinary (==) _botWeAreDoneWeight (sum $ fmap pw $ Set.toList $ _botWeAreDone) "==" "weight of WeAreDone for Bottom" "calculated value"
        checkBinary (<=) _botWeAreDoneWeight tw "<=" "weight of WeAreDone for Bottom" "total weight"
        checkBinary (==) _completed (_topWeAreDoneWeight >= tw - cw || _botWeAreDoneWeight >= tw - cw) "iff" "completed" "top/bottom WeAreDone exceeds (n - t)"
    where
        checkBinary bop x y sbop sx sy = unless (bop x y) $ Left $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"

data ABBAInput
    = JustifyABBAChoice Choice
    | ReceiveABBAMessage Party ABBAMessage
    | BeginABBA Choice
    deriving (Eq,Show)

makeInput :: ABBAInput -> ABBA ()
makeInput (JustifyABBAChoice c) = justifyABBAChoice c
makeInput (ReceiveABBAMessage p m) = receiveABBAMessage p m
makeInput (BeginABBA c) = beginABBA c

-- |Pick an element from a sequence, returning the element
-- and the sequence with that element removed.
selectFromSeq :: Seq a -> Gen (a, Seq a)
selectFromSeq s = select <$> choose (0, length s - 1)
    where
        select n = (Seq.index s n, Seq.deleteAt n s)

selectFromSeq' :: (RandomGen g) => g -> Seq a -> (a, Seq a, g)
selectFromSeq' g s =
    let (n , g') = randomR (0, length s - 1) g in
    (Seq.index s n, Seq.deleteAt n s, g')

atParty :: Party -> Traversal' (Vec.Vector a) a
atParty = ix . fromIntegral

runABBATestRG :: RandomGen g => g -> BS.ByteString -> Int -> Int -> Vec.Vector VRF.KeyPair -> Seq.Seq (Party, ABBAInput) -> IO Property
runABBATestRG g0 baid nparties allparties vrfkeys = go g0 iStates iResults
    where
        iResults = Vec.replicate nparties (First Nothing)
        iStates = Vec.replicate nparties (initialABBAState)
        checkSucceed = (allparties - nparties) * 3 < allparties
        corruptWeight = (allparties - 1) `div` 3
        inst i = ABBAInstance baid allparties corruptWeight (const 1) (fromIntegral nparties) (VRF.publicKey . (vrfkeys Vec.!) . fromIntegral) i (vrfkeys Vec.! fromIntegral i)
        go g sts ress msgs
            | null msgs = return $ counterexample ("Outcome: " ++ show ress) $ not checkSucceed || all (checkRes (ress Vec.! 0)) ress
            | otherwise = do
                let ((rcpt, inp), msgs', g') = selectFromSeq' g msgs
                let s = (sts Vec.! fromIntegral rcpt)
                (_, s', out) <- runABBA (makeInput inp) (inst rcpt) s
                let lbl = if _currentPhase s /= _currentPhase s' then label ("reached phase " ++ show (_currentPhase s')) else id
                lbl <$> case invariantABBAState (inst rcpt) s' of
                    Left err -> return $ counterexample ("Invariant failed: " ++ err ++ "\n" ++ show s') False
                    Right _ -> do
                        let sts' = sts & atParty rcpt .~ s'
                        let (msgs'', c') = mconcat $ fromOut rcpt <$> out
                        go g' sts' (ress & atParty rcpt %~ (<> c')) (msgs' <> msgs'')
        checkRes _ (First Nothing) = False
        checkRes g r = r == g
        fromOut src (SendABBAMessage msg) = (Seq.fromList [(i,ReceiveABBAMessage src msg)|i <- parties], mempty)
        fromOut _ (ABBAComplete c) = (mempty, First (Just c))
        parties = [0..fromIntegral nparties-1]


{-
runABBATest :: BS.ByteString -> Int -> Int -> Vec.Vector VRF.KeyPair -> Seq.Seq (Party, ABBAInput) -> PropertyM IO Property
runABBATest baid nparties allparties vrfkeys = go iStates iResults
    where
        iResults = Vec.replicate nparties (First Nothing)
        iStates = Vec.replicate nparties (initialABBAState)
        checkSucceed = (allparties - nparties) * 3 < allparties
        corruptWeight = (allparties - 1) `div` 3
        inst i = ABBAInstance baid allparties corruptWeight (const 1) (fromIntegral nparties) (VRF.publicKey . (vrfkeys Vec.!) . fromIntegral) i (vrfkeys Vec.! fromIntegral i)
        go sts ress msgs
            | null msgs = return $ counterexample ("Outcome: " ++ show ress) $ not checkSucceed || all (checkRes (ress Vec.! 0)) ress
            | otherwise = do
                ((rcpt, inp), msgs') <- pick (selectFromSeq msgs)
                let s = (sts Vec.! fromIntegral rcpt)
                (_, s', out) <- lift $ runABBA (makeInput inp) (inst rcpt) s
                let lbl = if _currentPhase s /= _currentPhase s' then label ("reached phase " ++ show (_currentPhase s')) else id
                lbl <$> case invariantABBAState (inst rcpt) s' of
                    Left err -> return $ counterexample ("Invariant failed: " ++ err ++ "\n" ++ show s') False
                    Right _ -> do
                        let sts' = sts & atParty rcpt .~ s'
                        let (msgs'', c') = mconcat $ fromOut rcpt <$> out
                        go sts' (ress & atParty rcpt %~ (<> c')) (msgs' <> msgs'')
        checkRes _ (First Nothing) = False
        checkRes g r = r == g
        fromOut src (SendABBAMessage msg) = (Seq.fromList [(i,ReceiveABBAMessage src msg)|i <- parties], mempty)
        fromOut _ (ABBAComplete c) = (mempty, First (Just c))
        parties = [0..fromIntegral nparties-1]
-}

runABBATest2 :: RandomGen g => g -> BS.ByteString -> Int -> Int -> Vec.Vector VRF.KeyPair -> Seq.Seq (Party, ABBAInput) -> Seq.Seq (Party, ABBAInput) -> IO Property
runABBATest2 g0 baid nparties allparties vrfkeys = go g0 iStates iResults
    where
        iResults = Vec.replicate nparties (First Nothing)
        iStates = Vec.replicate nparties (initialABBAState)
        checkSucceed = (allparties - nparties) * 3 < allparties
        corruptWeight = (allparties - 1) `div` 3
        inst i = ABBAInstance baid allparties corruptWeight (const 1) (fromIntegral nparties) (VRF.publicKey . (vrfkeys Vec.!) . fromIntegral) i (vrfkeys Vec.! fromIntegral i)
        go g sts ress msgs lowmsgs
            | null msgs = if null lowmsgs then
                            return $ counterexample ("Outcome: " ++ show ress) $ not checkSucceed || all (checkRes (ress Vec.! 0)) ress
                          else do
                            let (msg, lowmsgs', g') = selectFromSeq' g lowmsgs
                            go g' sts ress (Seq.singleton msg) lowmsgs'
            | otherwise = do
                let ((rcpt, inp), msgs', g') = selectFromSeq' g msgs
                let s = (sts Vec.! fromIntegral rcpt)
                (_, s', out) <- runABBA (makeInput inp) (inst rcpt) s
                let lbl = if _currentPhase s /= _currentPhase s' then label ("reached phase " ++ show (_currentPhase s')) else id
                lbl <$> case invariantABBAState (inst rcpt) s' of
                    Left err -> return $ counterexample ("Invariant failed: " ++ err ++ "\n" ++ show s') False
                    Right _ -> do
                        let sts' = sts & atParty rcpt .~ s'
                        let (msgs'', c') = mconcat $ fromOut rcpt <$> out
                        go g' sts' (ress & atParty rcpt %~ (<> c')) (msgs' <> msgs'') lowmsgs
        checkRes _ (First Nothing) = False
        checkRes g r = r == g
        fromOut src (SendABBAMessage msg) = (Seq.fromList [(i,ReceiveABBAMessage src msg)|i <- parties], mempty)
        fromOut _ (ABBAComplete c) = (mempty, First (Just c))
        parties = [0..fromIntegral nparties-1]

makeKeys :: Int -> Gen (Vec.Vector VRF.KeyPair)
makeKeys = fmap Vec.fromList . vector


-- |Generate @good + bad@ keys, where the bad keys consistently beat the
-- good keys over @ugly@ rounds.  The last argument is a seed for
-- the random generation, to avoid searching for keys that satisfy the
-- requirements (if you already know an appropriate seed).
superCorruptKeys :: Int -> Int -> Int -> Int -> Vec.Vector VRF.KeyPair
superCorruptKeys good bad ugly = loop
    where
        areSuperCorrupt phase keys
            | phase < 0 = True
            | otherwise = maximum [valAtPhase phase (keys Vec.! k) | k <- [0..good-1]] < maximum [valAtPhase phase (keys Vec.! k) | k <- [good..good+bad-1]] 
                        && areSuperCorrupt (phase - 1) keys
        valAtPhase phase k = ticketValue (proofToTicket (unsafePerformIO $ makeTicketProof (lotteryId phase) k) 1 (good + bad))
        baid = "test" :: BS.ByteString
        lotteryId phase = Ser.runPut $ Ser.put baid >> Ser.put phase
        loop seed =
            let keys = Vec.fromList $ take (good + bad) $ unfoldr (Just . VRF.randomKeyPair) (mkStdGen seed) in
                        if areSuperCorrupt ugly keys then keys else loop (seed + 1)
makeBegins :: Int -> Gen (Seq.Seq (Party, ABBAInput))
makeBegins = fmap toBegins . vector
    where
        toBegins = Seq.fromList . zip [0..] . fmap BeginABBA

justifyAll :: Int -> Seq.Seq (Party, ABBAInput)
justifyAll n = Seq.fromList [(i, JustifyABBAChoice c) | i <- [0..fromIntegral n-1], c <- [False, True]]

allHonest :: Int -> Property
allHonest n = monadicIO $ do
    begins <- pick $ makeBegins n
    keys <- pick $ makeKeys n
    gen <- pick $ mkStdGen <$> arbitrary
    liftIO $ runABBATestRG gen "test" n n keys (justifyAll n <> begins)

multiWithInactive :: Int -> Int -> Property
multiWithInactive active inactive = monadicIO $ do
    begins <- pick $ makeBegins active
    keys <- pick $ makeKeys (active + inactive)
    gen <- pick $ mkStdGen <$> arbitrary
    liftIO $ runABBATestRG gen "test" active (active + inactive) keys (justifyAll active <> begins)

multiWithInactiveKeys :: Vec.Vector VRF.KeyPair -> Int -> Int -> Property
multiWithInactiveKeys keys active inactive = monadicIO $ do
    begins <- pick $ makeBegins active
    gen <- pick $ mkStdGen <$> arbitrary
    liftIO $ runABBATestRG gen "test" active (active + inactive) keys (justifyAll active <> begins)


multiWithCorrupt :: Int -> Int -> Property
multiWithCorrupt active corrupt = property $ do
        keys <- makeKeys (active + corrupt)
        return $ multiWithCorruptKeys keys active corrupt

multiWithCorruptKeys :: Vec.Vector VRF.KeyPair -> Int -> Int -> Property
multiWithCorruptKeys keys active corrupt = monadicIO $ do
    begins <- pick $ makeBegins active
    let baid :: BS.ByteString = "test"
    let lotteryId phase = Ser.runPut $ Ser.put baid >> Ser.put phase
    jmsgs <- liftIO $ sequence [(makeTicketProof (lotteryId phase) (keys Vec.! src)) <&> \tkt -> (a, ReceiveABBAMessage (fromIntegral src) $ Justified phase c tkt) | a <- [0..fromIntegral active-1], src <- [active..active+corrupt-1], phase<-[0..5], c <- [False, True]]
    let corruptMsgs = Seq.fromList $
            jmsgs ++
            [(a, ReceiveABBAMessage (fromIntegral src) $ CSSSeen phase $ singletonNominationSet (fromIntegral p) c) | a <- [0..fromIntegral active-1], src <- [active..active+corrupt-1], phase<-[0..5], p <-[0..active+corrupt-1], c <- [False, True]] ++
            [(a, ReceiveABBAMessage (fromIntegral src) $ WeAreDone c ) | a <- [0..fromIntegral active-1], src <- [active..active+corrupt-1], c <- [False, True]]
    gen <- pick $ mkStdGen <$> arbitrary
    liftIO $ runABBATestRG gen baid active (active + corrupt) keys (justifyAll active <> begins <> corruptMsgs)


-- This test is not valid, because it involves messages that are only selectively delivered.
multiWithCorruptKeysEvil :: Vec.Vector VRF.KeyPair -> Int -> Int -> Property
multiWithCorruptKeysEvil keys active corrupt = monadicIO $ do
    let begins = Seq.fromList $ [(p, BeginABBA (p `mod` 2 == 0)) | p <- [0..fromIntegral active-1]]
    let baid :: BS.ByteString = "test"
    let lotteryId phase = Ser.runPut $ Ser.put baid >> Ser.put phase
    jmsgs <- liftIO $ sequence [(makeTicketProof (lotteryId phase) (keys Vec.! src)) <&> \tkt -> (a, ReceiveABBAMessage (fromIntegral src) $ Justified phase (a `mod` 2 == 0) tkt) | a <- [0..fromIntegral active-1], src <- [active..active+corrupt-1], phase<-[0..5]]
    let corruptMsgs = Seq.fromList $ 
            jmsgs ++
            [(a, ReceiveABBAMessage (fromIntegral src) $ CSSSeen phase $ singletonNominationSet (fromIntegral p) (a `mod` 2 == 0)) | a <- [0..fromIntegral active-1], src <- [active..active+corrupt-1], phase<-[0..5], p <-[0..active+corrupt-1]] ++
            [(a, ReceiveABBAMessage (fromIntegral src) $ WeAreDone (a `mod` 2 == 0)) | a <- [0..fromIntegral active-1], src <- [active..active+corrupt-1]]
    jmsgs2 <- liftIO $ sequence [(makeTicketProof (lotteryId phase) (keys Vec.! src)) <&> \tkt -> (a, ReceiveABBAMessage (fromIntegral src) $ Justified phase (a `mod` 2 /= 0) tkt) | a <- [0..fromIntegral active-1], src <- [active..active+corrupt-1], phase<-[0..5]]
    let corruptMsgs2 = Seq.fromList $
            jmsgs2 ++
            [(a, ReceiveABBAMessage (fromIntegral src) $ CSSSeen phase $ singletonNominationSet (fromIntegral p) (a `mod` 2 /= 0)) | a <- [0..fromIntegral active-1], src <- [active..active+corrupt-1], phase<-[0..5], p <-[0..active+corrupt-1]] ++
            [(a, ReceiveABBAMessage (fromIntegral src) $ WeAreDone (a `mod` 2 /= 0)) | a <- [0..fromIntegral active-1], src <- [active..active+corrupt-1]]
    gen <- pick $ mkStdGen <$> arbitrary
    liftIO $ runABBATest2 gen baid active (active + corrupt) keys (justifyAll active <> begins <> corruptMsgs) corruptMsgs2



tests :: Spec
tests = parallel $ describe "Concordium.Afgjort.ABBA" $ do
    it "3 parties + 1 super inactive" $ withMaxSuccess 1000 $ multiWithInactiveKeys (superCorruptKeys 3 1 6 22636) 3 1
    it "3 parties + 1 super corrupt" $ withMaxSuccess 5000 $ multiWithCorruptKeys (superCorruptKeys 3 1 6 22636) 3 1
    it "3 parties + 1 super corrupt evil" $ withMaxSuccess 5000 $ multiWithCorruptKeysEvil (superCorruptKeys 3 1 6 22636) 3 1
    it "5 parties + 2 super corrupt" $ withMaxSuccess 5000 $ multiWithCorruptKeys (superCorruptKeys 5 2 6 4602) 5 2
    it "5 parties + 2 super corrupt evil" $ withMaxSuccess 5000 $ multiWithCorruptKeysEvil (superCorruptKeys 5 2 6 4602) 5 2
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
