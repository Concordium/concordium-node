{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
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

import qualified Concordium.Crypto.VRF as VRF
import Concordium.Afgjort.ABBA

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
            | null msgs = return $ property $ not checkSucceed || all (checkRes (ress Vec.! 0)) ress
            | otherwise = do
                ((rcpt, inp), msgs') <- selectFromSeq msgs
                let (_, s', out) = runABBA (makeInput inp) (inst rcpt) (sts Vec.! rcpt)
                case invariantABBAState (inst rcpt) s' of
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


tests :: Spec
tests = describe "Concordium.Afgjort.ABBA" $ do
    it "Two parties" $ withMaxSuccess 10000 $ allHonest 2
    it "Three parties" $ withMaxSuccess 10000 $ allHonest 3
    it "5 parties" $ withMaxSuccess 1000 $ allHonest 5
    it "25 parties" $ withMaxSuccess 100 $ allHonest 25
    it "3 parties + 1 inactive" $ withMaxSuccess 1000 $ multiWithInactive 3 1