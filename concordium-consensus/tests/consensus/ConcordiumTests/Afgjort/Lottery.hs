{-# LANGUAGE OverloadedStrings #-}
module ConcordiumTests.Afgjort.Lottery where

import qualified Concordium.Crypto.VRF as VRF
import Concordium.Afgjort.Lottery

import Numeric.SpecFunctions (incompleteBeta)
import System.Random
import System.IO.Unsafe

import qualified Data.ByteString.Char8 as BS
import Test.QuickCheck
import Test.QuickCheck.Monadic
import Test.Hspec

ticketCheck :: Property
ticketCheck = monadicIO $ do
    keyp <- pick $ arbitrary
    lotteryid <- pick $ BS.pack <$> arbitrary
    tproof <- run $ makeTicketProof lotteryid keyp
    let ticket = proofToTicket tproof 1 10
    return $ checkTicket lotteryid (VRF.publicKey keyp) ticket 

ticketNoCheckOther :: Property
ticketNoCheckOther = property $ \kp1 kp2 -> kp1 /= kp2 ==> monadicIO $ do
    lotteryid <- pick $ BS.pack <$> arbitrary
    tproof <- run $ makeTicketProof lotteryid kp1
    let ticket = proofToTicket tproof 1 10
    return $ not $ checkTicket lotteryid (VRF.publicKey kp2) ticket

-- |Cumulative distribution function for the binomial distribution
binCdf :: Integer -> Integer -> Double -> Double
binCdf k n p = incompleteBeta (fromIntegral $ n - k) (fromIntegral $ k + 1) (1 - p)

-- |Compute a p-value for observing a particular number of successes for
-- a binomial distribution with a given number of samples and probability.
pval :: Integer -> Integer -> Double -> Double
pval samples successes p = 2 * min (binCdf successes samples p) (1 - binCdf (successes - 1) samples p )


-- |Run a number of simulations of two parties competing in the lottery,
-- where the second party has twice the weight of the first.  The result
-- is the probability of observing an outcome that is at least as extreme
-- under the assumption that the first party is expected to win each round
-- with probability 1/3.
doubleWeightTrial :: Integer -> Double
doubleWeightTrial samples = pval samples (trials samples 0) p
    where
        g = mkStdGen 0
        (keyp1, g1) = VRF.randomKeyPair g
        (keyp2, _) = VRF.randomKeyPair g1
        trials 0 n = n
        trials r n = trials (r - 1) (trial r + n)
        w1 = 1
        w2 = 2
        tw = 10
        p = fromIntegral w1 / fromIntegral (w1 + w2)
        trial r = let lid = BS.pack $ show r in
                    if ticketValue (proofToTicket (unsafePerformIO $ makeTicketProof lid keyp1) w1 tw) > ticketValue (proofToTicket (unsafePerformIO $ makeTicketProof lid keyp2) w2 tw) then 1 else 0

tests :: Word -> Spec
tests lvl = parallel $ describe "ConcordiumTests.Afgjort.Lottery" $ do
    it "Generated ticket passes check" $ withMaxSuccess (10^lvl) $ ticketCheck
    it "Generated ticket does not pass for other keypair" $ withMaxSuccess (10^lvl) $ ticketNoCheckOther
    let p = doubleWeightTrial 100000 in it ("double weight trial p-value>0.05 (n=100000): p = " ++ show p) $ p > 0.05
