{-# LANGUAGE NumericUnderscores #-}

-- | Tests for timer scheduling.
module ConcordiumTests.TimerMonad (tests) where

import Control.Concurrent.MVar
import Data.Time
import qualified System.Timeout as Timeout
import Test.Hspec

import Concordium.TimerMonad

-- | Register timer scheduling tests.
tests :: Spec
tests = describe "TimerMonad" $
    it "does not invoke a far-future ThreadTimer callback early" $ do
        callback <- newEmptyMVar
        now <- getCurrentTime
        -- we put "()" into the callback box, to prove that the timer did not trigger
        timer <- makeThreadTimer (DelayUntil $ addUTCTime farFutureDelay now) (putMVar callback ())
        -- then we wait 100 ms.
        result <- Timeout.timeout 100_000 $ takeMVar callback
        cancelThreadTimer timer
        result `shouldBe` Nothing

farFutureDelay :: NominalDiffTime
farFutureDelay = 10_000_000_000_000
