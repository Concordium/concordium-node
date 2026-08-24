{-# LANGUAGE NumericUnderscores #-}

-- | Tests for timer scheduling.
module ConcordiumTests.TimerMonad (tests) where

import Control.Concurrent.MVar
import Data.IORef
import qualified Data.Map.Strict as Map
import Data.Time
import qualified System.Timeout as Timeout
import Test.Hspec

import Concordium.TimerMonad
import Concordium.TimerMonad.Internal

-- | Register timer scheduling tests.
tests :: Spec
tests = describe "TimerMonad" $ do
    it "does not invoke a far-future ThreadTimer callback early" $ do
        callback <- newEmptyMVar
        now <- getCurrentTime
        -- we put "()" into the callback box, to prove that the timer did not trigger
        timer <- makeThreadTimer (DelayUntil $ addUTCTime farFutureDelay now) (putMVar callback ())
        -- then we wait 100 ms.
        result <- Timeout.timeout 100_000 $ takeMVar callback
        cancelThreadTimer timer
        result `shouldBe` Nothing

    it "invokes a far-future timer at its deadline and not before" $ do
        (backend, state) <- newTimerBackend baseTime
        callbackCount <- newIORef (0 :: Int)
        let deadline = addUTCTime farFutureDelay baseTime
        _ <- makeInternalTimer backend deadline (modifyIORef' callbackCount (+ 1))
        runNextTimer state
        readIORef callbackCount `shouldReturn` 0
        setCurrentTime state $ addUTCTime (-1) deadline
        runNextTimer state
        readIORef callbackCount `shouldReturn` 0
        setCurrentTime state deadline
        runNextTimer state
        readIORef callbackCount `shouldReturn` 1

    it "does not invoke the action after an early backend wake-up" $ do
        (backend, state) <- newTimerBackend baseTime
        callbackCount <- newIORef (0 :: Int)
        let deadline = addUTCTime 10 baseTime
        _ <- makeInternalTimer backend deadline (modifyIORef' callbackCount (+ 1))
        runNextTimer state
        readIORef callbackCount `shouldReturn` 0
        setCurrentTime state deadline
        runNextTimer state
        readIORef callbackCount `shouldReturn` 1

    it "retains the original deadline across early backend wake-ups" $ do
        (backend, state) <- newTimerBackend baseTime
        callbackCount <- newIORef (0 :: Int)
        let deadline = addUTCTime 10 baseTime
        _ <- makeInternalTimer backend deadline (modifyIORef' callbackCount (+ 1))
        setCurrentTime state $ addUTCTime 9 baseTime
        runNextTimer state
        readIORef callbackCount `shouldReturn` 0
        setCurrentTime state deadline
        runNextTimer state
        readIORef callbackCount `shouldReturn` 1

    it "does not invoke or reschedule the action after cancellation" $ do
        (backend, state) <- newTimerBackend baseTime
        callbackCount <- newIORef (0 :: Int)
        let deadline = addUTCTime 10 baseTime
        timer <- makeInternalTimer backend deadline (modifyIORef' callbackCount (+ 1))
        wakeUp <- takeNextTimer state
        cancelInternalTimer timer
        setCurrentTime state deadline
        wakeUp
        readIORef callbackCount `shouldReturn` 0
        hasPendingTimers state `shouldReturn` False

farFutureDelay :: NominalDiffTime
farFutureDelay = 10_000_000_000_000

baseTime :: UTCTime
baseTime = UTCTime (fromGregorian 2024 1 1) 0

newtype DummyTimer = DummyTimer Int

data DummyTimerState = DummyTimerState
    { currentTime :: UTCTime,
      nextTimerId :: Int,
      pendingTimers :: Map.Map Int (IO ())
    }

newTimerBackend :: UTCTime -> IO (TimerBackend DummyTimer, IORef DummyTimerState)
newTimerBackend startTime = do
    state <- newIORef $ DummyTimerState startTime 0 Map.empty
    let
        schedule _ action = atomicModifyIORef' state $ \timerState ->
            let timerId = nextTimerId timerState
                updatedState =
                    timerState
                        { nextTimerId = timerId + 1,
                          pendingTimers = Map.insert timerId action (pendingTimers timerState)
                        }
            in  (updatedState, DummyTimer timerId)
        cancel (DummyTimer timerId) =
            modifyIORef' state $ \timerState -> timerState{pendingTimers = Map.delete timerId (pendingTimers timerState)}
    pure
        ( TimerBackend
            { timerCurrentTime = currentTime <$> readIORef state,
              timerSchedule = schedule,
              timerCancel = cancel
            },
          state
        )

setCurrentTime :: IORef DummyTimerState -> UTCTime -> IO ()
setCurrentTime state newTime = modifyIORef' state $ \timerState -> timerState{currentTime = newTime}

runNextTimer :: IORef DummyTimerState -> IO ()
runNextTimer state = takeNextTimer state >>= id

takeNextTimer :: IORef DummyTimerState -> IO (IO ())
takeNextTimer state = atomicModifyIORef' state $ \timerState ->
    case Map.minViewWithKey (pendingTimers timerState) of
        Nothing -> (timerState, pure ())
        Just ((_, action), remainingTimers) ->
            (timerState{pendingTimers = remainingTimers}, action)

hasPendingTimers :: IORef DummyTimerState -> IO Bool
hasPendingTimers state = not . Map.null . pendingTimers <$> readIORef state
