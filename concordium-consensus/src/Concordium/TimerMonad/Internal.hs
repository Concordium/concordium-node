module Concordium.TimerMonad.Internal (
    TimerBackend (..),
    InternalTimer,
    makeInternalTimer,
    cancelInternalTimer,
) where

import Control.Concurrent.MVar
import Control.Monad
import Data.Time

-- | Operations needed to schedule a timer chunk.
--
-- This module is shared by the production timer implementation and its
-- deterministic tests; it is not part of the public consensus library API.
data TimerBackend timer = TimerBackend
    { -- | Get the current time.
      timerCurrentTime :: IO UTCTime,
      -- | Schedule an action after a delay in microseconds.
      timerSchedule :: Int -> IO () -> IO timer,
      -- | Cancel a scheduled action.
      timerCancel :: timer -> IO ()
    }

-- | A cancellable internal timer.
newtype InternalTimer = InternalTimer (IO ())

-- | Largest delay supported by the platform timer implementation.
maxTimerDelay :: NominalDiffTime
maxTimerDelay = fromIntegral (maxBound :: Int) / 1e6

-- | Compute one bounded timer delay in microseconds.
--
-- The deadline is checked again after each chunk, so the conversion to 'Int'
-- cannot cause a timer to fire before its requested deadline.
boundedDelay :: TimerBackend timer -> UTCTime -> IO Int
boundedDelay TimerBackend{timerCurrentTime = getTime} deadline = do
    now <- getTime
    pure $ max 1 $ truncate (min maxTimerDelay (diffUTCTime deadline now) * 1e6)

-- | Create a timer that invokes its action no earlier than the requested deadline.
--
-- Delays outside the platform timer range are scheduled in bounded chunks.
makeInternalTimer :: TimerBackend timer -> UTCTime -> IO () -> IO InternalTimer
makeInternalTimer backend@TimerBackend{timerCurrentTime = getTime, timerSchedule = scheduleTimer, timerCancel = cancelScheduledTimer} deadline action = do
    -- Serialize chunk rescheduling with cancellation, so cancellation cannot leave a successor chunk scheduled.
    state <- newMVar (True, Nothing)
    let
        schedule = do
            now <- getTime
            if now >= deadline
                then scheduleAction
                else do
                    -- schedule a new (bounded) delay, moving us closer to the deadline,
                    -- if not there/past
                    delay <- boundedDelay backend deadline
                    void . modifyMVar state $ \(enabled, _) ->
                        if enabled
                            then do
                                timer <- scheduleTimer delay schedule
                                pure ((enabled, Just timer), ())
                            else pure ((enabled, Nothing), ())
        -- Schedule expired timers too, preserving the asynchronous TimerMonad contract.
        scheduleAction =
            void . modifyMVar state $ \(enabled, _) ->
                if enabled
                    then do
                        timer <- scheduleTimer 1 $ do
                            continue <- withMVar state (pure . fst)
                            when continue action
                        pure ((enabled, Just timer), ())
                    else pure ((enabled, Nothing), ())
        cancel = do
            activeTimer <- modifyMVar state $ \(_, timer) -> pure ((False, Nothing), timer)
            forM_ activeTimer cancelScheduledTimer
    schedule
    pure $ InternalTimer cancel

-- | Cancel a timer created by 'makeInternalTimer'.
--
-- If the action has already begun, it is not interrupted.
cancelInternalTimer :: InternalTimer -> IO ()
cancelInternalTimer (InternalTimer cancel) = cancel
