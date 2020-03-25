{-# LANGUAGE TypeFamilies #-}
module Concordium.TimerMonad(
    Timeout(..),
    TimerMonad(..),
    ThreadTimer,
    makeThreadTimer,
    cancelThreadTimer
) where

import Data.Time
import GHC.Event


-- |Representation of a waiting period.
data Timeout
    = DelayFor NominalDiffTime
    -- ^Wait for a certain period of time
    | DelayUntil UTCTime
    -- ^Wait until a given time

class Monad m => TimerMonad m where
    type Timer m
    onTimeout :: Timeout -> m a -> m (Timer m)
    cancelTimer :: Timer m -> m ()

data ThreadTimer = ThreadTimer !TimerManager !TimeoutKey

-- |Compute a delay in microseconds. This assumes that the delay will
-- fit into an Int. On 64-bit platforms this means the delay should be no more
-- than 290000 years.
getDelay :: Timeout -> IO Int
getDelay (DelayFor d) = return (truncate (d * 1e6))
getDelay (DelayUntil t) = do
  now <- getCurrentTime
  return (truncate ((diffUTCTime t now) * 1e6))

makeThreadTimer :: Timeout -> IO () -> IO ThreadTimer
makeThreadTimer timeout action = do
  manager <- getSystemTimerManager
  micros <- getDelay timeout
  key <- registerTimeout manager micros action
  return $! ThreadTimer manager key

-- |Cancel the timer created by 'makeThreadTimer'. Note that if the given
-- computation (second argument of 'makeThreadTimer') has already started it is
-- not interrupted.
cancelThreadTimer :: ThreadTimer -> IO ()
cancelThreadTimer (ThreadTimer manager key) =
  unregisterTimeout manager key
