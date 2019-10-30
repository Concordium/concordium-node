{-# LANGUAGE TypeFamilies #-}
module Concordium.TimerMonad(
    Timeout(..),
    TimerMonad(..),
    ThreadTimer,
    makeThreadTimer,
    cancelThreadTimer
) where

import Control.Concurrent
import Control.Monad
import Data.Time
import Data.IORef


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

data ThreadTimer = ThreadTimer !ThreadId (IORef Bool)

maxDelay :: NominalDiffTime
maxDelay = fromIntegral ((maxBound :: Int) `div` 1000000)

delayFor :: NominalDiffTime -> IO ()
delayFor d
    | d <= 0 = return ()
    | d > maxDelay = delayFor maxDelay >> delayFor (d - maxDelay)
    | otherwise = threadDelay (truncate (d * 1e6))

delay :: Timeout -> IO ()
delay (DelayFor d) = delayFor d
delay (DelayUntil t) = do
        now <- getCurrentTime
        delayFor $ diffUTCTime t now

makeThreadTimer :: Timeout -> IO () -> IO ThreadTimer
makeThreadTimer timeout action = do
        enabled <- newIORef True
        thread <- forkIO $ do
            delay timeout
            continue <- readIORef enabled
            when continue action
        return $ ThreadTimer thread enabled

cancelThreadTimer :: ThreadTimer -> IO ()
cancelThreadTimer (ThreadTimer _ enabled) = writeIORef enabled False