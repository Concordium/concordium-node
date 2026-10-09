{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.TimerMonad (
    Timeout (..),
    TimerMonad (..),
    ThreadTimer,
    makeThreadTimer,
    cancelThreadTimer,
) where

import Data.Time
#if defined(mingw32_HOST_OS)
import Control.Concurrent
#else
import GHC.Event
#endif

import qualified Concordium.TimerMonad.Internal as Internal

-- | Representation of a waiting period.
data Timeout
    = -- | Wait for a certain period of time
      DelayFor NominalDiffTime
    | -- | Wait until a given time
      DelayUntil UTCTime
    deriving (Show)

class (Monad m) => TimerMonad m where
    type Timer m
    onTimeout :: Timeout -> m a -> m (Timer m)
    cancelTimer :: Timer m -> m ()

data ThreadTimer = ThreadTimer !Internal.InternalTimer

#if defined(mingw32_HOST_OS)
data ThreadTimerBackend = ThreadTimerBackend

instance Internal.TimerBackend ThreadTimerBackend ThreadId where
    timerCurrentTime _ = getCurrentTime
    timerSchedule _ delay callback = forkIO $ threadDelay delay >> callback
    timerCancel _ _ = pure ()
#else
newtype ThreadTimerBackend = ThreadTimerBackend TimerManager

instance Internal.TimerBackend ThreadTimerBackend TimeoutKey where
    timerCurrentTime _ = getCurrentTime
    timerSchedule (ThreadTimerBackend manager) = registerTimeout manager
    timerCancel (ThreadTimerBackend manager) = unregisterTimeout manager
#endif

-- | Normalize a timeout to an absolute deadline at registration time.
timeoutDeadline :: Timeout -> IO UTCTime
timeoutDeadline (DelayFor delay) = addUTCTime delay <$> getCurrentTime
timeoutDeadline (DelayUntil deadline) = pure deadline

-- | Create a platform timer that invokes its action no earlier than the requested timeout.
makeThreadTimer :: Timeout -> IO () -> IO ThreadTimer
#if defined(mingw32_HOST_OS)
makeThreadTimer timeout action = do
    deadline <- timeoutDeadline timeout
    ThreadTimer <$> Internal.makeInternalTimer ThreadTimerBackend deadline action
#else
makeThreadTimer timeout action = do
    deadline <- timeoutDeadline timeout
    manager <- getSystemTimerManager
    ThreadTimer <$> Internal.makeInternalTimer (ThreadTimerBackend manager) deadline action
#endif

-- | Cancel the timer created by 'makeThreadTimer'. Note that if the given
--  computation (second argument of 'makeThreadTimer') has already started it is
--  not interrupted.
cancelThreadTimer :: ThreadTimer -> IO ()
cancelThreadTimer (ThreadTimer timer) = Internal.cancelInternalTimer timer
