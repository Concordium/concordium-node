module Concordium.Afgjort.Buffer where

import Data.Map (Map)
import qualified Data.Map as Map
import Lens.Micro.Platform
import Control.Monad.State
import Data.Time.Clock

import Concordium.Afgjort.Finalize
import Concordium.Afgjort.ABBA
import Concordium.Afgjort.WMVBA
import Concordium.TimeMonad
import Concordium.TimerMonad
import Concordium.Logger

type BufferId = (FinalizationMessageHeader, Phase)

type FinalizationBuffer = Map BufferId (UTCTime, UTCTime, FinalizationMessage)

-- |The maximum time to delay a Seen message.
-- Set at 20 seconds.
maxDelay :: NominalDiffTime
maxDelay = 20

-- |The base time to delay a Seen message.
-- Seen messages will be sent at most once per 'delayStep'.
-- Set at 5 seconds.
delayStep :: NominalDiffTime
delayStep = 5

class FinalizationBufferLenses s where
    finBuffer :: Lens' s FinalizationBuffer

emptyFinalizationBuffer :: FinalizationBuffer
emptyFinalizationBuffer = Map.empty

-- |Buffer a finalization message.  This puts Seen messages into a buffer.
-- A new Seen message will replace an older one: it is assumed to subsume it.
-- A DoneReporting message will flush any buffered Seen message.
-- If the message is added to a buffer, then the time at which the buffer
-- should be polled and an identifier for the buffer are returned.
bufferFinalizationMessage :: (MonadState s m, FinalizationBufferLenses s, TimeMonad m, LoggerMonad m, TimerMonad m) => (FinalizationMessage -> m ()) -> FinalizationMessage -> m ()
bufferFinalizationMessage handleMsg msg@FinalizationMessage{msgBody = WMVBAABBAMessage (CSSSeen phase _) ,..} = do
        let bufId = (msgHeader, phase)
        now <- currentTime
        use (finBuffer . at bufId) >>= \case
            Nothing -> do
                let notifyTime = addUTCTime delayStep now
                finBuffer . at bufId ?= (notifyTime, addUTCTime maxDelay now, msg)
                logEvent Runner LLTrace $ "Buffering finalization message until: " ++ show notifyTime
                void $ onTimeout (DelayUntil notifyTime) $ notifyBuffer handleMsg bufId
            Just (oldNotifyTime, timeout, _) ->
                if oldNotifyTime <= now then do
                    finBuffer . at bufId .= Nothing
                    logEvent Runner LLTrace $ "Flushing buffered message with new Seen message."
                    handleMsg msg
                else do
                    let notifyTime = min timeout (addUTCTime delayStep now)
                    finBuffer . at bufId ?= (notifyTime, timeout, msg)
                    logEvent Runner LLTrace $ "Buffering finalization message until: " ++ show notifyTime
                    void $ onTimeout (DelayUntil notifyTime) $ notifyBuffer handleMsg bufId
bufferFinalizationMessage handleMsg msg@FinalizationMessage{msgBody = WMVBAABBAMessage (CSSDoneReporting phase _) ,..} = do
        let bufId = (msgHeader, phase)
        (finBuffer . at bufId <<.= Nothing) >>= \case
            Nothing -> handleMsg msg
            Just (_, _, seenMsg) -> do
                logEvent Runner LLTrace $ "Flushing buffered message with DoneReporting message."
                handleMsg seenMsg
                handleMsg msg
bufferFinalizationMessage handleMsg msg = handleMsg msg

-- |Alert a buffer that the notify time has elapsed.  The input time should be at least the notify time.
notifyBuffer :: (MonadState s m, FinalizationBufferLenses s, LoggerMonad m, TimeMonad m) => (FinalizationMessage -> m ()) -> BufferId -> m ()
notifyBuffer handleMsg bufId = do
        notifyTime <- currentTime
        use (finBuffer . at bufId) >>= \case
            Nothing -> return ()
            Just (expectedNotifyTime, _, msg) ->
                when (expectedNotifyTime <= notifyTime) $ do
                    finBuffer . at bufId .= Nothing
                    logEvent Runner LLTrace $ "Flushing buffered message on notify. expectedNotifyTime=" ++ show expectedNotifyTime ++ " notifyTime=" ++ show notifyTime
                    handleMsg msg