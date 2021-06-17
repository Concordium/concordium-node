{-# LANGUAGE TemplateHaskell #-}
module Concordium.Afgjort.Buffer where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform
import Control.Monad.State
import Data.Time.Clock

import qualified Concordium.Afgjort.CSS.NominationSet as NS
import Concordium.Afgjort.Finalize
import Concordium.Afgjort.FinalizationQueue
import Concordium.Afgjort.ABBA
import Concordium.Afgjort.WMVBA
import Concordium.TimeMonad
import Concordium.TimerMonad
import Concordium.Logger

type BufferId = (FinalizationMessageHeader, Phase)

data FinalizationDelay = FinalizationDelay !UTCTime !UTCTime !FinalizationMessage
  deriving(Show)

data FinalizationBuffer = FinalizationBuffer {
    _fbDelays :: !(Map BufferId FinalizationDelay),
    _fbCurrentDelayStep :: !(Maybe (BufferId, NominalDiffTime))
} deriving(Show)
makeLenses ''FinalizationBuffer

-- |The maximum time to delay a Seen message.
-- Set at 20 seconds.
maxDelay :: NominalDiffTime
maxDelay = 20

-- |The base time to delay a Seen message.
-- Seen messages will be sent at most once per 'delayStep'.
-- Set at 50 milliseconds.
delayStep :: NominalDiffTime
delayStep = 0.05

maxDelayStep :: NominalDiffTime
maxDelayStep = 5

class FinalizationBufferLenses s where
    finBuffer :: Lens' s FinalizationBuffer

emptyFinalizationBuffer :: FinalizationBuffer
emptyFinalizationBuffer = FinalizationBuffer Map.empty Nothing

-- |Buffer a finalization message.  This puts Seen messages into a buffer.
-- A new Seen message will replace an older one: it is assumed to subsume it.
-- A DoneReporting message will flush any buffered Seen message.
-- If the message is added to a buffer, then the time at which the buffer
-- should be polled and an identifier for the buffer are returned.
bufferFinalizationMessage :: (MonadState s m, FinalizationBufferLenses s, TimeMonad m, MonadLogger m, TimerMonad m) => (FinalizationMessage -> m ()) -> FinalizationMessage -> m ()
bufferFinalizationMessage handleMsg msg@FinalizationMessage{msgBody = WMVBAABBAMessage (CSSSeen phase ns) ,..} = do
        let bufId = (msgHeader, phase)
        now <- currentTime
        FinalizationBuffer{..} <- use finBuffer
        case _fbDelays ^. at bufId of
            Nothing -> do
                if NS.isFull ns then
                    handleMsg msg
                else do
                    let newDelayStep = case _fbCurrentDelayStep of
                            Nothing -> delayStep
                            Just (bufId', oldDS)
                                | bufId == bufId' -> min maxDelayStep (2 * oldDS)
                            _ -> delayStep
                    let notifyTime = addUTCTime newDelayStep now
                    finBuffer .= FinalizationBuffer {
                        _fbDelays = Map.insert bufId (FinalizationDelay notifyTime (addUTCTime maxDelay now) msg) _fbDelays,
                        _fbCurrentDelayStep = Just (bufId, newDelayStep)
                    }
                    logEvent Runner LLTrace $ "Buffering finalization message until: " ++ show notifyTime ++ " (new delay step=" ++ show newDelayStep ++ ")"
                    void $ onTimeout (DelayUntil notifyTime) $ notifyBuffer handleMsg bufId
            Just (FinalizationDelay oldNotifyTime timeout _) ->
                if oldNotifyTime <= now || NS.isFull ns then do
                    finBuffer . fbDelays %= Map.delete bufId
                    logEvent Runner LLTrace $ "Flushing buffered message with new Seen message."
                    handleMsg msg
                else do
                    let curDelayStep = case _fbCurrentDelayStep of
                            Just (bufId', ds)
                                | bufId == bufId' -> ds
                            _ -> delayStep
                    let notifyTime = min timeout (addUTCTime curDelayStep now)
                    finBuffer . fbDelays %= Map.insert bufId (FinalizationDelay notifyTime timeout msg)
                    logEvent Runner LLTrace $ "Buffering finalization message until: " ++ show notifyTime ++ " (current delay step=" ++ show curDelayStep ++ ")"
                    void $ onTimeout (DelayUntil notifyTime) $ notifyBuffer handleMsg bufId
bufferFinalizationMessage handleMsg msg@FinalizationMessage{msgBody = WMVBAABBAMessage (CSSDoneReporting phase _) ,..} = do
        let bufId = (msgHeader, phase)
        (finBuffer . fbDelays . at bufId <<.= Nothing) >>= \case
            Nothing -> handleMsg msg
            Just (FinalizationDelay _ _ seenMsg) -> do
                logEvent Runner LLTrace $ "Flushing buffered message with DoneReporting message."
                handleMsg seenMsg
                handleMsg msg
bufferFinalizationMessage handleMsg msg = handleMsg msg

-- |Alert a buffer that the notify time has elapsed.  The input time should be at least the notify time.
notifyBuffer :: (MonadState s m, FinalizationBufferLenses s, MonadLogger m, TimeMonad m) => (FinalizationMessage -> m ()) -> BufferId -> m ()
notifyBuffer handleMsg bufId = do
        notifyTime <- currentTime
        use (finBuffer . fbDelays . at bufId) >>= \case
            Nothing -> return ()
            Just (FinalizationDelay expectedNotifyTime _ msg) ->
                when (expectedNotifyTime <= notifyTime) $ do
                    finBuffer . fbDelays %= Map.delete bufId
                    logEvent Runner LLTrace $ "Flushing buffered message on notify. expectedNotifyTime=" ++ show expectedNotifyTime ++ " notifyTime=" ++ show notifyTime
                    handleMsg msg

-- |A 'FinalizationState' equipped with a 'FinalizationBuffer'.  The buffer is used in the
-- 'Concordium.Skov.MonadImplementation.BufferedFinalization' configuration to buffer Seen
-- messages so that fewer messages need to be sent.
data BufferedFinalizationState t = BufferedFinalizationState {
        _bfsFinalization :: !(FinalizationState t),
        _bfsBuffer :: !FinalizationBuffer
    }
    deriving(Show)
makeLenses ''BufferedFinalizationState

instance FinalizationQueueLenses (BufferedFinalizationState t) where
    finQueue = bfsFinalization . finQueue
instance FinalizationStateLenses (BufferedFinalizationState t) t where
    finState = bfsFinalization
instance FinalizationBufferLenses (BufferedFinalizationState t) where
    finBuffer = bfsBuffer
