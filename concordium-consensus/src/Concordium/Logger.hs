{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
-- |Event logging monad.
module Concordium.Logger where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (MonadTrans(..))
import Control.Monad.Trans.State (StateT)
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans.RWS (RWST)
import qualified Control.Monad.RWS.Strict as Strict
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.Word

import Concordium.GlobalState.BlockState (BSMTrans)

-- |The source module for a log event.
data LogSource
    = Runner
    | Afgjort
    | Birk
    | Crypto
    | Kontrol
    | Skov
    | Baker
    | External
    deriving (Eq, Ord, Show)

-- |Convert a 'LogSource' value to the representation required by the
-- Rust API.
logSourceId :: LogSource -> Word8
logSourceId Runner = 1
logSourceId Afgjort = 2
logSourceId Birk = 3
logSourceId Crypto = 4
logSourceId Kontrol = 5
logSourceId Skov = 6
logSourceId Baker = 7
logSourceId External = 8

-- |The logging level for a log event.
data LogLevel
    = LLError
    | LLWarning
    | LLInfo
    | LLDebug
    | LLTrace
    deriving (Eq, Ord)

instance Show LogLevel where
    show LLError = "ERROR"
    show LLWarning = "WARNING"
    show LLInfo = "INFO"
    show LLDebug = "DEBUG"
    show LLTrace = "TRACE"

-- |Convert a 'LogLevel' value to the representation required by the
-- Rust API.
logLevelId :: LogLevel -> Word8
logLevelId LLError = 1
logLevelId LLWarning = 2
logLevelId LLInfo = 3
logLevelId LLDebug = 4
logLevelId LLTrace = 5

-- |A method for logging an event in a given monad.
type LogMethod m = LogSource -> LogLevel -> String -> m ()

-- |The 'LoggerT' monad transformer equips a monad with logging
-- functionality.
newtype LoggerT m a = LoggerT {runLoggerT' :: ReaderT (LogMethod m) m a}
    deriving (Functor, Applicative, Monad, MonadIO)

-- |Run an action in the 'LoggerT' monad, handling log events with the
-- given log method.
runLoggerT :: LoggerT m a -> LogMethod m -> m a
runLoggerT = runReaderT . runLoggerT'

-- |Run an action in the 'LoggerT' monad, discarding all log events.
runSilentLogger :: (Monad m) => LoggerT m a -> m a
runSilentLogger a = runLoggerT a (\_ _ _ -> pure ())

-- |Class for a monad that supports logging.
class Monad m => LoggerMonad m where
    -- |Record a log event.
    logEvent :: LogMethod m

instance Monad m => LoggerMonad (LoggerT m) where
    logEvent src lvl msg = LoggerT $ do
        le <- ask
        lift $ le src lvl msg

instance MonadTrans LoggerT where
    lift = LoggerT . lift

instance (LoggerMonad m, Monoid w) => LoggerMonad (RWST r w s m) where
    logEvent src lvl msg = lift (logEvent src lvl msg)

instance (LoggerMonad m, Monoid w) => LoggerMonad (Strict.RWST r w s m) where
    logEvent src lvl msg = lift (logEvent src lvl msg)

instance LoggerMonad m => LoggerMonad (StateT s m) where
    logEvent src lvl msg = lift (logEvent src lvl msg)

instance LoggerMonad m => LoggerMonad (Strict.StateT s m) where
    logEvent src lvl msg = lift (logEvent src lvl msg)

instance LoggerMonad m => LoggerMonad (MaybeT m) where
    logEvent src lvl msg = lift (logEvent src lvl msg)

instance LoggerMonad m => LoggerMonad (ExceptT e m) where
    logEvent src lvl msg = lift (logEvent src lvl msg)

instance (Monad (t m), MonadTrans t, LoggerMonad m) => LoggerMonad (BSMTrans t m) where
    logEvent src lvl msg = lift (logEvent src lvl msg)

type LogIO = LoggerT IO
