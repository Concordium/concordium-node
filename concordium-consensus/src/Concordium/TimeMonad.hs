{-# LANGUAGE DefaultSignatures #-}

module Concordium.TimeMonad where

import Control.Monad.IO.Class
import qualified Control.Monad.RWS.Strict as Strict
import qualified Control.Monad.State.Strict as Strict
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.RWS
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Time

import Concordium.Logger

import Concordium.GlobalState.Classes (MGSTrans)

class Monad m => TimeMonad m where
    currentTime :: m UTCTime
    default currentTime :: MonadIO m => m UTCTime
    currentTime = liftIO getCurrentTime

-- |Measure the time of a given action m a.
-- Note. One has to remember to factor in laziness when
-- reasoning about the returned 'NominalDiffTime'.
measureTime :: TimeMonad m => m a -> m (NominalDiffTime, a)
measureTime a = do
    now <- currentTime
    res <- a
    after <- currentTime
    return (diffUTCTime after now, res)

instance TimeMonad IO

instance TimeMonad m => TimeMonad (StateT s m) where
    currentTime = lift currentTime
    {-# INLINE currentTime #-}

instance TimeMonad m => TimeMonad (Strict.StateT s m) where
    currentTime = lift currentTime
    {-# INLINE currentTime #-}

instance (TimeMonad m, Monoid w) => TimeMonad (RWST r w s m) where
    currentTime = lift currentTime
    {-# INLINE currentTime #-}

instance (TimeMonad m, Monoid w) => TimeMonad (Strict.RWST r w s m) where
    currentTime = lift currentTime
    {-# INLINE currentTime #-}

instance TimeMonad m => TimeMonad (MaybeT m) where
    currentTime = lift currentTime
    {-# INLINE currentTime #-}

instance TimeMonad m => TimeMonad (ExceptT e m) where
    currentTime = lift currentTime
    {-# INLINE currentTime #-}

instance TimeMonad m => TimeMonad (LoggerT m) where
    currentTime = lift currentTime
    {-# INLINE currentTime #-}

instance TimeMonad m => TimeMonad (ReaderT r m) where
    currentTime = lift currentTime
    {-# INLINE currentTime #-}

instance (MonadTrans t, Monad (t m), TimeMonad m) => TimeMonad (MGSTrans t m) where
    currentTime = lift currentTime
    {-# INLINE currentTime #-}
