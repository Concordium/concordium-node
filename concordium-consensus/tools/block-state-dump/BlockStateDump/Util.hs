module BlockStateDump.Util where

import Control.Monad.IO.Class

throwUserError :: (MonadIO m) => String -> m a
throwUserError = liftIO . ioError . userError
