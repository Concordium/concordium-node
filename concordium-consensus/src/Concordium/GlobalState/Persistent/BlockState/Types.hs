{-# LANGUAGE ConstraintKinds #-}
module Concordium.GlobalState.Persistent.BlockState.Types where

import Control.Monad.Reader

import Concordium.GlobalState.Persistent.BlobStore

type MonadStateStore r m = (MonadIO m, MonadReader r m, HasBlobStore r)