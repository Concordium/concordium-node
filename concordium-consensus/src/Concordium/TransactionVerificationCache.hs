{-# LANGUAGE TemplateHaskell #-}
module Concordium.TransactionVerificationCache
where

import Lens.Micro.Platform
import Control.Monad.State.Class
import Control.Monad.RWS.Strict
import Data.Hashable

import Concordium.Types
import qualified Concordium.Caching as Caching
import qualified Concordium.TransactionVerification as TVer

newtype Cache = Cache {
    -- |transactionVerificationResults
    -- Transaction which have been subject to a 'verification' resides in this cache.
    -- The purpose of the cache is to eliminate the need for re-verifying already verified transactions.
    -- Entries should be deleted when either the corresponding transaction has been *purged* or *finalized*. 
  transactionVerificationResults :: Caching.Cache TransactionHash TVer.VerificationResult
}

makeLenses ''Cache

emptyCache :: Cache
emptyCache = Cache Caching.empty

class CacheMonad m where
  insert :: k -> v -> m ()
  lookup :: k -> m (Maybe v)
  delete :: k -> m ()
