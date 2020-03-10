{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Concordium.GlobalState.Persistent.Block where

import Concordium.Types.PersistentTransactions
import Concordium.Types.Transactions
import Concordium.Types.HashableTo
import Concordium.Crypto.SHA256 as Hash
import Data.Serialize
import Concordium.Types
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockMonads
import Data.Time.Clock

type PersistentBakedBlock = BakedBlock PersistentTransaction
type PersistentBlock = Block PersistentTransaction
type PersistentPendingBlock = PendingBlock PersistentTransaction

instance (Monad m, Convert Transaction PersistentTransaction m) =>
    HashableTo (m BlockHash) PersistentBakedBlock where
  getHash b = do
    putter <- fullBody b
    return $ Hash.hashLazy . runPutLazy $ putter >> put (bbSignature b)

instance (Monad m,
          BlockDataMonad PersistentBlock m) =>
         HashableTo (m BlockHash) PersistentBlock where
    getHash (GenesisBlock genData) =
      return $ Hash.hashLazy . runPutLazy $ put genesisSlot >> put genData
    getHash (NormalBlock bb) = getHash bb

-- | Create a `Get` for a PersistentBlock
--
-- Note that this block won't have the transaction data.
getBlock :: TransactionTime -> Get PersistentBlock
getBlock arrivalTime = do
  sl <- get
  if sl == 0 then GenesisBlock <$> get
  else do
    bfBlockPointer <- get
    bfBlockBaker <- get
    bfBlockProof <- get
    bfBlockNonce <- get
    bfBlockLastFinalized <- get
    bbTransactions <- getListOf (getPersistentTransaction arrivalTime)
    bbSignature <- get
    return $ NormalBlock (BakedBlock{bbSlot = sl, bbFields = BlockFields{..}, ..})

makePendingBlock :: (Convert Transaction PersistentTransaction m) =>
                   PersistentBakedBlock -> UTCTime -> m PersistentPendingBlock
makePendingBlock pbBlock pbReceiveTime = do
  pbHash <- getHash pbBlock
  return $ PendingBlock{..}
