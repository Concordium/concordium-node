{-# LANGUAGE RecordWildCards, TypeFamilies, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Concordium.GlobalState.Persistent.Block where

import Concordium.Types.PersistentTransactions
import Concordium.Types.Transactions
import Concordium.Types.HashableTo
import Data.Serialize
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Block
import Data.Time.Clock

type PersistentBakedBlock = BakedBlock PersistentTransaction
type PersistentBlock = Block PersistentTransaction
type PersistentPendingBlock = PendingBlock PersistentTransaction

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
