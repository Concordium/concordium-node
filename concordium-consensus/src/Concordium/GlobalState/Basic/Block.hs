{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, TypeFamilies, FlexibleContexts, TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}
module Concordium.GlobalState.Basic.Block where

import Data.Serialize
import Data.Time.Clock

import Concordium.Types.Transactions
import Concordium.Types.HashableTo
import Concordium.GlobalState.Block

type BasicBakedBlock = BakedBlock Transaction
type BasicBlock = Block Transaction
type BasicPendingBlock = PendingBlock Transaction

-- |Deserialize a block.
-- NB: This does not check transaction signatures.
getBlock :: TransactionTime -> Get BasicBlock
getBlock arrivalTime = do
  sl <- get
  if sl == 0 then GenesisBlock <$> get
  else do
    bfBlockPointer <- get
    bfBlockBaker <- get
    bfBlockProof <- get
    bfBlockNonce <- get
    bfBlockLastFinalized <- get
    bbTransactions <- getListOf (getUnverifiedTransaction arrivalTime)
    bbSignature <- get
    return $ NormalBlock (BakedBlock{bbSlot = sl, bbFields = BlockFields{..}, ..})

makePendingBlock :: BasicBakedBlock -> UTCTime -> BasicPendingBlock
makePendingBlock pbBlock pbReceiveTime = PendingBlock{pbHash = getHash pbBlock,..}
