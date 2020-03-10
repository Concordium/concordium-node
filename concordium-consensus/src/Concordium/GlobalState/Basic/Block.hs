{-# LANGUAGE MultiParamTypeClasses, RecordWildCards, TypeFamilies, FlexibleContexts, TypeSynonymInstances, FlexibleInstances, UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Concordium.GlobalState.Basic.Block where

import Data.Serialize.Put
import Data.Serialize
import Data.Time.Clock

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.Transactions
import Concordium.Types.HashableTo
import Concordium.GlobalState.Block

type BasicBakedBlock = BakedBlock Transaction
type BasicBlock = Block Transaction
type BasicPendingBlock = PendingBlock Transaction

instance HashableTo BlockHash BasicBakedBlock where
    getHash b = Hash.hashLazy . runPutLazy $ blockBody b >> put (bbSignature b)

instance HashableTo BlockHash BasicBlock where
    getHash (GenesisBlock genData) = Hash.hashLazy . runPutLazy $ put genesisSlot >> put genData
    getHash (NormalBlock bb) = getHash bb

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
