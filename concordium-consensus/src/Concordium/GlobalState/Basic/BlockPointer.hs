{-# LANGUAGE
        RecordWildCards,
        MultiParamTypeClasses,
        TypeFamilies,
        FlexibleInstances
        #-}
module Concordium.GlobalState.Basic.BlockPointer where

import Data.Time
import Data.Time.Clock.POSIX
import qualified Data.List as List
import Control.Exception
import Data.Functor.Identity
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.Basic.Block
import qualified Concordium.Types.Transactions as Transactions

type BasicBlockPointer s = BlockPointer () Transactions.Transaction Identity s

-- |Make a 'BasicBlockPointer' from a 'PendingBlock'.
-- The parent and last finalized block pointers must match the block data.
makeBasicBlockPointer ::
    BasicPendingBlock        -- ^Pending block
    -> BasicBlockPointer s    -- ^Parent block pointer
    -> BasicBlockPointer s    -- ^Last finalized block pointer
    -> s       -- ^Block state
    -> UTCTime          -- ^Block arrival time
    -> Energy           -- ^Energy cost of all transactions in the block
    -> BasicBlockPointer s
makeBasicBlockPointer pb parent lastFinalized _bpState _bpArriveTime _bpTransactionsEnergyCost
        = assert (getHash parent == blockPointer bf) $
            assert (getHash lastFinalized == blockLastFinalized bf) $
                BlockPointer {
                    _bpInfo = BasicBlockPointerData{
                        _bpHash = getHash pb,
                        _bpHeight = bpHeight parent + 1,
                        _bpReceiveTime = pbReceiveTime pb,
                        ..},
                      _bpBlock = NormalBlock (pbBlock pb),
                      _bpParent = Identity parent,
                      _bpLastFinalized = Identity lastFinalized,
                      _bpATI = (),
                    ..}
    where
        bf = bbFields $ pbBlock pb
        (_bpTransactionCount, _bpTransactionsSize) = List.foldl' (\(clen, csize) tx -> (clen + 1, Transactions.trSize tx + csize)) (0, 0) (blockTransactions pb)

makeGenesisBasicBlockPointer :: GenesisData -> s -> BasicBlockPointer s
makeGenesisBasicBlockPointer genData _bpState = theBlockPointer
    where
        theBlockPointer = BlockPointer {_bpInfo=BasicBlockPointerData{..},_bpATI=(),..}
        _bpBlock = GenesisBlock genData
        _bpHash = getHash _bpBlock
        _bpParent = Identity theBlockPointer
        _bpLastFinalized = Identity theBlockPointer
        _bpHeight = 0
        _bpReceiveTime = posixSecondsToUTCTime (fromIntegral (genesisTime genData))
        _bpArriveTime = _bpReceiveTime
        _bpTransactionCount = 0
        _bpTransactionsEnergyCost = 0
        _bpTransactionsSize = 0
