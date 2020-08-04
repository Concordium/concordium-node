module Concordium.GlobalState.Basic.BlockPointer where

import Data.Time
import qualified Data.List as List
import Control.Exception
import Data.Functor.Identity
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockPointer
import qualified Concordium.Types.Transactions as Transactions

type BasicBlockPointer s = BlockPointer () Identity s

-- |Make a 'BasicBlockPointer' from a 'PendingBlock'.
-- The parent and last finalized block pointers must match the block data.
makeBasicBlockPointer ::
    PendingBlock        -- ^Pending block
    -> BasicBlockPointer s    -- ^Parent block pointer
    -> BasicBlockPointer s    -- ^Last finalized block pointer
    -> s       -- ^Block state
    -> UTCTime          -- ^Block arrival time
    -> Energy           -- ^Energy cost of all transactions in the block
    -> BasicBlockPointer s
makeBasicBlockPointer pb parent lastFinalized _bpState _bpArriveTime _bpTransactionsEnergyCost
        = assert (getHash parent == blockPointer bf) $
            assert checkLastFin $
                BlockPointer {
                    _bpInfo = BasicBlockPointerData{
                        _bpHash = getHash pb,
                        _bpHeight = bpHeight parent + 1,
                        _bpReceiveTime = pbReceiveTime pb,
                        _bpLastFinalizedHash = getHash lastFinalized,
                        ..},
                    _bpBlock = NormalBlock (pbBlock pb),
                    _bpParent = Identity parent,
                    _bpLastFinalized = Identity lastFinalized,
                    _bpATI = (),
                    ..}
    where
        bf = bbFields $ pbBlock pb
        (_bpTransactionCount, _bpTransactionsSize) =
          List.foldl' (\(clen, csize) tx -> (clen + 1, Transactions.biSize tx + csize)) (0, 0) (blockTransactions pb)
        checkLastFin = case blockFinalizationData bf of
            NoFinalizationData -> lastFinalized == runIdentity (_bpLastFinalized parent)
            BlockFinalizationData r -> getHash lastFinalized == finalizationBlockPointer r

makeGenesisBasicBlockPointer :: GenesisData -> s -> BasicBlockPointer s
makeGenesisBasicBlockPointer genData _bpState = theBlockPointer
    where
        theBlockPointer = BlockPointer {_bpInfo=BasicBlockPointerData{..},_bpATI=(),..}
        _bpBlock = GenesisBlock genData
        _bpHash = getHash _bpBlock
        _bpParent = Identity theBlockPointer
        _bpLastFinalized = Identity theBlockPointer
        _bpLastFinalizedHash = _bpHash
        _bpHeight = 0
        _bpReceiveTime = timestampToUTCTime (genesisTime genData)
        _bpArriveTime = _bpReceiveTime
        _bpTransactionCount = 0
        _bpTransactionsEnergyCost = 0
        _bpTransactionsSize = 0
