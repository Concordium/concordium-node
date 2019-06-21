{-# LANGUAGE TemplateHaskell, RecordWildCards, MultiParamTypeClasses, TypeFamilies #-}
module Concordium.GlobalState.Basic.BlockState where

import Lens.Micro.Platform
import Data.Hashable hiding (unhashed, hashed)
import Data.Time
import Data.Time.Clock.POSIX
import Control.Exception

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Block
import qualified Concordium.GlobalState.BlockState as BS
import qualified Concordium.GlobalState.Modules as Modules
import qualified Concordium.GlobalState.Account as Account
import qualified Concordium.GlobalState.Instances as Instances
import qualified Concordium.GlobalState.Rewards as Rewards
import qualified Concordium.GlobalState.IdentityProviders as IPS


data BlockState = BlockState {
    _blockAccounts :: !Account.Accounts,
    _blockInstances :: !Instances.Instances,
    _blockModules :: !Modules.Modules,
    _blockBank :: !Rewards.BankStatus,
    _blockIdentityProviders :: !IPS.IdentityProviders,
    _blockBirkParameters :: BirkParameters
}

makeLenses ''BlockState

-- |Mostly empty block state, apart from using 'Rewards.genesisBankStatus' which
-- has hard-coded initial values for amount of gtu in existence.
emptyBlockState :: BirkParameters -> BlockState
emptyBlockState _blockBirkParameters = BlockState {
  _blockAccounts = Account.emptyAccounts
  , _blockInstances = Instances.emptyInstances
  , _blockModules = Modules.emptyModules
  , _blockBank = Rewards.emptyBankStatus
  , _blockIdentityProviders = IPS.emptyIdentityProviders
  ,..
  }


data BlockPointer = BlockPointer {
    -- |Hash of the block
    _bpHash :: !BlockHash,
    -- |The block itself
    _bpBlock :: !Block,
    -- |Pointer to the parent (circular reference for genesis block)
    _bpParent :: BlockPointer,
    -- |Pointer to the last finalized block (circular for genesis)
    _bpLastFinalized :: BlockPointer,
    -- |Height of the block in the tree
    _bpHeight :: !BlockHeight,
    -- |The handle for accessing the state (of accounts, contracts, etc.) after execution of the block.
    _bpState :: !BlockState,
    -- |Time at which the block was first received
    _bpReceiveTime :: UTCTime,
    -- |Time at which the block was first considered part of the tree (validated)
    _bpArriveTime :: UTCTime,
    -- |Number of transactions in a block
    _bpTransactionCount :: Int
}

instance Eq BlockPointer where
    bp1 == bp2 = _bpHash bp1 == _bpHash bp2

instance Ord BlockPointer where
    compare bp1 bp2 = compare (_bpHash bp1) (_bpHash bp2)

instance Hashable BlockPointer where
    hashWithSalt s = hashWithSalt s . _bpHash
    hash = hash . _bpHash

instance Show BlockPointer where
    show = show . _bpHash

instance HashableTo Hash.Hash BlockPointer where
    getHash = _bpHash

instance BlockData BlockPointer where
    blockSlot = blockSlot . _bpBlock
    blockFields = blockFields . _bpBlock
    blockTransactions = blockTransactions . _bpBlock
    verifyBlockSignature key = verifyBlockSignature key . _bpBlock

-- |Make a 'BlockPointer' from a 'PendingBlock'.
-- The parent and last finalized block pointers must match the block data.
makeBlockPointer ::
    PendingBlock        -- ^Pending block
    -> BlockPointer     -- ^Parent block pointer
    -> BlockPointer     -- ^Last finalized block pointer
    -> BlockState       -- ^Block state
    -> UTCTime          -- ^Block arrival time
    -> BlockPointer
makeBlockPointer pb _bpParent _bpLastFinalized _bpState _bpArriveTime
        = assert (getHash _bpParent == blockPointer bf) $
            assert (getHash _bpLastFinalized == blockLastFinalized bf) $
                BlockPointer {
                    _bpHash = getHash pb,
                    _bpBlock = NormalBlock (pbBlock pb),
                    _bpHeight = _bpHeight _bpParent + 1,
                    _bpReceiveTime = pbReceiveTime pb,
                    _bpTransactionCount = length (blockTransactions pb),
                    ..}
    where
        bf = bbFields $ pbBlock pb


makeGenesisBlockPointer :: GenesisData -> BlockState -> BlockPointer
makeGenesisBlockPointer genData _bpState = theBlockPointer
    where
        theBlockPointer = BlockPointer {..}
        _bpBlock = makeGenesisBlock genData
        _bpHash = getHash _bpBlock
        _bpParent = theBlockPointer
        _bpLastFinalized = theBlockPointer
        _bpHeight = 0
        _bpReceiveTime = posixSecondsToUTCTime (fromIntegral (genesisTime genData))
        _bpArriveTime = _bpReceiveTime
        _bpTransactionCount = 0


instance BS.BlockPointerData BlockPointer where
    type BlockState' BlockPointer = BlockState

    bpHash = _bpHash
    bpBlock = _bpBlock
    bpParent = _bpParent
    bpLastFinalized = _bpLastFinalized
    bpHeight = _bpHeight
    bpState = _bpState
    bpReceiveTime = _bpReceiveTime
    bpArriveTime = _bpArriveTime
    bpTransactionCount = _bpTransactionCount
