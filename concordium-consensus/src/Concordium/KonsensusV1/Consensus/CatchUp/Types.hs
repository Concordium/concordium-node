module Concordium.KonsensusV1.Consensus.CatchUp.Types where

import qualified Data.Map.Strict as Map
import Data.Serialize hiding (getListOf, putListOf)

import Concordium.KonsensusV1.Types
import Concordium.Types
import Concordium.Utils.Serialization
import Control.Monad

-- |A summary of timeout messages received for a particular round.
data TimeoutSet = TimeoutSet
    { -- |The first epoch for which we have timeout signatures for the round.
      tsFirstEpoch :: !Epoch,
      -- |The set of finalizers for which we have timeout signatures in epoch 'tsFirstEpoch'.
      -- This must be non-empty.
      tsFirstEpochTimeouts :: !FinalizerSet,
      -- |The set of finalizers for which we have timeout signatures in epoch @tsFirstEpoch + 1@.
      tsSecondEpochTimeouts :: !FinalizerSet
    }
    deriving (Eq, Show)

instance Serialize TimeoutSet where
    put TimeoutSet{..} = do
        put tsFirstEpoch
        put tsFirstEpochTimeouts
        put tsSecondEpochTimeouts
    get = do
        tsFirstEpoch <- get
        tsFirstEpochTimeouts <- get
        tsSecondEpochTimeouts <- get
        return TimeoutSet{..}

-- |Status of the consensus used for determining whether and how to catch-up.
data CatchUpStatus = CatchUpStatus
    { -- |The node's last finalized block hash.
      cusLastFinalizedBlock :: BlockHash,
      -- |Round of the node's last finalized block.
      cusLastFinalizedRound :: Round,
      -- |Hashes of the leaves of the node's block tree.
      cusLeaves :: [BlockHash],
      -- |Hashes of the blocks on branches beyond the last finalized block.
      -- This should only be included in requests, and otherwise should be the empty list.
      cusBranches :: [BlockHash],
      -- |The current round.
      cusCurrentRound :: Round,
      -- |The valid quorum signatures on blocks in the current round.
      cusCurrentRoundQuorum :: Map.Map BlockHash FinalizerSet,
      -- |The valid timeout messages in the current round.
      cusCurrentRoundTimeouts :: Option TimeoutSet
    }
    deriving (Eq, Show)

-- |Serialize a 'CatchUpStatus'.
putCatchUpStatus ::
    -- |Whether to include branches
    Bool ->
    Putter CatchUpStatus
putCatchUpStatus includeBranches CatchUpStatus{..} = do
    put cusLastFinalizedBlock
    put cusLastFinalizedRound
    putListOf put cusLeaves
    when includeBranches $ putListOf put cusBranches
    put cusCurrentRound
    putSafeMapOf put put cusCurrentRoundQuorum
    putOptionOf put cusCurrentRoundTimeouts

-- |Deserialize a 'CatchUpStatus'.
getCatchUpStatus ::
    -- |Whether to include branches
    Bool ->
    Get CatchUpStatus
getCatchUpStatus includeBranches = do
    cusLastFinalizedBlock <- get
    cusLastFinalizedRound <- get
    cusLeaves <- getListOf get
    cusBranches <-
        if includeBranches
            then getListOf get
            else return []
    cusCurrentRound <- get
    cusCurrentRoundQuorum <- getSafeMapOf get get
    cusCurrentRoundTimeouts <- getOptionOf get
    return CatchUpStatus{..}

-- |The 'CatchUpTerminalData' is sent as part of a catch-up response that concludes catch-up with
-- the peer (i.e. the peer has sent all relevant information).
--
-- If the peer is not otherwise aware of them, 'cutdQuorumCertificates' should include QCs on:
--    * The block in the round after the last finalized block (if the peer does not consider it
--      finalized already).
--    * The highest certified block (if for a later round than the peer's highest certified block).
data CatchUpTerminalData = CatchUpTerminalData
    { -- |Quorum certificates to ensure agreement on the last finalized block and highest certified
      -- block.
      cutdQuorumCertificates :: ![QuorumCertificate],
      -- |A timeout certificate for the last round, if available.
      cutdTimeoutCertificate :: !(Option TimeoutCertificate),
      -- |Valid quorum messages for the current round.
      -- TODO: Repackage all quorum messages for the same block together.
      cutdCurrentRoundQuorumMessages :: ![QuorumMessage],
      -- |Valid timeout messages for the current round.
      -- TODO: Repackage timeout messages together.
      cutdCurrentRoundTimeoutMessages :: ![TimeoutMessage]
    }
    deriving (Eq, Show)

instance Serialize CatchUpTerminalData where
    put CatchUpTerminalData{..} = do
        putListOf put cutdQuorumCertificates
        putOptionOf put cutdTimeoutCertificate
        putListOf put cutdCurrentRoundQuorumMessages
        putListOf put cutdCurrentRoundTimeoutMessages
    get = do
        cutdQuorumCertificates <- getListOf get
        cutdTimeoutCertificate <- getOptionOf get
        cutdCurrentRoundQuorumMessages <- getListOf get
        cutdCurrentRoundTimeoutMessages <- getListOf get
        return CatchUpTerminalData{..}

-- |A catch-up message that is sent between peers.
data CatchUpMessage
    = -- |The message is neither a request nor response, but used to notify peers that they may
      -- need to catch up.
      CatchUpStatusMessage
        { -- |Catch-up status. Should not include branches.
          cumStatus :: CatchUpStatus
        }
    | -- |The message is a request for the peer to send blocks and data to catch-up.
      CatchUpRequestMessage
        { -- |Catch-up status. Should include branches.
          cumStatus :: CatchUpStatus
        }
    | -- |The message is a response to a previous catch-up request.
      CatchUpResponseMessage
        { -- |Catch-up status. Should not include branches.
          cumStatus :: CatchUpStatus,
          -- |Terminal data, included if the catch-up was completed.
          cumTerminalData :: Option CatchUpTerminalData
        }
    deriving (Show)

instance Serialize CatchUpMessage where
    put CatchUpStatusMessage{..} = do
        putWord8 0
        putCatchUpStatus False cumStatus
    put CatchUpRequestMessage{..} = do
        putWord8 1
        putCatchUpStatus True cumStatus
    put CatchUpResponseMessage{cumTerminalData = Absent, ..} = do
        putWord8 2
        putCatchUpStatus False cumStatus
    put CatchUpResponseMessage{cumTerminalData = Present td, ..} = do
        putWord8 3
        putCatchUpStatus False cumStatus
        put td
    get = do
        getWord8 >>= \case
            0 -> CatchUpStatusMessage <$> getCatchUpStatus False
            1 -> CatchUpRequestMessage <$> getCatchUpStatus True
            2 -> CatchUpResponseMessage <$> getCatchUpStatus False <*> return Absent
            3 -> do
                cumStatus <- getCatchUpStatus False
                cumTerminalData <- Present <$> get
                return CatchUpResponseMessage{..}
            _ -> fail "Invalid CatchUpMessage tag"

-- |'CatchUpTerminalData' with no contents. This can be sent as part of a catch-up response when
-- the node knows that none of its data would be relevant for the peer.
emptyCatchUpTerminalData :: CatchUpTerminalData
emptyCatchUpTerminalData =
    CatchUpTerminalData
        { cutdQuorumCertificates = [],
          cutdTimeoutCertificate = Absent,
          cutdCurrentRoundQuorumMessages = [],
          cutdCurrentRoundTimeoutMessages = []
        }
