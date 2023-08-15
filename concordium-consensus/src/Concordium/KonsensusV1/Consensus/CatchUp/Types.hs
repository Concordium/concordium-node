module Concordium.KonsensusV1.Consensus.CatchUp.Types where

import Data.Bits
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
      -- This may be empty.
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
        when (tsFirstEpochTimeouts == emptyFinalizerSet) $
            fail "Empty set of timeouts for first epoch."
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
      -- |The current round. For the purposes of catch-up, this should be the highest round such
      -- that the node can provide a QC or TC for the previous round.
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

-- |Flags used for serializing 'CatchUpTerminalData'.
data CatchUpTerminalDataFlags = CatchUpTerminalDataFlags
    { hasLatestFinalizationEntry :: !Bool,
      hasHighestQuorumCertificate :: !Bool,
      hasTimeoutCertificate :: !Bool
    }

instance Serialize CatchUpTerminalDataFlags where
    put CatchUpTerminalDataFlags{..} =
        putWord8
            $ oSetBit hasLatestFinalizationEntry 0
                . oSetBit hasHighestQuorumCertificate 1
                . oSetBit hasTimeoutCertificate 2
            $ 0
      where
        oSetBit True i x = setBit x i
        oSetBit False _ x = x
    get = do
        bits <- getWord8
        return $!
            CatchUpTerminalDataFlags
                { hasLatestFinalizationEntry = testBit bits 0,
                  hasHighestQuorumCertificate = testBit bits 1,
                  hasTimeoutCertificate = testBit bits 2
                }

-- |The 'CatchUpTerminalData' is sent as part of a catch-up response that concludes catch-up with
-- the peer (i.e. the peer has sent all relevant information).
--
-- Note: in some circumstances, 'cutdHighestQuorumCertificate' should not be the actual highest
-- quorum certificate available to the node. Specifically, when the timeout certificate is present,
-- it must be valid with respect to the epoch of 'cutdHighestQuorumCertificate'. This means that
-- it may be necessary to use an earlier quorum certificate in this case.
data CatchUpTerminalData = CatchUpTerminalData
    { -- |Finalization entry for the latest finalized block.
      cutdLatestFinalizationEntry :: !(Option FinalizationEntry),
      -- |Quorum certificate for the highest certified block.
      cutdHighestQuorumCertificate :: !(Option QuorumCertificate),
      -- |A timeout certificate for the last round, if available.
      cutdTimeoutCertificate :: !(Option TimeoutCertificate),
      -- |Valid quorum messages for the current round.
      cutdCurrentRoundQuorumMessages :: ![QuorumMessage],
      -- |Valid timeout messages for the current round.
      cutdCurrentRoundTimeoutMessages :: ![TimeoutMessage]
    }
    deriving (Eq, Show)

toCatchUpTerminalDataFlags :: CatchUpTerminalData -> CatchUpTerminalDataFlags
toCatchUpTerminalDataFlags CatchUpTerminalData{..} =
    CatchUpTerminalDataFlags
        { hasLatestFinalizationEntry = isPresent cutdLatestFinalizationEntry,
          hasHighestQuorumCertificate = isPresent cutdHighestQuorumCertificate,
          hasTimeoutCertificate = isPresent cutdTimeoutCertificate
        }

instance Serialize CatchUpTerminalData where
    put cutd@CatchUpTerminalData{..} = do
        put $ toCatchUpTerminalDataFlags cutd
        mapM_ put cutdLatestFinalizationEntry
        mapM_ put cutdHighestQuorumCertificate
        mapM_ put cutdTimeoutCertificate
        putListOf put cutdCurrentRoundQuorumMessages
        putListOf put cutdCurrentRoundTimeoutMessages
    get = do
        CatchUpTerminalDataFlags{..} <- get
        cutdLatestFinalizationEntry <- if hasLatestFinalizationEntry then Present <$> get else return Absent
        cutdHighestQuorumCertificate <- if hasHighestQuorumCertificate then Present <$> get else return Absent
        cutdTimeoutCertificate <- if hasTimeoutCertificate then Present <$> get else return Absent
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
        { cutdLatestFinalizationEntry = Absent,
          cutdHighestQuorumCertificate = Absent,
          cutdTimeoutCertificate = Absent,
          cutdCurrentRoundQuorumMessages = [],
          cutdCurrentRoundTimeoutMessages = []
        }
