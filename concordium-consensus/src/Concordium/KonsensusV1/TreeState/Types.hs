{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}

module Concordium.KonsensusV1.TreeState.Types where

import Data.Serialize
import Data.Time
import Data.Time.Clock.POSIX

import Concordium.KonsensusV1.Types
import Concordium.Types

-- |Metadata about a block that has been executed.
data BlockMetadata = BlockMetadata
    { bmHeight :: !BlockHeight,
      bmReceiveTime :: !UTCTime,
      bmArriveTime :: !UTCTime
    }

instance Serialize BlockMetadata where
    put BlockMetadata{..} = do
        put bmHeight
        putUTCPOSIXMicros bmReceiveTime
        putUTCPOSIXMicros bmArriveTime
      where
        putUTCPOSIXMicros = putWord64be . floor . (1_000_000 *) . utcTimeToPOSIXSeconds
    get = do
        bmHeight <- get
        bmReceiveTime <- getUTCPOSIXMicros
        bmArriveTime <- getUTCPOSIXMicros
        return BlockMetadata{..}
      where
        getUTCPOSIXMicros = posixSecondsToUTCTime . (/ 1_000_000) . realToFrac <$> getWord64be

-- |The current round status.
-- Note that it can be the case that both the 'QuorumSignatureMessage' and the
-- 'TimeoutSignatureMessage' are present.
-- This is the case if the consensus runner has first signed a block
-- but not enough quorum signature messages were retrieved before timeout.
data RoundStatus = RoundStatus
    { -- |The highest 'Epoch' that the consensus runner participated in.
      rsCurrentEpoch :: !Epoch,
      -- |The highest 'Round' that the consensus runner participated in.
      rsCurrentRound :: !Round,
      -- |If the consensus runner is part of the finalization committee,
      -- then this will yield the last signed 'QuorumSignatureMessage'
      rsLastSignedQuouromSignatureMessage :: !(Maybe QuorumSignatureMessage),
      -- |If the consensus runner is part of the finalization committee,
      -- then this will yield the last signed timeout message.
      rsLastSignedTimeoutSignatureMessage :: !(Maybe TimeoutSignatureMessage),
      -- |The current timeout.
      rsCurrentTimeout :: !Duration,
      -- |The highest 'QuorumCertificate' seen so far.
      -- This is 'Nothing' if no rounds since genesis has
      -- been able to produce a 'QuorumCertificate'.
      rsHighestQC :: !(Maybe QuorumCertificate),
      -- |The current 'LeadershipElectionNonce'.
      rsLeadershipElectionNonce :: !LeadershipElectionNonce,
      -- |The latest 'Epoch' 'FinalizationEntry'.
      -- This will only be 'Nothing' in between the
      -- genesis block and the first explicitly finalized block.
      rsLatestEpochFinEntry :: !(Maybe FinalizationEntry),
      -- |The previous round timeout certificate if the previous round timed out.
      -- This is @Just (TimeoutCertificate, QuorumCertificate)@ if the previous round timed out or otherwise 'Nothing'.
      -- In the case of @Just@ then the associated 'QuorumCertificate' is the highest 'QuorumCertificate' at the time
      -- that the 'TimeoutCertificate' was built.
      rsPreviousRoundTC :: !(Maybe (TimeoutCertificate, QuorumCertificate))
    }

instance Serialize RoundStatus where
    put RoundStatus{..} = do
        put rsCurrentEpoch
        put rsCurrentRound
        put rsLastSignedQuouromSignatureMessage
        put rsLastSignedTimeoutSignatureMessage
        put rsCurrentTimeout
        put rsHighestQC
        put rsLeadershipElectionNonce
        put rsLatestEpochFinEntry
        put rsPreviousRoundTC
    get = do
        rsCurrentEpoch <- get
        rsCurrentRound <- get
        rsLastSignedQuouromSignatureMessage <- get
        rsLastSignedTimeoutSignatureMessage <- get
        rsCurrentTimeout <- get
        rsHighestQC <- get
        rsLeadershipElectionNonce <- get
        rsLatestEpochFinEntry <- get
        rsPreviousRoundTC <- get
        return RoundStatus{..}

-- |The 'RoundStatus' for consensus at genesis.
initialRoundStatus :: Duration -> LeadershipElectionNonce -> RoundStatus
initialRoundStatus baseTimeout leNonce =
    RoundStatus
        { rsCurrentEpoch = 0,
          rsCurrentRound = 0,
          rsLastSignedQuouromSignatureMessage = Nothing,
          rsLastSignedTimeoutSignatureMessage = Nothing,
          rsCurrentTimeout = baseTimeout,
          rsHighestQC = Nothing,
          rsLeadershipElectionNonce = leNonce,
          rsLatestEpochFinEntry = Nothing,
          rsPreviousRoundTC = Nothing
        }
