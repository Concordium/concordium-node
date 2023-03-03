{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus where

import Control.Monad.Reader
import Control.Monad.State

import Data.Ratio
import Data.Word
import Lens.Micro.Platform

import Concordium.KonsensusV1.TreeState.Implementation
import qualified Concordium.KonsensusV1.TreeState.LowLevel as LowLevel
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

import Concordium.GlobalState.BlockState
import Concordium.GlobalState.Persistent.BlockState
import Concordium.GlobalState.Types
import Concordium.Types
import Concordium.Types.BakerIdentity
import Concordium.Types.Parameters hiding (getChainParameters)

-- |A Monad for multicasting timeout messages.
class MonadMulticast m where
    -- |Multicast a timeout message over the network
    sendTimeoutMessage :: TimeoutMessage -> m ()

-- |A baker context containing the baker identity. Used for accessing relevant baker keys and the baker id.
newtype BakerContext = BakerContext
    { _bakerIdentity :: BakerIdentity
    }
makeClassy ''BakerContext

-- |Get the private baker aggregation key.
getBakerAggSecretKey :: (MonadReader r m, HasBakerContext r) => m BakerAggregationPrivateKey
getBakerAggSecretKey = do
    bi <- view bakerIdentity
    return $ bakerAggregationKey bi

-- |Get the private baker sign key.
getBakerSignPrivateKey :: (MonadReader r m, HasBakerContext r) => m BakerSignPrivateKey
getBakerSignPrivateKey = do
    bi <- view bakerIdentity
    return $ bakerSignKey bi

-- |Get the baker id.
getBakerId :: (MonadReader r m, HasBakerContext r) => m BakerId
getBakerId = do
    bi <- view bakerIdentity
    return $ bakerId bi


updateRoundStatus :: Ratio Word64 -> RoundStatus -> RoundStatus
updateRoundStatus timeoutIncrease currentRoundStatus =
    let newNextSignableRound = 1 + rsCurrentRound currentRoundStatus
        timeoutIncreaseRational = toRational timeoutIncrease :: Rational
        currentTimeOutRational = toRational $ rsCurrentTimeout currentRoundStatus :: Rational
        newCurrentTimeoutRational = timeoutIncreaseRational * currentTimeOutRational :: Rational
        newCurrentTimeoutInteger = floor newCurrentTimeoutRational :: Integer
        newCurrentTimeout = Duration $ fromIntegral newCurrentTimeoutInteger
    in currentRoundStatus{
        rsNextSignableRound = newNextSignableRound,
        rsCurrentTimeout = newCurrentTimeout}

-- |This is 'uponTimeoutEvent' from the bluepaper. If a timeout occurs, a finalizers should call this function to
-- generate and send out a timeout message.
uponTimeoutEvent ::
    ( MonadMulticast m,
      MonadReader r m,
      HasBakerContext r,
      BlockStateQuery m,
      BlockState m ~ HashedPersistentBlockState (MPV m),
      IsConsensusV1 (MPV m),
      MonadState (SkovData (MPV m)) m,
      LowLevel.MonadTreeStateStore m
    ) =>
    m ()
uponTimeoutEvent = do
    bakerId <- getBakerId
    finComm <- (^. currentEpochBakers . bfFinalizers) <$> use skovEpochBakers
    let maybeFinalizer = finalizerByBakerId finComm bakerId

    case maybeFinalizer of
        Nothing -> return ()
        Just finInfo -> do
            currentRoundStatus <- doGetRoundStatus
            lastFinBlockPtr <- use lastFinalized
            gc <- use genesisMetadata
            let genesisHash = gmFirstGenesisHash gc
            cp <- getChainParameters $ bpState lastFinBlockPtr
            let timeoutIncrease = cp ^. cpConsensusParameters . cpTimeoutParameters . tpTimeoutIncrease
            doSetRoundStatus $ updateRoundStatus timeoutIncrease currentRoundStatus

            let highestQC = rsHighestQC currentRoundStatus

            let timeoutSigMessage =
                    TimeoutSignatureMessage
                        { tsmGenesis = genesisHash,
                        tsmRound = rsCurrentRound currentRoundStatus,
                        tsmQCRound = qcRound highestQC,
                        tsmQCEpoch = qcEpoch highestQC
                        }
            bakerAggSk <- getBakerAggSecretKey
            let timeoutSig = signTimeoutSignatureMessage timeoutSigMessage bakerAggSk


            let timeoutMessageBody =
                    TimeoutMessageBody
                        {
                        tmFinalizerIndex = finalizerIndex finInfo,
                        tmRound = rsCurrentRound currentRoundStatus,
                        tmQuorumCertificate = highestQC,
                        tmAggregateSignature = timeoutSig
                        }
            bakerSignSk <- getBakerSignPrivateKey
            let timeoutMessage = signTimeoutMessage timeoutMessageBody genesisHash bakerSignSk
            sendTimeoutMessage timeoutMessage
            processTimeout timeoutMessage

-- |This is 'processTimeout' from the bluepaper. FIXME: add more documentation when spefication is ready.
processTimeout :: Monad m => TimeoutMessage -> m ()
processTimeout _ = return () -- FIXME: implement this when specification is ready
