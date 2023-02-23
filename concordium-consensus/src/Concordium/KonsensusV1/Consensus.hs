{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Concordium.KonsensusV1.Consensus where

import Control.Monad.Reader
import Control.Monad.State
import qualified Data.ByteString as BS

-- import Data.Ratio
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

-- import Concordium.Types

class MonadMulticast m where
    sendTimeoutMessage :: TimeoutMessage -> m ()

data BakerContext = BakerContext
    { _bakerIdentity :: BakerIdentity
    }
makeClassy ''BakerContext

getBakerAggSecretKey :: (MonadReader r m, HasBakerContext r) => m BakerAggregationPrivateKey
getBakerAggSecretKey = do
    bi <- asks (view bakerIdentity)
    return $ bakerAggregationKey bi

getBakerSignPrivateKey :: (MonadReader r m, HasBakerContext r) => m BakerSignPrivateKey
getBakerSignPrivateKey = do
    bi <- asks (view bakerIdentity)
    return $ bakerSignKey bi

getBakerId :: (MonadReader r m, HasBakerContext r) => m BakerId
getBakerId = do
    bi <- asks (view bakerIdentity)
    return $ bakerId bi

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
            let newNextSignableRound = 1 + rsCurrentRound currentRoundStatus
            lastFinBlockPtr <- use lastFinalized
            gc <- use genesisConfiguration
            let genesisHash = gcFirstGenesisHash gc
            cp <- getChainParameters $ bpState lastFinBlockPtr
            let timeoutIncrease = cp ^. cpConsensusParameters . cpTimeoutParameters . tpTimeoutIncrease
            let timeoutIncreaseRational = toRational timeoutIncrease :: Rational
            let currentTimeOutRational = toRational $ rsCurrentTimeout currentRoundStatus :: Rational
            let newCurrentTimeoutRational = timeoutIncreaseRational * currentTimeOutRational :: Rational
            let newCurrentTimeoutInteger = floor newCurrentTimeoutRational :: Integer
            let newCurrentTimeout = Duration $ fromIntegral newCurrentTimeoutInteger

            let newRoundStatus = currentRoundStatus{
                    rsNextSignableRound = newNextSignableRound,
                    rsCurrentTimeout = newCurrentTimeout}
            doSetRoundStatus newRoundStatus

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

processTimeout :: Monad m => TimeoutMessage -> m ()
processTimeout _ = return () -- FIXME: implement this when specification is ready
