{-# LANGUAGE TemplateHaskell, RecordWildCards #-}
module Concordium.Afgjort.Finalize.Summary where

import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.HashMap.Strict as HM
import Lens.Micro.Platform

import Concordium.Crypto.BlockSignature (Signature)
import Concordium.Afgjort.Types
import Concordium.Afgjort.Finalize.Types
import Concordium.Afgjort.CSS.BitSet
import Concordium.Afgjort.CSS.NominationSet

{-
data BlockMessages = BlockMessages {
    _bmProposals :: !(Map Party Signature),
    _bmVotes :: !(Map Party Signature),
    _bmWitnessCreators :: !(Map Party Signature)
}
makeLenses ''BlockMessages

emptyBlockMessages :: BlockMessages
emptyBlockMessages = BlockMessages Map.empty Map.empty Map.empty

data CSSMessages = CSSMessages {
    _cmJustifiedTop :: !(Map Party Signature),
    _cmJustifiedBot :: !(Map Party Signature),
    _cmSeen :: !(Map Party [(NominationSet, Signature)]),
    _cmDoneReporting :: !(Map Party [(NominationSet, Signature)])
}
makeLenses ''CSSMessages

data RoundSummary = RoundSummary {
    _rsBlockMessages :: !(HM.HashMap BlockHash BlockMessages),
    _rsBottomVotes :: !(Map Party Signature),
    _rsCSSMessages :: !(Map Phase CSSMessages),
    _rsWeAreDoneTopMessages :: !(Map Party Signature),
    _rsWeAreDoneTopWeight :: VoterPower,
    _rsWeAreDoneBotMessages :: !(Map Party Signature),
    _rsWeAreDoneBotWeight :: VoterPower
}
makeLenses ''RoundSummary

data FinalizationSummary = FinalizationSummary {
    fsRoundSummaries :: !(Map.Map BlockHeight RoundSummary)
}
--
emptyRoundSummary :: RoundSummary
emptyRoundSummary = RoundSummary HM.empty Map.empty Map.empty Map.empty 0 Map.empty 0

emptyFinalizationSummary :: FinalizationSummary
emptyFinalizationSummary = FinalizationSummary Map.empty

addRoundMessage :: FinalizationCommittee -> FinalizationMessage -> RoundSummary -> RoundSummary
addRoundMessage finCom FinalizationMessage{..} rs0 = unlessDoneBot $ case msgBody of
        WMVBAFreezeMessage fmsg = case fmsg of
            -- 
            Proposal v -> rs0 & rsBlockMessages . at v . non emptyBlockMessages . bmProposals . at sender ?~ msgSignature
            Vote (Just v) -> rs0 & rsBlockMessages . at v . non emptyBlockMessages . bmVotes . at sender ?~ msgSignature
            Vote Nothing -> rs & rsBlockMessages . rsBottomVotes . at sender ?~ msgSignature


    where
        honestWeight = totalWeight finCom - corruptWeight finCom
        cssDoneBot = rsWeAreDoneBotWeight rs0 >= honestWeight
        cssDone = cssDoneBot || (rsWeAreDoneTopWeight rs0 >= honestWeight)
        unlessDoneBot r = if cssDoneBot then rs0 else r
        unlessDone r = if cssDone then rs0 else r
        sender = msgSenderIndex msgHeader

addMessage :: FinalizationCommittee -> FinalizationMessage -> FinalizationSummary -> FinalizationSummary
addMessage finCom
-}