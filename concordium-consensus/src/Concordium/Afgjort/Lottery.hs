{-# LANGUAGE RecordWildCards, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Concordium.Afgjort.Lottery(
    TicketProof,
    Ticket(ticketValue, ticketProof),
    proofToTicket,
    makeTicketProof,
    checkTicket,
    checkTicketProof
) where

import qualified Data.Serialize as Ser
import qualified Data.ByteString as BS
import Control.Monad

import qualified Concordium.Crypto.VRF as VRF
import Concordium.Afgjort.Types (VoterPower)

newtype TicketProof = TicketProof {theProof :: VRF.Proof} deriving (Ser.Serialize,Eq,Ord,Show)

data Ticket = Ticket {
    ticketValue :: Double,
    ticketProof :: TicketProof
} deriving (Eq, Ord, Show)

calculateTicketValue :: VRF.Proof -> VoterPower -> VoterPower -> Double
calculateTicketValue pf weight totalWeight = (VRF.hashToDouble (VRF.proofToHash pf) + encodeFloat 1 (-53)) ** (fromIntegral totalWeight / fromIntegral weight)

proofToTicket :: TicketProof    -- ^Ticket proof
        -> VoterPower                  -- ^Party weight
        -> VoterPower                  -- ^Total party weight
        -> Ticket
proofToTicket tp@(TicketProof pf) weight totalWeight = Ticket (calculateTicketValue pf weight totalWeight) tp

-- |Generate a ticket for a lottery
makeTicketProof :: BS.ByteString -- ^Lottery identifier
            -> VRF.KeyPair   -- ^Private VRF key
            -> IO TicketProof
makeTicketProof lotteryid key = TicketProof <$> pf
    where
        pf = VRF.prove key ("AL" <> lotteryid)

-- |Check that a ticket is valid
checkTicket :: BS.ByteString    -- ^Lottery identifier
        -> VRF.PublicKey        -- ^Party's public VRF key
        -> Ticket               -- ^Ticket to check
        -> Bool
checkTicket lotteryid key Ticket{..} =
        VRF.verify key ("AL" <> lotteryid) (theProof ticketProof)

-- |Check a 'TicketProof' and generate a 'Ticket'.
checkTicketProof :: BS.ByteString    -- ^Lottery identifier
        -> VRF.PublicKey        -- ^Party's public VRF key
        -> TicketProof               -- ^Ticket proof to check
        -> VoterPower                  -- ^Party weight
        -> VoterPower                  -- ^Total party weight
        -> Maybe Ticket
checkTicketProof lotteryid key tp@TicketProof{..} weight totalWeight = do
        guard (VRF.verify key ("AL" <> lotteryid) theProof)
        return (proofToTicket tp weight totalWeight)