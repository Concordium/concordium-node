{-# LANGUAGE OverloadedStrings #-}

module Concordium.Afgjort.Lottery (
    TicketProof,
    Ticket (ticketValue, ticketProof),
    proofToTicket,
    makeTicketProof,
    checkTicket,
    checkTicketProof,
) where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.Serialize as Ser

import Concordium.Afgjort.Types (VoterPower)
import qualified Concordium.Crypto.VRF as VRF

newtype TicketProof = TicketProof {theProof :: VRF.Proof} deriving (Ser.Serialize, Eq, Ord, Show)

data Ticket = Ticket
    { ticketValue :: !Double,
      ticketProof :: !TicketProof
    }
    deriving (Eq, Ord, Show)

calculateTicketValue :: VRF.Proof -> VoterPower -> VoterPower -> Double
calculateTicketValue pf weight totalWeight = (VRF.hashToDouble (VRF.proofToHash pf) + encodeFloat 1 (-53)) ** (fromIntegral totalWeight / fromIntegral weight)

proofToTicket ::
    -- |Ticket proof
    TicketProof ->
    -- |Party weight
    VoterPower ->
    -- |Total party weight
    VoterPower ->
    Ticket
proofToTicket tp@(TicketProof pf) weight totalWeight = Ticket (calculateTicketValue pf weight totalWeight) tp

-- |Generate a ticket for a lottery
makeTicketProof ::
    -- |Lottery identifier
    BS.ByteString ->
    -- |Private VRF key
    VRF.KeyPair ->
    TicketProof
makeTicketProof lotteryid key = TicketProof pf
  where
    pf = VRF.prove key ("AL" <> lotteryid)

-- |Check that a ticket is valid
checkTicket ::
    -- |Lottery identifier
    BS.ByteString ->
    -- |Party's public VRF key
    VRF.PublicKey ->
    -- |Ticket to check
    Ticket ->
    Bool
checkTicket lotteryid key Ticket{..} =
    VRF.verify key ("AL" <> lotteryid) (theProof ticketProof)

-- |Check a 'TicketProof' and generate a 'Ticket'.
checkTicketProof ::
    -- |Lottery identifier
    BS.ByteString ->
    -- |Party's public VRF key
    VRF.PublicKey ->
    -- |Ticket proof to check
    TicketProof ->
    -- |Party weight
    VoterPower ->
    -- |Total party weight
    VoterPower ->
    Maybe Ticket
checkTicketProof lotteryid key tp@TicketProof{..} weight totalWeight = do
    guard (VRF.verify key ("AL" <> lotteryid) theProof)
    return $! (proofToTicket tp weight totalWeight)
