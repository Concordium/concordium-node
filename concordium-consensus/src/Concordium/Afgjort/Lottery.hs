{-# LANGUAGE RecordWildCards, OverloadedStrings, GeneralizedNewtypeDeriving #-}
module Concordium.Afgjort.Lottery(
    TicketProof,
    Ticket(ticketValue),
    proofToTicket,
    makeTicketProof,
    checkTicket
) where

import qualified Data.Serialize as Ser
import qualified Data.ByteString as BS

import qualified Concordium.Crypto.VRF as VRF

newtype TicketProof = TicketProof VRF.Proof deriving (Ser.Serialize,Eq,Ord,Show)

data Ticket = Ticket {
    ticketValue :: Double,
    ticketProof :: VRF.Proof
} deriving (Eq, Ord, Show)

calculateTicketValue :: VRF.Proof -> Int -> Int -> Double
calculateTicketValue pf weight totalWeight = (VRF.hashToDouble (VRF.proofToHash pf) + encodeFloat 1 (-53)) ** (fromIntegral totalWeight / fromIntegral weight)

proofToTicket :: TicketProof    -- ^Ticket proof
        -> Int                  -- ^Party weight
        -> Int                  -- ^Total party weight
        -> Ticket
proofToTicket (TicketProof pf) weight totalWeight = Ticket (calculateTicketValue pf weight totalWeight) pf

-- |Generate a ticket for a lottery
makeTicketProof :: BS.ByteString -- ^Lottery identifier
            -> VRF.KeyPair   -- ^Private VRF key
            -> TicketProof
makeTicketProof lotteryid key = TicketProof pf
    where
        pf = VRF.prove key ("AL" <> lotteryid)

-- |Check that a ticket is valid
checkTicket :: BS.ByteString    -- ^Lottery identifier
        -> VRF.PublicKey        -- ^Party's public VRF key
        -> Ticket               -- ^Ticket to check
        -> Bool
checkTicket lotteryid key Ticket{..} =
        VRF.verifyKey key && -- TODO: possibly this is not necessary
        VRF.verify key ("AL" <> lotteryid) ticketProof
