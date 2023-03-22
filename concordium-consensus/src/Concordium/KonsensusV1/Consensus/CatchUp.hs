module Concordium.KonsensusV1.Consensus.CatchUp where

-- notes

-- We need to catch up blocks,
-- quorum certificates/messages and timeout certificates/messages
--
-- In particular if receiving a quorum/timeout message triggers catch up then
-- these messages will need to be part of the catch up.
--
-- (In an aggregated fashion qcs, tcs, qms, tms)
