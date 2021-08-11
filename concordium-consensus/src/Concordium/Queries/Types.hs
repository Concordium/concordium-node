-- |Types for representing the results of consensus queries.
module Concordium.Queries.Types where

import Concordium.Types

-- |Result of a baker status query.
data BakerStatus
    = -- |The baker is a member of the current committee
      ActiveBaker !BakerId
    | -- |The account has a baker, but it is not yet in the committee
      InactiveBaker !BakerId
    | -- |The baker id does not correspond with a current baker
      NoBaker
    | -- |The baker may exist, but the keys do not match
      BadKeys !BakerId
    deriving (Eq, Ord, Show)
