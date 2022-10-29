module Concordium.GlobalState.Basic.BlockState.ReleaseSchedule where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Concordium.Types

data ReleaseSchedule = ReleaseSchedule
    { -- |This is the minimal timestamp for which a release is scheduled, or @Timestamp maxBound@
      -- if there is no scheduled release. This MUST NOT be used to infer that there are no
      -- scheduled releases, since a release could be scheduled at the maximum timestamp.
      rsFirstTimestamp :: !Timestamp,
      -- |A map recording the first release time for each account with a pending release.
      -- An account should occur at most once in the map.
      rsMap :: !(Map.Map Timestamp (Set.Set AccountIndex))
    }
    deriving (Show)

-- |A 'ReleaseSchedule' with no pending releases.
emptyReleaseSchedule :: ReleaseSchedule
emptyReleaseSchedule = ReleaseSchedule (Timestamp maxBound) Map.empty

-- |Add a release for a given account.
-- PRECONDITION: The account must not already have a scheduled release.
addAccountRelease :: Timestamp -> AccountIndex -> ReleaseSchedule -> ReleaseSchedule
addAccountRelease ts ai ReleaseSchedule{..} =
    ReleaseSchedule
        { rsFirstTimestamp = min ts rsFirstTimestamp,
          rsMap = Map.alter addAcc ts rsMap
        }
  where
    addAcc Nothing = Just $! Set.singleton ai
    addAcc (Just accs) = Just $! Set.insert ai accs

-- |Update the scheduled release time for an account.
-- PRECONDITION: The account must have a scheduled release at the old release time.
-- PRECONDITION: @oldReleaseTime >= newReleaseTime@.
updateAccountRelease ::
    -- |Old release time
    Timestamp ->
    -- |New release time
    Timestamp ->
    AccountIndex ->
    ReleaseSchedule ->
    ReleaseSchedule
updateAccountRelease oldReleaseTime newReleaseTime ai ReleaseSchedule{..} =
    ReleaseSchedule
        { rsFirstTimestamp = min newReleaseTime rsFirstTimestamp,
          rsMap = Map.alter addAcc newReleaseTime (Map.alter remAcc oldReleaseTime rsMap)
        }
  where
    addAcc Nothing = Just $! Set.singleton ai
    addAcc (Just accs) = Just $! Set.insert ai accs
    remAcc Nothing = Nothing
    remAcc (Just accs) =
        let s' = Set.delete ai accs
         in if Set.null s' then Nothing else Just s'

-- |Remove all releases from the schedule up to and including the provided timestamp,
-- returning a list of the accounts with removed releases.
processReleasesUntil :: Timestamp -> ReleaseSchedule -> ([AccountIndex], ReleaseSchedule)
processReleasesUntil ts rs0
    | ts < rsFirstTimestamp rs0 = ([], rs0)
    | otherwise = go [] rs0
  where
    go accum rs@ReleaseSchedule{..} = case Map.minViewWithKey rsMap of
        Nothing -> (accum, ReleaseSchedule (Timestamp maxBound) Map.empty)
        Just ((minTS, accs), newMap)
            | minTS <= ts -> go (accum ++ Set.toList accs) (rs{rsMap = newMap})
            | otherwise -> (accum, rs{rsFirstTimestamp = minTS})