{-# LANGUAGE TemplateHaskell,
             OverloadedStrings,
             BangPatterns #-}
{-|
Module      : Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
Description : The data structure implementing account lock ups.

This module defines a data structure that stores the amounts that are locked up
for a given account.

The structure consists of a vector and a priority queue:

* The priority queue (implemented with a Map) maps timestamps to the index in
which the schedule is stored in the vector.

* The vector keeps a list of items that are either Nothing if that schedule was completed
or Just a list of releases if that schedule is not yet completed.

Whenever a release schedule is completed, its entry in the vector will be
replaced with a Nothing. Once every entry in the vector is empty (checked
with the remaining total locked amount) it just resets the structure to an empty
structure, effectively resetting the size of the vector to 0.
-}
module Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule (
  AccountReleaseSchedule(..),
  Release(..),
  totalLockedUpBalance,
  values,
  pendingReleases,
  emptyAccountReleaseSchedule,
  AccountReleaseScheduleHash(..),
  emptyAccountReleaseScheduleHash,
  addReleases,
  unlockAmountsUntil
  ) where

import Concordium.Types
import Concordium.Types.Accounts.Releases
import Data.Foldable
import Data.Function
import Data.List (group, sort)
import qualified Data.Map as Map
import qualified Data.Vector as Vector
import Lens.Micro.Platform

------------------------------------- API --------------------------------------

-- | Create an empty account release schedule
emptyAccountReleaseSchedule :: AccountReleaseSchedule
emptyAccountReleaseSchedule = AccountReleaseSchedule Vector.empty Map.empty 0

-- | Add a list of amounts to this @AccountReleaseSchedule@.
addReleases :: ([(Timestamp, Amount)], TransactionHash) -> AccountReleaseSchedule -> AccountReleaseSchedule
addReleases (l, txh) ars =
  let newIdx = Vector.length $ _values ars
      (chain, totalAmount, timestamps) = foldr (\(t, a) ~(next, am, ts) -> (Release t a : next, am + a, t:ts)) ([], 0, []) l in
    ars & values %~ flip Vector.snoc (Just (chain, txh))
        & pendingReleases %~ flip (foldl' (\m t -> Map.alter (maybe (Just [newIdx]) (Just . (newIdx :))) t m)) timestamps
        & totalLockedUpBalance +~ totalAmount

-- | Remove the amounts up to the given timestamp.
-- It returns the unlocked amount, maybe the next smallest timestamp for this account and the new account release schedule.
unlockAmountsUntil :: Timestamp -> AccountReleaseSchedule -> (Amount, Maybe Timestamp, AccountReleaseSchedule)
unlockAmountsUntil up ars =
  let (!toRemove, x, !toKeep) = Map.splitLookup up (ars ^. pendingReleases) in
    if Map.null toKeep
    then (ars ^. totalLockedUpBalance, Nothing, emptyAccountReleaseSchedule)
    else
      let fullToRemove = map (\y -> (y, length y)) $ group $ sort $ concat $ maybe id (:) x $ Map.elems toRemove
          f _ ([], _) = error "Unreachable"
          f acc@(v, am) (idx:_, numOfItems) = do
            case v Vector.! idx of
              Nothing -> acc -- should not happen
              Just (item, txh) ->
                let (toRemove', toKeep') = splitAt numOfItems item :: ([Release], [Release])
                    acumAmount = sum $ map amount toRemove'
                in
                  if null toKeep'
                  then (v Vector.// [(idx, Nothing)], am + acumAmount)
                  else (v Vector.// [(idx, Just (toKeep', txh))], am + acumAmount)
          (_values', minusAmount) = foldl' f (ars ^. values, 0) fullToRemove
      in
        (minusAmount, fst <$> Map.lookupMin toKeep, ars & values .~ _values'
                                                        & pendingReleases .~ toKeep
                                                        & totalLockedUpBalance -~ minusAmount)
