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

import Concordium.Crypto.SHA256
import Concordium.Types
import Concordium.Types.HashableTo
import Data.Aeson (ToJSON)
import qualified Data.Aeson as AE
import qualified Data.ByteString as BS
import Data.Foldable
import Data.Function
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Serialize
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Lens.Micro.Platform
import Concordium.Utils.Serialization

----------------------------------- Release ------------------------------------

-- | A Release represents a moment at which the amount should be released.
-- A TransferWithSchedule transaction generates a list of these Releases.
data Release = Release {
  timestamp :: !Timestamp -- ^ The moment at which the amount is considered unlocked
  , amount :: !Amount -- ^ The amount to unlock
  } deriving (Show, Eq)

instance Serialize Release where
  put Release{..} = do
    put timestamp
    put amount
  get = do
    timestamp <- get
    amount <-  get
    return Release{..}

-- | Generate the hash of a list of releases.
-- PRECONDITION: this should never be called with an empty list of releases
getHashOfReleases :: [Release] -> Hash
getHashOfReleases [] = error "Unreachable"
getHashOfReleases (x:[]) = hash $ encode x
getHashOfReleases (x:xs) =
  let xSerialized = encode x
      hashOfNext = getHashOfReleases xs
  in
    hash (xSerialized <> hashToByteString hashOfNext)

--------------------------- Account release schedule ---------------------------

-- | Contains the amounts that are locked for a given account as well as
-- their release dates.
data AccountReleaseSchedule = AccountReleaseSchedule {
  -- | The vector of current releases. When a schedule has been fully released its
  -- entry in this vector is replaced by a Nothing and will eventually be removed when
  -- all the current Schedules are released.
  _values :: !(Vector (Maybe ([Release], TransactionHash))),
  -- | The priority queue with indices to the vector items on each timestamp.
  _pendingReleases :: !(Map Timestamp [Int]),
  -- | The total amount that is locked for this account
  _totalLockedUpBalance :: !Amount
  } deriving (Show, Eq)
makeLenses ''AccountReleaseSchedule

instance ToJSON AccountReleaseSchedule where
  toJSON AccountReleaseSchedule{..} =
    let listOfReleasesByTimestamp = [ (tm, (a, t)) | Just (r, t) <- Vector.toList _values, Release tm a <- r ]
        sortedAndGroupedByTimestamp = groupBy ((==) `on` fst) $ sortOn fst listOfReleasesByTimestamp
        -- combine the elements for each timestamp by summing up the amounts and creating a list with the transaction hashes
        -- @head@ is safe because group won't create empty lists.
        schedule = map (\x -> (fst (head x), foldl' (\(accB, accT) (_, (b, t)) -> (accB + b, t : accT)) (0, []) x)) sortedAndGroupedByTimestamp
    in
      AE.object ["total" AE..= _totalLockedUpBalance,
                 "schedule" AE..= map toObject schedule]


    where toObject :: (Timestamp, (Amount, [TransactionHash])) -> AE.Value
          toObject (timestamp, (amount, hashes)) = AE.object [
            "timestamp" AE..= timestamp,
            "amount" AE..= amount,
            "transactions" AE..= hashes
            ]

instance Serialize AccountReleaseSchedule where
  get = do
    vecLength <- getLength
    _values <- Vector.replicateM vecLength (getMaybe (do
                                                         l <- getLength
                                                         item <- sequence $ replicate l get
                                                         txh <- get
                                                         return (item, txh)))
    let (_pendingReleases, _totalLockedUpBalance) =
          Vector.ifoldl' (\acc idx -> \case
                             Nothing -> acc
                             Just (rel, _) ->
                               let f (pending, am) Release{..} =
                                     let pending' = Map.alter (maybe (Just [idx]) (Just . (idx :))) timestamp pending
                                         am' = am + amount
                                     in (pending', am')
                               in
                                 foldl' f acc rel) (Map.empty, 0) _values
    return AccountReleaseSchedule{..}
  put AccountReleaseSchedule{..} = do
    putLength $ Vector.length _values
    Vector.mapM_ (putMaybe (\(rel, txh) -> do
                               putLength $ length rel
                               mapM_ put rel
                               put txh)
                 ) _values

-- λ: getHash $ addReleases ([(3,5), (4,10)], th) $ addReleases ([(1,2), (3,4)], th) emptyAccountReleaseSchedule :: Hash
-- 5473ef105c995db8d8dfe75881d8a2018bb12eaeef32032569edfff6814f1b50
-- λ: h1 = hash ((runPut $ put (1 :: Timestamp) >> put (2 :: Amount)) <> hashToByteString (hash (runPut $ put (3 :: Timestamp) >> put (4 :: Amount))))
-- λ: h2 = hash ((runPut $ put (3 :: Timestamp) >> put (5 :: Amount)) <> hashToByteString (hash (runPut $ put (4 :: Timestamp) >> put (10 :: Amount))))
-- λ: hashOfHashes h1 h2
-- 5473ef105c995db8d8dfe75881d8a2018bb12eaeef32032569edfff6814f1b50

newtype AccountReleaseScheduleHash = AccountReleaseScheduleHash Hash
 deriving (Serialize)

emptyAccountReleaseScheduleHash :: Hash
emptyAccountReleaseScheduleHash = hash "EmptyAccountReleaseSchedule"

instance HashableTo Hash AccountReleaseSchedule where
  getHash AccountReleaseSchedule{..} =
    if _totalLockedUpBalance == 0
    then emptyAccountReleaseScheduleHash
    else hash $ Vector.foldl' (\prevB -> \case
                                    Nothing -> prevB
                                    Just (r, _) -> prevB <> hashToByteString (getHashOfReleases r)) BS.empty _values

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
