{-# LANGUAGE TemplateHaskell,
             OverloadedStrings,
             BangPatterns #-}
{-|
Module      : Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
Description : The data structure implementing account lock ups.

This module defines a data structure that stores the amounts that are locked
up for a given account.

Amounts are stored in a map sorted by timestamp of release.
-}
module Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule (
  AccountReleaseSchedule,
  emptyAccountReleaseSchedule,
  addReleases,
  unlockAmountsUntil
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Lens.Micro.Platform
import Concordium.Types
import Data.Foldable
import Data.Serialize
import Data.Aeson (ToJSON)
import qualified Data.Aeson as AE
import Concordium.Utils.Serialization
import Concordium.Types.HashableTo
import Concordium.Crypto.SHA256
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Data.List
import qualified Data.ByteString as BS

data Release = Release {
  timestamp :: !(Timestamp),
  amount :: !(Amount),
  next :: !(Maybe Release)
  } deriving (Show, Eq)

instance HashableTo Hash Release where
  getHash rel = go (put (timestamp rel) >> put (amount rel)) (next rel)
    where go partial Nothing = hash $ runPut partial
          go partial (Just r) = hash (runPut partial <> hashToByteString (getHash r))

instance Serialize Release where
  put Release{..} = do
    put timestamp
    put amount
    case next of
      Nothing -> putWord8 0
      Just v -> do
        putWord8 1
        put v
  get = do
    timestamp <- get
    amount <-  get
    hasNext <- getWord8
    next <- if hasNext == 0
            then return Nothing
            else Just <$> get
    return Release{..}

-- | Contains the amounts that are locked for a given account as well as
-- their release dates.
data AccountReleaseSchedule = AccountReleaseSchedule {
  _values :: !(Vector (Maybe (Release, TransactionHash))),
  -- | The priority queue with the locked amounts
  _pendingReleases :: !(Map Timestamp [Int]),
  -- | The total amount that is locked for this account
  _totalLockedUpBalance :: !Amount
  } deriving (Show, Eq)
makeLenses ''AccountReleaseSchedule

instance ToJSON AccountReleaseSchedule where
  toJSON AccountReleaseSchedule{..} =
    AE.object ["total" AE..= _totalLockedUpBalance,
               "schedule" AE..= map toObject (undefined $ Map.toAscList _pendingReleases)]

    where toObject :: (Timestamp, (Amount, [TransactionHash])) -> AE.Value
          toObject (timestamp, (amount, hashes)) = AE.object [
            "timestamp" AE..= timestamp,
            "amount" AE..= amount,
            "transactions" AE..= hashes
            ]

instance Serialize AccountReleaseSchedule where
  get = do
    vecLength <- get
    _values <- Vector.replicateM vecLength (do
                                               isEmpty <- getWord8
                                               if isEmpty == 0
                                                 then return Nothing
                                                 else do
                                                   item <- get
                                                   txh <- get
                                                   return $ Just (item, txh))
    let (_pendingReleases, _totalLockedUpBalance) =
          Vector.ifoldl' (\acc@(pending, am) idx -> \case
                             Nothing -> acc
                             Just (rel, _) ->
                               let go Release{..} (pending, am) =
                                     let pending' = Map.alter (maybe (Just [idx]) (Just . (idx :))) timestamp pending
                                         am' = am + amount
                                     in
                                       case next of
                                         Nothing -> (pending', am')
                                         Just n -> go n (pending', am')
                               in
                                 go rel acc) (Map.empty, 0) _values
    return AccountReleaseSchedule{..}
  put AccountReleaseSchedule{..} = do
    put $ Vector.length _values
    Vector.mapM_ (\case
                    Nothing -> putWord8 0
                    Just (rel, txh) -> do
                      putWord8 1
                      put rel
                      put txh) _values

-- λ: getHash $ addReleases ([(3,5), (4,10)], th) $ addReleases ([(1,2), (3,4)], th) emptyAccountReleaseSchedule :: Hash
-- 5473ef105c995db8d8dfe75881d8a2018bb12eaeef32032569edfff6814f1b50
-- λ: h1 = hash ((runPut $ put (1 :: Timestamp) >> put (2 :: Amount)) <> hashToByteString (hash (runPut $ put (3 :: Timestamp) >> put (4 :: Amount))))
-- λ: h2 = hash ((runPut $ put (3 :: Timestamp) >> put (5 :: Amount)) <> hashToByteString (hash (runPut $ put (4 :: Timestamp) >> put (10 :: Amount))))
-- λ: hashOfHashes h1 h2
-- 5473ef105c995db8d8dfe75881d8a2018bb12eaeef32032569edfff6814f1b50

instance HashableTo Hash AccountReleaseSchedule where
  getHash AccountReleaseSchedule{..} =
    if _totalLockedUpBalance == 0
    then hash "EmptyAccountReleaseSchedule"
    else hash $ Vector.foldl' (\prevB -> \case
                                    Nothing -> prevB
                                    Just (r, _) -> prevB <> hashToByteString (getHash r)) BS.empty _values

-- | Create an empty account release schedule
emptyAccountReleaseSchedule :: AccountReleaseSchedule
emptyAccountReleaseSchedule = AccountReleaseSchedule Vector.empty Map.empty 0

-- | Add a list of amounts to this @AccountReleaseSchedule@.
addReleases :: ([(Timestamp, Amount)], TransactionHash) -> AccountReleaseSchedule -> AccountReleaseSchedule
addReleases (l, txh) ars =
  let newIdx = Vector.length $ _values ars
      ~(Just chain, totalAmount, timestamps) = foldr (\(t, a) (next, am, ts) -> (Just $ Release t a next, am + a, t:ts)) (Nothing, 0, []) l in
    ars & values %~ flip Vector.snoc (Just (chain, txh))
        & pendingReleases %~ flip (foldl' (\m t -> Map.alter (maybe (Just [newIdx]) (Just . (newIdx :))) t m)) timestamps
        & totalLockedUpBalance +~ totalAmount

-- | Remove the amounts up to the given timestamp.
unlockAmountsUntil :: Timestamp -> AccountReleaseSchedule -> (Amount, AccountReleaseSchedule)
unlockAmountsUntil up ars =
  let (!toRemove, x, !toKeep) = Map.splitLookup up (ars ^. pendingReleases) in
    if Map.null toKeep
    then (ars ^. totalLockedUpBalance, emptyAccountReleaseSchedule)
    else
      let fullToRemove = map (\y -> (y, length y)) $ group $ sort $ concat $ maybe id (:) x $ Map.elems toRemove
          f acc@(v, am) (idx:_, numOfItems) = do
            case v Vector.! idx of
              Nothing -> acc
              Just (item, txh) ->
                let go acc 0 = acc
                    go acc@(Nothing, _) _ = acc
                    go (Just i, amnt) numOfItems =
                      let amnt' = amnt + amount i
                      in
                        go (next i, amnt') (numOfItems - 1)
                    (newItem, acumAmount) = go (Just item, 0) numOfItems
                in
                  (v Vector.// [(idx, (,txh) <$> newItem)], am + acumAmount)
          (_values', minusAmount) = foldl' f (ars ^. values, 0) fullToRemove
      in
        (minusAmount, ars & values .~ _values'
                          & pendingReleases .~ toKeep
                          & totalLockedUpBalance -~ minusAmount)
