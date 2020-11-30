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
  AccountReleaseSchedule(..),
  Release(..),
  totalLockedUpBalance,
  values,
  emptyAccountReleaseSchedule,
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

----------------------------------- Release ------------------------------------

data Release = Release {
  timestamp :: !(Timestamp),
  amount :: !(Amount)
  } deriving (Show, Eq)

instance Serialize Release where
  put Release{..} = do
    put timestamp
    put amount
  get = do
    timestamp <- get
    amount <-  get
    return Release{..}

getHashOfReleases :: [Release] -> Hash
-- The code shall never try to get the hash of an empty list of releases
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
  -- | The vector of current releases
  _values :: !(Vector (Maybe ([Release], TransactionHash))),
  -- | The priority queue with indices to the vector items on each timestamp.
  _pendingReleases :: !(Map Timestamp [Int]),
  -- | The total amount that is locked for this account
  _totalLockedUpBalance :: !Amount
  } deriving (Show, Eq)
makeLenses ''AccountReleaseSchedule

instance ToJSON AccountReleaseSchedule where
  toJSON AccountReleaseSchedule{..} =
    AE.object ["total" AE..= _totalLockedUpBalance,
               "schedule" AE..= map toObject (map (\x -> (fst (head x), foldl' (\(accB, accT) (_, (b, t)) -> (accB + b, t : accT)) (0, []) x)) $
                                              groupBy ((==) `on` fst) $
                                              sortOn fst
                                              [ (tm, (a, t)) | Just (r, t) <- Vector.toList _values, Release tm a <- r ])]

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
                                               hasItem <- getWord8
                                               if hasItem == 0
                                                 then return Nothing
                                                 else do
                                                   item <- get
                                                   txh <- get
                                                   return $ Just (item, txh))
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
                                    Just (r, _) -> prevB <> hashToByteString (getHashOfReleases r)) BS.empty _values

------------------------------------- API --------------------------------------

-- | Create an empty account release schedule
emptyAccountReleaseSchedule :: AccountReleaseSchedule
emptyAccountReleaseSchedule = AccountReleaseSchedule Vector.empty Map.empty 0

-- | Add a list of amounts to this @AccountReleaseSchedule@.
addReleases :: ([(Timestamp, Amount)], TransactionHash) -> AccountReleaseSchedule -> AccountReleaseSchedule
addReleases (l, txh) ars =
  let newIdx = Vector.length $ _values ars
      (chain, totalAmount, timestamps) = foldr (\(t, a) (next, am, ts) -> (Release t a : next, am + a, t:ts)) ([], 0, []) l in
    ars & values %~ flip Vector.snoc (Just (chain, txh))
        & pendingReleases %~ flip (foldl' (\m t -> Map.alter (maybe (Just [newIdx]) (Just . (newIdx :))) t m)) timestamps
        & totalLockedUpBalance +~ totalAmount

-- | Remove the amounts up to the given timestamp.
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
                  (v Vector.// [(idx, Just (toKeep', txh))], am + acumAmount)
          (_values', minusAmount) = foldl' f (ars ^. values, 0) fullToRemove
      in
        (minusAmount, fst <$> Map.lookupMin toKeep, ars & values .~ _values'
                                                        & pendingReleases .~ toKeep
                                                        & totalLockedUpBalance -~ minusAmount)
