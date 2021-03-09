{-# LANGUAGE TemplateHaskell,
             OverloadedStrings,
             ScopedTypeVariables #-}
{-|
Module      : Concordium.GlobalState.Persistent.BlockState.AccountReleaseSchedule
Description : The data structure implementing account lock ups.

This module defines a data structure that stores the amounts that are locked up
for a given account. The defined data structure  can be written to the disk and
will be retrieved as needed. It can also be fully reconstructed from the disk.

The structure consists of a vector and a priority queue:

* The priority queue (implemented with a Map) maps timestamps to the index in
which the schedule is stored in the vector. The prority queue lives purely in
memory and will be reconstructed when reading the structure from the disk.

* The vector keeps a list of Nullable hashed buffered references to the first
release of each schedule, which is a Null terminated linked list that points
backwards in the BlobStore. This vector is written to the disk and all its values
are recursively written to the disk (see the description of the @Release@ datatype).

Whenever a release schedule is completed, its entry in the vector will be
replaced with a Null reference. Once every entry in the vector is empty (checked
with the remaining total locked amount) it just resets the structure to an empty
structure, effectively resetting the size of the vector to 0.

== Disk layout description

When the vector has only one schedule pending (@r@), the disk would look like
this:

> | (Null <-) r3 | (r3 <-) r2 | (r2 <-) r1 | [Just (r1 <-)] |

which means that we have a linked chain or releases and then store a vector with a pointer to r1.

Supposing we then release the first amount (@r1@), the new layout would be:

> | (Null <-) r3 | (r3 <-) r2 | (r2 <-) r1 | [Just (r1 <-)] | [Just (r2 <-)] |

where everything except the last item was already stored in the disk. Now we have an vector with an item that points to @r2@.

And if we then add a new schedule (@s@):

> | (Null <-) r3 | (r3 <-) r2 | (r2 <-) r1 | [Just (r1 <-)] | [Just (r2 <-)] | (Null <-) s2 | (s2 <-) s1 | [Just (r2 <-), Just (s1 <-)] |

which has essentially added the @s_i@ releases and written a new vector that now also contains the pointer to @s1@.

If we then have to unlock both @r2@ and @r3@, the new layout would be:

> | (Null <-) r3 | (r3 <-) r2 | (r2 <-) r1 | [Just (r1 <-)] | [Just (r2 <-)] | (Null <-) s2 | (s2 <-) s1 | [Just (r2 <-), Just (s1 <-)] | [Nothing, Just (s1 <-)] |

So we just stored a new vector in which the first item is empty.

When all the releases in the vector have been released, we reset the vector to the empty one instead of keeping a list of empty values, so
suppose now that we unlock @s1@ and @s2@, the new layout would be:

> | (Null <-) r3 | (r3 <-) r2 | (r2 <-) r1 | [Just (r1 <-)] | [Just (r2 <-)] | (Null <-) s2 | (s2 <-) s1 | [Just (r2 <-), Just (s1 <-)] | [Nothing, Just (s1 <-)] | [] |
-}
module Concordium.GlobalState.Persistent.BlockState.AccountReleaseSchedule (
  -- * Account Release Schedule type
  AccountReleaseSchedule,
  -- * Construction
  emptyAccountReleaseSchedule,
  addReleases,
  -- * Deletion
  unlockAmountsUntil,
  -- * Conversions
  loadPersistentAccountReleaseSchedule,
  storePersistentAccountReleaseSchedule
  ) where

import Concordium.Crypto.SHA256
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule as Transient
import Concordium.Types
import Concordium.Types.HashableTo
import Control.Monad
import qualified Data.ByteString as BS
import Data.Foldable
import Data.List (group, sort)
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Serialize
import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Lens.Micro.Platform
import Concordium.Utils.Serialization

----------------------------------- Release ------------------------------------

-- | A release represents the data that will be stored in the disk for each
-- amount that has to be unlocked. Releases form a Null-terminated chain of
-- @HashedBufferedRef@s.
data Release = Release {
  _rTimestamp :: !Timestamp,
  _rAmount :: !Amount,
  _rNext :: !(Nullable (HashedBufferedRef Release))
  } deriving (Show)

-- | As every link in the chain is a HashedBufferedRef, when computing the hash
-- of a release we will compute the hash of @timestamp <> amount <> nextHash@.
instance MonadBlobStore m => MHashableTo m Hash Release where
  getHashM rel = go (put (_rTimestamp rel) >> put (_rAmount rel)) (_rNext rel)
    where go partial Null = return $ hash $ runPut partial
          go partial (Some r) = do
            nextHash <- getHashM r
            return $ hash (runPut partial <> hashToByteString nextHash)

instance MonadBlobStore m => BlobStorable m Release where
  storeUpdate r@Release{..} = do
    (pNext, _rNext') <- storeUpdate _rNext
    return $ ( put _rTimestamp >> put _rAmount >> pNext,
              r { _rNext = _rNext' })
  store r = fst <$> storeUpdate r
  load = do
    _rTimestamp <- get
    _rAmount <- get
    pNext <- load
    return $ do
      _rNext <- pNext
      return Release {..}

--------------------------- Account release schedule ---------------------------

-- | Stores schedules. New items are inserted with 'addReleases' and are removed
-- with 'unlockAmountsUntil'.
data AccountReleaseSchedule = AccountReleaseSchedule {
  _arsValues :: !(Vector (Nullable (HashedBufferedRef Release, TransactionHash))),
  _arsPrioQueue :: !(Map Timestamp [Int]),
  _arsTotalLockedUpBalance :: !Amount
  } deriving (Show)
makeLenses ''AccountReleaseSchedule

instance MonadBlobStore m => BlobStorable m AccountReleaseSchedule where
  storeUpdate a = (, a) <$> store a
  store AccountReleaseSchedule{..} = do
    let f accPut item = do
              pItem <- store item
              return (accPut >> pItem)
    Vector.foldM' f (putLength $ Vector.length _arsValues) _arsValues
  load = do
    numOfReleases <- getLength
    case numOfReleases of
      0 -> return $ return emptyAccountReleaseSchedule
      _ -> do
        _arsValues' <- Vector.replicateM numOfReleases load
        return $ do
          _arsValues <- Vector.mapM id _arsValues'
          let -- if this item is empty just continue
              f acc _ Null = return acc
              -- if this item is not empty, process this schedule
              f acc thisIndex (Some (thisRef, _)) = do
                let -- when reaching an empty item, return
                    g accum Null = return accum
                    -- when processing a non-empty item, load it and push the values
                    g (gThisMap, gThisBalance) (Some ref) = do
                      Release{..} <- refLoad ref
                      g (Map.alter (maybe (Just [thisIndex]) (Just . (thisIndex :))) _rTimestamp gThisMap, gThisBalance + _rAmount) _rNext
                g acc (Some thisRef)
          (_arsPrioQueue, _arsTotalLockedUpBalance) <- Vector.ifoldM f (Map.empty, 0) _arsValues
          return AccountReleaseSchedule{..}

-- | @hash(AccountReleaseSchedule(releases) = hash (foldl (\h i -> h <> hash i)
-- mempty) releases@ so @hash (hash a_1 <> hash a_2 <> ... <> hash a_n)@
instance MonadBlobStore m => MHashableTo m Transient.AccountReleaseScheduleHash AccountReleaseSchedule where
  getHashM AccountReleaseSchedule{..} =
    if _arsTotalLockedUpBalance == 0
    then return Transient.emptyAccountReleaseScheduleHash
    else Transient.AccountReleaseScheduleHash . hash <$> Vector.foldM' (\prevB -> \case
                                    Null -> return prevB
                                    Some (r, _) -> do
                                      itemHash <- getHashM r
                                      return $ prevB <> hashToByteString itemHash) BS.empty _arsValues

instance MonadBlobStore m => Cacheable m AccountReleaseSchedule where

------------------------------------- API --------------------------------------

-- | The empty account release schedule.
emptyAccountReleaseSchedule :: AccountReleaseSchedule
emptyAccountReleaseSchedule = AccountReleaseSchedule Vector.empty Map.empty 0

-- | Insert a new schedule in the structure.
--
-- Precondition: The given list of timestamps and amounts MUST NOT be empty.
addReleases :: MonadBlobStore m => ([(Timestamp, Amount)], TransactionHash) -> AccountReleaseSchedule -> m AccountReleaseSchedule
addReleases (l, txh) ars = do
  -- get the index that will be used with this new item
  let itemIndex = length $ ars ^. arsValues
      -- This fold will:
      -- 1. Chain the releases, adding links to the next release for each one of them.
      -- 2. Accumulate the total amount that is scheduled in these releases.
      -- 3. Add the entries to the prio queue so that they are later unqueued.
      f (thisTimestamp, thisAmount) (nextRelease, thisPrioQueue, thisBalance) = do
        thisReleaseRef <- makeHashedBufferedRef (Release thisTimestamp thisAmount nextRelease)
        let thisPrioQueue' = Map.alter (maybe (Just [itemIndex]) (Just . (itemIndex:))) thisTimestamp thisPrioQueue
            thisBalance' = thisBalance + thisAmount
        return (Some thisReleaseRef, thisPrioQueue', thisBalance')
  ~(Some first, arsPrioQueue', arsTotalLockedUpBalance') <- foldrM f (Null, ars ^. arsPrioQueue, ars ^. arsTotalLockedUpBalance) l

  return $ ars & arsValues %~ flip Vector.snoc (Some (first, txh))
               & arsPrioQueue .~ arsPrioQueue'
               & arsTotalLockedUpBalance .~ arsTotalLockedUpBalance'

-- | Returns the amount that was unlocked, the next timestamp for this account
-- (if there is one) and the new account release schedule after removing the
-- amounts whose timestamp was less or equal to the given timestamp.
unlockAmountsUntil :: MonadBlobStore m => Timestamp -> AccountReleaseSchedule -> m (Amount, Maybe Timestamp, AccountReleaseSchedule)
unlockAmountsUntil up ars = do
  let (toRemove, x, toKeep) = Map.splitLookup up (ars ^. arsPrioQueue)
  if Map.null toKeep
    -- If we are going to clear the whole release schedule then short-circuit
    then return (ars ^. arsTotalLockedUpBalance, Nothing, emptyAccountReleaseSchedule)
    else do
    let
      -- create a list of tuples of:
      -- - list in which all the elements are equal, this will be the index at which we shall remove releases.
      -- - the length of said list, to see how many elements we have to remove from the schedule.
      fullToRemove = map (\y -> (y, length y)) $ group $ sort $ concat $ maybe id (:) x $ Map.elems toRemove

      -- for each index, purge its release schedule.
      f (v, am) (idx:_, numOfItems) = do
        case v Vector.! idx of
          Null -> return (v, am)
          Some (item, txh) -> do
            let g (this, acc) =
                  case this of
                    -- In principle this will not happen,
                    -- numOfItems will be 0 before this is reached
                    Null -> return (False, (Null, acc))
                    Some r -> do
                      Release{..} <- refLoad r
                      return (True, (_rNext, acc + _rAmount))
            (newItem, acc) <- pickNthResultM g (Some item, 0) numOfItems
            -- Update this element in the vector and return the accumulated amount.
            case newItem of
              Null -> return (v Vector.// [(idx, Null)], am + acc)
              Some i -> return (v Vector.// [(idx, Some (i, txh))], am + acc)

      f _ ([], _) = error "This case cannot happen" -- group won't create empty lists so this is unreachable

    (arsValues', minusAmount) <- foldM f (ars ^. arsValues, 0) fullToRemove
    return (minusAmount, fst <$> Map.lookupMin toKeep, ars & arsValues .~ arsValues'
                                                           & arsPrioQueue .~ toKeep
                                                           & arsTotalLockedUpBalance -~ minusAmount)

-- | Get the Nth element after repeating the monadic operation or after short-circuiting on the given function
--
-- This function is kind of equivalent to @head . drop num <$> iterateM f i@ if @iterateM :: (a -> m a) -> a -> m [a]@ existed. but also with
-- short-circuit if the function @f@ returns @(False, _)@.
pickNthResultM :: Monad m
               => (a -> m (Bool, a)) -- ^ The iteration function. If it returns @(False,_)@ this function will terminate
               -> a -- ^ The initial element
               -> Int -- ^ Number of times we want to repeat this operation
               -> m a
pickNthResultM f i num
  | num > 0 = do
      (continue, newI) <- f i
      if continue
        then pickNthResultM f newI (num - 1)
        else return newI
  | otherwise = return i

--------------------------------- Conversions ----------------------------------

storePersistentAccountReleaseSchedule :: MonadBlobStore m => Transient.AccountReleaseSchedule -> m AccountReleaseSchedule
storePersistentAccountReleaseSchedule Transient.AccountReleaseSchedule{..} = do
  _arsValues <- Vector.mapM (\case
                                Nothing -> return Null
                                Just (r, t) -> fmap (, t) <$> foldrM (\(Transient.Release thisTimestamp thisAmount) nextRelease -> Some <$> makeHashedBufferedRef (Release thisTimestamp thisAmount nextRelease)) Null r) _values
  return AccountReleaseSchedule{
    _arsPrioQueue = _pendingReleases,
    _arsTotalLockedUpBalance = _totalLockedUpBalance,
    ..
    }



loadPersistentAccountReleaseSchedule :: MonadBlobStore m => AccountReleaseSchedule -> m Transient.AccountReleaseSchedule
loadPersistentAccountReleaseSchedule AccountReleaseSchedule{..} = do
  _values <- Vector.mapM (\case
                             Null -> return Nothing
                             Some (r, t) ->
                               let go Release{..} = do
                                     next <- case _rNext of
                                               Null -> return []
                                               Some n -> go =<< refLoad n
                                     return $ Transient.Release _rTimestamp _rAmount : next
                               in
                                 Just . (, t) <$> (go =<< refLoad r)
                         ) _arsValues
  return Transient.AccountReleaseSchedule {
  _pendingReleases = _arsPrioQueue,
  _totalLockedUpBalance = _arsTotalLockedUpBalance,
  ..
  }
