{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.GlobalState.Persistent.Account where

import Data.Serialize
import Lens.Micro.Platform
import Data.Word

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types.HashableTo
import qualified Concordium.GlobalState.Basic.BlockState.Account as Transient
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types
import Concordium.GlobalState.Account hiding (addIncomingEncryptedAmount, addToSelfEncryptedAmount)
import Concordium.Crypto.EncryptedTransfers
import qualified Data.Sequence as Seq
import Data.Foldable (foldlM)

-- | An encrypted amount that possibly holds a buffered ref to another one.
data LinkedEncryptedAmount = LinkedEncryptedAmount {
  -- | The encrypted amount
  leAmount :: !EncryptedAmount,
  -- | A reference to the previous linked encrypted amount on the incoming amounts list.
  lePointer :: !(Nullable (BufferedRef LinkedEncryptedAmount))
  } deriving Show

instance MonadBlobStore r m => BlobStorable r m LinkedEncryptedAmount where
  storeUpdate v@LinkedEncryptedAmount{..} = do
    ref <- case lePointer of
      Null -> return refNull
      Some r -> getBRRef r -- note that this one will not store the amount, instead we assume it has already been stored when writing the list.
    let putter = do
          put leAmount
          put ref
    return (putter, v)
  store a = fst <$> storeUpdate a
  load = do
    leAmount <- get
    br <- get
    return . return $ LinkedEncryptedAmount leAmount $ if isNull br then Null else Some (BRBlobbed br)

-- | The persistent version of the encrypted amount structure per account.
-- See note [Persistent Encrypted Amount] below.
data PersistentAccountEncryptedAmount = PersistentAccountEncryptedAmount {
  -- | The index of the first encrypted amount.
  _startIdx :: !EncryptedAmountAggIndex,
  -- | The pointer to the first encrypted amount. It is null if the list is empty.
  _start :: !(Nullable (BufferedRef LinkedEncryptedAmount)),
  -- | The pointer to the last encrypted amount. It is null if the list is empty.
  _end :: !(Nullable (BufferedRef LinkedEncryptedAmount)),
  -- | The number of items in the list.
  _numAmounts :: !Word32,
  -- | The reference to the aggregated amount if present.
  _aggAmount :: !(Nullable (BufferedRef EncryptedAmount)),
  -- | The number of aggregated amounts if present.
  _numAgg :: !(Maybe Word32),
  -- | The reference to the self encrypted amount.
  _self :: !(BufferedRef EncryptedAmount),
  -- | The current running hash of the structure. See notes on _accountEncAmountHash for instructions on how to update it.
  _pAccountEncAmountHash :: !Hash.Hash
  } deriving Show

-- | Create an empty PersistentAccountEncryptedAmount
initialPersistentAccountEncryptedAmount :: MonadBlobStore r m => m PersistentAccountEncryptedAmount
initialPersistentAccountEncryptedAmount = do
  _self <- makeBufferedRef mempty
  return PersistentAccountEncryptedAmount {
    _startIdx = 0,
    _start = Null,
    _end = Null,
    _numAmounts = 0,
    _aggAmount = Null,
    _numAgg = Nothing,
    _pAccountEncAmountHash = Hash.hash "",
    ..
    }

-- | Given an AccountEncryptedAmount, create a Persistent version of it
storePersistentAccountEncryptedAmount :: MonadBlobStore r m => AccountEncryptedAmount -> m PersistentAccountEncryptedAmount
storePersistentAccountEncryptedAmount AccountEncryptedAmount{..} = do
  _self <- makeBufferedRef _selfAmount
  let _numAmounts = fromIntegral $ length _incomingEncryptedAmounts
      _pAccountEncAmountHash = _accountEncAmountHash
  case _incomingEncryptedAmounts of
    Seq.Empty ->
      -- Empty incoming encrypted amounts, start and end set to Null
      return PersistentAccountEncryptedAmount{
      _end = Null,
      _start = Null,
      _startIdx = _startIndex,
      _numAgg = Nothing,
      _aggAmount = Null,
      ..
      }
    (agg Seq.:<| rest) ->
      case _numAggregated of
        v@(Nothing) -> do
          -- We have incoming amounts but none of them are aggregated. Store each amount.
          (_end, list) <- foldlM (\(prev, list) e -> do
                                         l <- makeBufferedRef (LinkedEncryptedAmount e prev)
                                         return (Some l, l:list)) (Null, []) _incomingEncryptedAmounts
          let _start = Some (last list)
              _startIdx = _startIndex
              _numAgg = v
              _aggAmount = Null
          return PersistentAccountEncryptedAmount{..}

        v@(Just _) -> do
          -- We have incoming amounts and the first one is an aggregated amount. Store it and then the list.
          aggAm <- makeBufferedRef agg
          (_end, list) <- foldlM (\(prev, list) e -> do
                                         l <- makeBufferedRef (LinkedEncryptedAmount e prev)
                                         return (Some l, l:list)) (Null, []) rest
          let _start = Some (last list)
              _startIdx = _startIndex
              _numAgg = v
              _aggAmount = Some aggAm
          return PersistentAccountEncryptedAmount{..}

-- | Given the first and last references to the elements in the list, traverse the list loading every value.
loadLinkedAmountList :: MonadBlobStore r m => BufferedRef LinkedEncryptedAmount -> BufferedRef LinkedEncryptedAmount -> m [(BufferedRef LinkedEncryptedAmount, LinkedEncryptedAmount)]
loadLinkedAmountList st ed = do
          firstRef <- getBRRef st
          lastItem <- loadBufferedRef ed
          let iterateStep LinkedEncryptedAmount{..} = do
                          case lePointer of
                            -- if the pointer is null then return
                            Null -> return []
                            -- if the pointer is not null, get next element and continue
                            Some v -> do
                              nextRef <- getBRRef v
                              next <- loadBufferedRef v
                              if nextRef == firstRef
                                then do
                                return [(v,next)]
                                else do
                                ((v, next) :) <$> iterateStep next
          ((ed, lastItem) :) <$> iterateStep lastItem

-- | Given a PersistentAccountEncryptedAmount, load its equivalent AccountEncryptedAmount
loadPersistentAccountEncryptedAmount :: MonadBlobStore r m => PersistentAccountEncryptedAmount -> m AccountEncryptedAmount
loadPersistentAccountEncryptedAmount PersistentAccountEncryptedAmount{..} = do
  _selfAmount <- loadBufferedRef _self
  let _startIndex = _startIdx
      _numAggregated = _numAgg
      _accountEncAmountHash = _pAccountEncAmountHash
  _incomingEncryptedAmounts <- loadAmounts _start _end _aggAmount
  return AccountEncryptedAmount{..}
  where loadAmounts Null _ _ = return Seq.Empty
        loadAmounts _ Null _ = return Seq.Empty
        loadAmounts (Some st) (Some ed) Null = Seq.fromList . map (leAmount . snd) <$> loadLinkedAmountList st ed
        loadAmounts (Some st) (Some ed) (Some v) = do
          first <- loadBufferedRef v
          (first Seq.:<|)  . Seq.fromList . map (leAmount . snd) <$> loadLinkedAmountList st ed

instance MonadBlobStore r m => BlobStorable r m PersistentAccountEncryptedAmount where
  storeUpdate PersistentAccountEncryptedAmount{..} = do
    (pStart, dataStart) <- storeUpdate _start
    (pEnd, dataEnd) <- storeUpdate _end
    (pAggAmount, dataAggAmount) <- storeUpdate _aggAmount
    (pSelf, dataSelf) <- storeUpdate _self
    let dataEnc = PersistentAccountEncryptedAmount {
          _start = dataStart,
          _end = dataEnd,
          _aggAmount =  dataAggAmount,
          _self = dataSelf,
          ..
          }
    let putEnc = do
          put _startIdx
          pStart
          pEnd
          put _numAmounts
          pAggAmount
          put _numAgg
          pSelf
          put _pAccountEncAmountHash
    return (putEnc, dataEnc)

  store a = fst <$> storeUpdate a
  load = do
    _startIdx <- get
    pStart <- load
    pEnd <- load
    _numAmounts <- get
    pAggAmount <- load
    _numAgg <- get
    pSelf <- load
    _pAccountEncAmountHash <- get
    return $ do
      _start <- pStart
      _end <- pEnd
      _aggAmount <- pAggAmount
      _self <- pSelf
      return PersistentAccountEncryptedAmount {..}


data PersistentAccount = PersistentAccount {
  -- |Next available nonce for this account.
  _accountNonce :: !Nonce
  -- |Current public account balance.
  ,_accountAmount :: !Amount
  -- |List of encrypted amounts on the account.
  -- TODO (MRA) create bufferedref list
  ,_accountEncryptedAmount :: !(BufferedRef PersistentAccountEncryptedAmount)
  -- |A pointer to account data that changes rarely
  ,_persistingData :: !(BufferedRef PersistingAccountData)
  -- |A hash of all account data. We store the hash explicitly here because we cannot compute the hash once
  -- the persisting account data is stored behind a pointer
  ,_accountHash :: !Hash.Hash
  } deriving Show

makeLenses ''PersistentAccount

instance MonadBlobStore r m => BlobStorable r m PersistentAccount where
    storeUpdate PersistentAccount{..} = do
        (pAccData, accData) <- storeUpdate _persistingData
        (pEnc, encData) <- storeUpdate _accountEncryptedAmount
        let persistentAcc = PersistentAccount {
                _persistingData = accData,
                _accountEncryptedAmount = encData,
                ..
            }
        let putAccs = do
                    put _accountNonce
                    put _accountAmount
                    pAccData
                    pEnc
        return (putAccs, persistentAcc)
    store a = fst <$> storeUpdate a
    load = do
        _accountNonce <- get
        _accountAmount <- get
        mAccountEncryptedAmountPtr <- load
        mAccDataPtr <- load
        return $ do
          _persistingData <- mAccDataPtr
          _accountEncryptedAmount <- mAccountEncryptedAmountPtr
          pData <- loadBufferedRef _persistingData
          eData <- loadBufferedRef _accountEncryptedAmount
          let _accountHash = makeAccountHash _accountNonce _accountAmount (_pAccountEncAmountHash eData) pData
          return PersistentAccount {..}

instance HashableTo Hash.Hash PersistentAccount where
  getHash = _accountHash

instance Monad m => MHashableTo m Hash.Hash PersistentAccount

-- |Make a 'PersistentAccount' from an 'Transient.Account'.
makePersistentAccount :: MonadBlobStore r m => Transient.Account -> m PersistentAccount
makePersistentAccount Transient.Account{..} = do
  let _accountHash = makeAccountHash _accountNonce _accountAmount (_accountEncAmountHash _accountEncryptedAmount) _accountPersisting
  _persistingData <- makeBufferedRef _accountPersisting
  _accountEncryptedAmount <- makeBufferedRef =<< storePersistentAccountEncryptedAmount _accountEncryptedAmount
  return PersistentAccount {..}

-- |Checks whether the two arguments represent the same account. (Used for testing.)
sameAccount :: MonadBlobStore r m => Transient.Account -> PersistentAccount -> m Bool
sameAccount bAcc pAcc@PersistentAccount{..} = do
  _accountPersisting <- loadBufferedRef _persistingData
  _accountEncryptedAmount <- loadPersistentAccountEncryptedAmount =<< loadBufferedRef _accountEncryptedAmount
  return $ sameAccountHash bAcc pAcc && Transient.Account{..} == bAcc

-- |Checks whether the two arguments represent the same account by comparing the account hashes.
-- (Used for testing.)
sameAccountHash :: Transient.Account -> PersistentAccount -> Bool
sameAccountHash bAcc pAcc = getHash bAcc == _accountHash pAcc

-- |Load a field from an account's 'PersistingAccountData' pointer. E.g., @acc ^^. accountAddress@ returns the account's address.
(^^.) :: MonadBlobStore r m
      => PersistentAccount
      -> Getting b PersistingAccountData b
      -> m b
acc ^^. l = (^. l) <$> loadBufferedRef (acc ^. persistingData)

{-# INLINE (^^.) #-}
infixl 8 ^^.

-- |Update a field of an account's 'PersistingAccountData' pointer, creating a new pointer.
-- Used to implement '.~~' and '%~~'.
setPAD :: MonadBlobStore r m
          => (PersistingAccountData -> PersistingAccountData)
          -> PersistentAccount
          -> m PersistentAccount
setPAD f acc@PersistentAccount{..} = do
  pData <- loadBufferedRef (acc ^. persistingData)
  eac <- _pAccountEncAmountHash <$> loadBufferedRef _accountEncryptedAmount
  newPData <- makeBufferedRef $ f pData
  return $ acc & persistingData .~ newPData
               & accountHash .~ makeAccountHash _accountNonce _accountAmount eac pData

-- |Set a field of an account's 'PersistingAccountData' pointer, creating a new pointer.
-- E.g., @acc & accountStakeDelegate .~~ Nothing@ sets the
-- account's stake delegate to 'Nothing'.
(.~~) :: MonadBlobStore r m
      => ASetter PersistingAccountData PersistingAccountData a b
      -> b
      -> PersistentAccount
      -> m PersistentAccount
(.~~) l v = setPAD (l .~ v)

{-# INLINE (.~~) #-}
infixr 4 .~~

-- |Modify a field of an account's 'PersistingAccountData' pointer, creating a new pointer.
-- E.g., @acc & accountInstances %~~ Set.insert i@ inserts an instance @i@ to the set of an account's instances.
(%~~) :: MonadBlobStore r m
      => ASetter PersistingAccountData PersistingAccountData a b
      -> (a -> b)
      -> PersistentAccount
      -> m PersistentAccount
(%~~) l f = setPAD (l %~ f)

{-# INLINE (%~~) #-}
infixr 4 %~~

-- | Add an encrypted amount to the end of the list.
-- This is used when an incoming transfer is added to the account. If this would
-- go over the threshold for the maximum number of incoming amounts then
-- aggregate the first two incoming amounts.
addIncomingEncryptedAmount :: MonadBlobStore r m => EncryptedAmount -> PersistentAccountEncryptedAmount -> m PersistentAccountEncryptedAmount
addIncomingEncryptedAmount newAmount old@PersistentAccountEncryptedAmount{..} = do
  afterCombining <- if fromIntegral _numAmounts >= maxNumIncoming
    then do
    case (_start, _end) of
      (Some st, Some ed) ->
        -- The list has some items
        case _aggAmount of
          Null -> do
            -- we dont have yet aggregated anything
            list <- loadLinkedAmountList st ed
            case list of
              ((_, first):(_, second):(thirdref, _):_) -> do
                -- combine the first and second items on the list
                let newAggAmount = leAmount first <> leAmount second
                    -- now _start points to the third item
                    newStart = Some thirdref
                    -- we have one amount less
                    newNumAmounts = _numAmounts - (1 :: Word32)
                    -- we have 2 amounts aggregated
                    newNumAgg = 2 :: Word32
                    -- the index of the first amount is one more than before
                    newStartIdx = _startIdx + 1
                -- we create the new amount
                newAggAmountRef <- makeBufferedRef newAggAmount
                return old {
                    _start = newStart,
                    _numAmounts = newNumAmounts,
                    _numAgg = Just newNumAgg,
                    _startIdx = newStartIdx,
                    _aggAmount = Some newAggAmountRef
                    }
              _ -> error "Data claims that there are more than maxNumIncoming amounts but we could not even read 3 of those amounts"
          Some v -> do
            -- we already have one aggregated amount
            list <- loadLinkedAmountList st ed
            case list of
              ((_, first):(secondref, _):_) -> do
                -- combine the amount we had and the first one
                oldAggAmount <- loadBufferedRef v
                let newAggAmount = oldAggAmount <> leAmount first
                    -- now _start points to the second item
                    newStart = Some secondref
                    -- we have one amount less
                    newNumAmounts = _numAmounts - (1 :: Word32)
                    -- we have one more amount aggregated
                    newNumAgg = maybe 2 (+1) _numAgg
                    -- the index of the first amount is one more than before
                    newStartIdx = _startIdx + 1
                -- we create the new amount
                newAggAmountRef <- makeBufferedRef newAggAmount
                return old {
                  _start = newStart,
                  _numAmounts = newNumAmounts,
                  _numAgg = Just newNumAgg,
                  _startIdx = newStartIdx,
                  _aggAmount = Some newAggAmountRef
                  }
              _ -> error "Data claims that there are more than maxNumIncoming amounts but we could not even read 3 of those amounts"
      _ ->
        -- we have no incoming amounts, so there is nothing to combine
        return old
    else do
    -- we have less than maxNumIncoming amounts so we don't have to combine
    return old

  -- create the new amount
  newItem <- makeBufferedRef (LinkedEncryptedAmount newAmount _end)
  let
    -- _end will be the new item
    newEnd = Some newItem
    -- start will be the same as after combining or the new item if we had no items before
    newStart = if _numAmounts afterCombining == 0 then newEnd else _start afterCombining
    newHash = Hash.hash (Hash.hashToByteString _pAccountEncAmountHash <> encode newAmount)
  return afterCombining{_numAmounts = _numAmounts afterCombining + 1,
                        _end = newEnd,
                        _start = newStart,
                        _pAccountEncAmountHash = newHash}

-- | Drop the encrypted amount with indices up to the given one, and add the new amount at the end.
-- This is used when an account is transfering from from an encrypted balance, and the newly added
-- amount is the remaining balance that was not used.
--
-- As mentioned above, the whole 'selfBalance' must always be used in any
-- outgoing action of the account.
replaceUpTo :: MonadBlobStore r m => EncryptedAmountAggIndex -> EncryptedAmount -> PersistentAccountEncryptedAmount -> m PersistentAccountEncryptedAmount
replaceUpTo newIndex newAmount old@PersistentAccountEncryptedAmount{..} = do
  afterDrop <-
    if newIndex > _startIdx
    then do
      -- We have to remove some amounts
      let toDrop = newIndex - _startIdx
      let newNumAmounts = _numAmounts - fromIntegral toDrop
      case (_start, _end) of
        (Some st, Some ed) -> do -- numAmounts > 0
          -- we have encrypted amounts, we remove toDrop or one less if we have agg amount
          list <- drop (if isNull _aggAmount then fromIntegral toDrop else fromIntegral toDrop - 1) <$> loadLinkedAmountList st ed
          -- agg amount will always be removed
          let newAggAmount = Null
              newNumAgg = Nothing
          -- lets find the _start and _end refs
          case list of
            ((firstRef, _):rest) -> do
              -- we have a first item, so _start is this one
              let newStart = Some firstRef
              case rest of
                (_:_) -> do
                  -- we have more items, so _end is the last one and we have newNumAmounts
                  let newEnd = Some (fst $ last rest)
                  return old{
                    _aggAmount = newAggAmount,
                    _numAgg = newNumAgg,
                    _start = newStart,
                    _end = newEnd,
                    _numAmounts = newNumAmounts,
                    _startIdx = newIndex
                    }
                _ -> do
                  -- we don't have more items, so _end is the same one as _start and we have 1 amount (which must be newNumAmounts)
                  let newEnd = newStart
                  return old{
                    _aggAmount = newAggAmount,
                    _numAgg = newNumAgg,
                    _start = newStart,
                    _end = newEnd,
                    _numAmounts = newNumAmounts,
                    _startIdx = newIndex
                    }
            _ -> do
              -- we removed all the items in the list so _end and _start are null and we have 0 amounts
              return old{
                    _aggAmount = newAggAmount,
                    _numAgg = newNumAgg,
                    _start = Null,
                    _end = Null,
                    _numAmounts = 0,
                    _startIdx = newIndex
                    }
        _ ->
          -- we don't have any amounts, so we cannot remove any amounts
          return old{_startIdx = newIndex}
    else
      -- we don't have to remove amounts
      return old
  newAmountRef <- makeBufferedRef newAmount
  return afterDrop{_self = newAmountRef,
                   _pAccountEncAmountHash = Hash.hash (Hash.hashToByteString _pAccountEncAmountHash <> encode newAmount <> encode newIndex)}

-- | Add the given encrypted amount to 'selfAmount'
-- This is used when the account is transferring from public to secret balance.
addToSelfEncryptedAmount :: MonadBlobStore r m => EncryptedAmount -> PersistentAccountEncryptedAmount -> m PersistentAccountEncryptedAmount
addToSelfEncryptedAmount newAmount old@PersistentAccountEncryptedAmount{..} = do
  oldSelf <- loadBufferedRef _self
  newSelf <- makeBufferedRef (oldSelf <> newAmount)
  return old{_self = newSelf, _pAccountEncAmountHash = Hash.hash (Hash.hashToByteString _pAccountEncAmountHash <> encode newAmount)}

-- Note [Persistent Encrypted Amount]
-------------------------------------
--
-- In order to explain how this structure is supposed to work, here it is presented
-- a diagram in which the receival of a new encrypted amount when the number of amounts
-- stored is already maxNumIncoming is shown.
--
-- On the drawings, the depicted array symbolizes the disk memory and the indices are
-- loose indices "by item", not respecting the byte size of each item. The values `RX` refer
-- to the reference to the position X.
--
-- In the beginning, we start with a PersistentAccountEncryptedAmount that shall represent
--  - The self encrypted amount of XX
--  - The incoming amounts list of [A,B,..,AG]
--  - No aggregated amounts, and the index for the first amount is 0
--
--                                                                     |
--                                                                     |
--                                                                     v
-- +-------------+-------------+     +-------------+------------+------+------+
-- |      0      |      1      |     |      31     |     32     |     33      |
-- +---------------------------+     +----------------------------------------+
-- |             |             |     |             |            | start: R0   |
-- |      A      |      B      |     |      AG     |            |   idx: 0    |
-- |             |             | ... |             |  self: XX  |   end: R31  |
-- |   prev: ()  |   prev: 0   |     |   prev: 30  |            |   agg: Nul  |
-- |             |             |     |             |            |  #agg: ()   |
-- |             |             |     |             |            |  self: R32  |
-- |             |             |     |             |            | #elem: 32   |
-- +-------------+-------------+     +-------------+------------+-------------+
--
-- When receiving another amount (AH), the first two amounts are combined and written into
-- a new position on the list and we also store the received amount, having then a PersistentAccountEncryptedAmount
-- that represents:
--  - The self encrypted amount of XX
--  - The incoming amounts list of [A+B,C,..,AG,AH]
--  - 2 aggregated amounts (A+B), and the index for the first amount is 1.
--
--                                                                                                              |
--                                                                                                              |
--                                                                                                              v
-- +-------------+-------------+     +-------------+------------+-------------+-------------+------------+------+------+
-- |      0      |      1      |     |      31     |     32     |     33      |      34     |     35     |     36      |
-- +---------------------------+     +----------------------------------------+----------------------------------------+
-- |             |             |     |             |            | start: R0   |             |            | start: R2   |
-- |      A      |      B      |     |      AG     |            |   idx: 0    |             |     AH     |   idx: 1    |
-- |             |             | ... |             |            |   end: R31  |             |            |   end: R35  |
-- |   prev: ()  |   prev: 0   |     |   prev: 30  |     XX     |   agg: Nul  |     A+B     |  prev: 31  |   agg: R34  |
-- |             |             |     |             |            |  #agg: ()   |             |            |  #agg: 2    |
-- |             |             |     |             |            |  self: R32  |             |            |  self: R32  |
-- |             |             |     |             |            | #elem: 32   |             |            | #elem: 32   |
-- +-------------+-------------+     +-------------+------------+-------------+-------------+------------+-------------+
--
-- This way, the actual list is formed by:
--  - if there is an aggregated amount, that one is the first element
--  - then the element pointed by _start. Its index is idx.
--  - then each element points to the previous one until reaching _end. We have #elem elements.
--
-- The rest of the operations are pretty much obvious. Here is a loosy description:
-- - inserting an element when we have room for it means just writing that element and modifying the _end pointer
-- - Replacing the self amount means writing the new self amount.
-- - Removing the amounts up to X means possibly removing the aggregated amount pointer (and #agg), and shifting the _start pointer the required number of items.
