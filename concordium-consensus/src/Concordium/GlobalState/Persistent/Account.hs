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

--

data LinkedEncryptedAmount = LinkedEncryptedAmount {
  leAmount :: !EncryptedAmount,
  lePointer :: !(Nullable (BufferedRef LinkedEncryptedAmount))
  } deriving Show

instance MonadBlobStore r m => BlobStorable r m LinkedEncryptedAmount where
  storeUpdate v@LinkedEncryptedAmount{..} = do
    ref <- case lePointer of
      Null -> return refNull
      Some r -> getBRRef r
    let putter = do
          put leAmount
          put ref
    return (putter, v)
  store a = fst <$> storeUpdate a
  load = do
    leAmount <- get
    br <- get
    return . return $ LinkedEncryptedAmount leAmount $ Some (BRBlobbed br)

data PersistentAccountEncryptedAmount = PersistentAccountEncryptedAmount {
  _startIdx :: !EncryptedAmountAggIndex,
  _start :: !(Nullable (BufferedRef LinkedEncryptedAmount)),
  _end :: !(Nullable (BufferedRef LinkedEncryptedAmount)),
  _numAmounts :: !Word32,
  _aggAmount :: !(Nullable (BufferedRef EncryptedAmount)),
  _numAgg :: !(Maybe Word32),
  _self :: !(BufferedRef EncryptedAmount),
  _pAccountEncAmountHash :: !Hash.Hash
  } deriving Show

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
          iterateStep lastItem

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


addIncomingEncryptedAmount :: MonadBlobStore r m => EncryptedAmount -> PersistentAccountEncryptedAmount -> m PersistentAccountEncryptedAmount
addIncomingEncryptedAmount newAmount old@PersistentAccountEncryptedAmount{..} = do
  afterCombining <- if fromIntegral _numAmounts >= maxNumIncoming
    then do
    case (_start, _end) of
      (Some st, Some ed) ->
        case _aggAmount of
          Null -> do
            list <- loadLinkedAmountList st ed
            case list of
              ((_, first):(_, second):(thirdref, _):_) -> do
                let newAggAmount = leAmount first <> leAmount second
                    newStart = Some thirdref
                    newNumAmounts = _numAmounts - (1 :: Word32)
                    newNumAgg = 2 :: Word32
                    newStartIdx = _startIdx + 1
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
            list <- loadLinkedAmountList st ed
            case list of
              ((_, first):(secondref, _):_) -> do
                oldAggAmount <- loadBufferedRef v
                let newAggAmount = oldAggAmount <> leAmount first
                    newStart = Some secondref
                    newNumAmounts = _numAmounts - (1 :: Word32)
                    newNumAgg = maybe 2 (+1) _numAgg
                    newStartIdx = _startIdx + 1
                newAggAmountRef <- makeBufferedRef newAggAmount
                return old {
                  _start = newStart,
                  _numAmounts = newNumAmounts,
                  _numAgg = Just newNumAgg,
                  _startIdx = newStartIdx,
                  _aggAmount = Some newAggAmountRef
                  }
              _ -> error "Data claims that there are more than maxNumIncoming amounts but we could not even read 3 of those amounts"
      _ -> return old
    else do
    return old

  newItem <- makeBufferedRef (LinkedEncryptedAmount newAmount _end)
  let newEnd = Some newItem
      newStart = if _numAmounts == 0 then newEnd else _start
      newHash = Hash.hash (Hash.hashToByteString _pAccountEncAmountHash <> encode newAmount)
  return afterCombining{_numAmounts = _numAmounts + 1, _end = newEnd, _start = newStart, _pAccountEncAmountHash = newHash}

addToSelfEncryptedAmount :: MonadBlobStore r m => EncryptedAmount -> PersistentAccountEncryptedAmount -> m PersistentAccountEncryptedAmount
addToSelfEncryptedAmount newAmount old@PersistentAccountEncryptedAmount{..} = do
  oldSelf <- loadBufferedRef _self
  newSelf <- makeBufferedRef (oldSelf <> newAmount)
  return old{_self = newSelf, _pAccountEncAmountHash = Hash.hash (Hash.hashToByteString _pAccountEncAmountHash <> encode newAmount)}

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
  return afterDrop{_self = newAmountRef, _pAccountEncAmountHash = Hash.hash (Hash.hashToByteString _pAccountEncAmountHash <> encode newAmount <> encode newIndex)}
