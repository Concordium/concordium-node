{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
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
import Concordium.GlobalState.Account
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
  _aggAmount :: !(Nullable (BufferedRef EncryptedAmount)),
  _numAgg :: !(Maybe Word32),
  _self :: !(BufferedRef EncryptedAmount)
  } deriving Show

storePersistentAccountEncryptedAmount :: MonadBlobStore r m => AccountEncryptedAmount -> m PersistentAccountEncryptedAmount
storePersistentAccountEncryptedAmount AccountEncryptedAmount{..} = do
  _self <- makeBufferedRef _selfAmount
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

loadPersistentAccountEncryptedAmount :: MonadBlobStore r m => PersistentAccountEncryptedAmount -> m AccountEncryptedAmount
loadPersistentAccountEncryptedAmount PersistentAccountEncryptedAmount{..} = do
  _selfAmount <- loadBufferedRef _self
  let _startIndex = _startIdx
      _numAggregated = _numAgg
  _incomingEncryptedAmounts <- loadAmounts _start _end _aggAmount
  return AccountEncryptedAmount{..}
  where loadAmountList st ed = do
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
                                return [next]
                                else do
                                (next :) <$> iterateStep next
          Seq.fromList . map leAmount <$> iterateStep lastItem
        loadAmounts Null _ _ = return Seq.Empty
        loadAmounts _ Null _ = return Seq.Empty
        loadAmounts (Some st) (Some ed) Null = loadAmountList st ed
        loadAmounts (Some st) (Some ed) (Some v) = do
          first <- loadBufferedRef v
          (first Seq.:<|) <$> loadAmountList st ed

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
          pAggAmount
          put _numAgg
          pSelf
    return (putEnc, dataEnc)

  store a = fst <$> storeUpdate a
  load = do
    _startIdx <- get
    pStart <- load
    pEnd <- load
    pAggAmount <- load
    _numAgg <- get
    pSelf <- load
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
          eData' <- loadPersistentAccountEncryptedAmount eData
          let _accountHash = makeAccountHash _accountNonce _accountAmount eData' pData
          return PersistentAccount {..}

instance HashableTo Hash.Hash PersistentAccount where
  getHash = _accountHash

instance Monad m => MHashableTo m Hash.Hash PersistentAccount

-- |Make a 'PersistentAccount' from an 'Transient.Account'.
makePersistentAccount :: MonadBlobStore r m => Transient.Account -> m PersistentAccount
makePersistentAccount Transient.Account{..} = do
  let _accountHash = makeAccountHash _accountNonce _accountAmount _accountEncryptedAmount _accountPersisting
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
  eac <- loadPersistentAccountEncryptedAmount =<< loadBufferedRef _accountEncryptedAmount
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
