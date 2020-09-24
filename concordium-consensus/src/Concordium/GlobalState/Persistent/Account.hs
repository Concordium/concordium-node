{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Concordium.GlobalState.Persistent.Account where

import Data.Serialize
import Lens.Micro.Platform
import Data.Word
import Control.Monad

import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Types.HashableTo
import qualified Concordium.GlobalState.Basic.BlockState.Account as Transient
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types hiding (_incomingEncryptedAmounts, _startIndex, _selfAmount, _aggregatedAmount)
import qualified Concordium.Types as TY (_incomingEncryptedAmounts, _startIndex, _selfAmount, _aggregatedAmount)
import Concordium.GlobalState.Account hiding (addIncomingEncryptedAmount, addToSelfEncryptedAmount)
import Concordium.Crypto.EncryptedTransfers
import qualified Data.Sequence as Seq
import Data.Maybe (isNothing)

-- | The persistent version of the encrypted amount structure per account.
data PersistentAccountEncryptedAmount = PersistentAccountEncryptedAmount {
  -- | Encrypted amount that is a result of this accounts' actions.
  -- In particular this list includes the aggregate of
  --
  -- - remaining amounts that result when transfering to public balance
  -- - remaining amounts when transfering to another account
  -- - encrypted amounts that are transfered from public balance
  --
  -- When a transfer is made all of these must always be used.
  _selfAmount :: !(BufferedRef EncryptedAmount),
  -- | Starting index for incoming encrypted amounts. If an aggregated amount is present
  -- then this index is associated with such an amount and the list of incoming encrypted amounts
  -- starts at the index @_startIndex + 1@.
  _startIndex :: !EncryptedAmountAggIndex,
  -- | Amounts starting at @startIndex@ (or at @startIndex + 1@ if there is an aggregated amount present).
  -- They are assumed to be numbered sequentially. This list will never contain more than 'maxNumIncoming'
  -- (or @maxNumIncoming - 1@ if there is an aggregated amount present) values.
  _incomingEncryptedAmounts :: !(Seq.Seq (BufferedRef EncryptedAmount)),
  -- |If 'Just', the amount that has resulted from aggregating other amounts and the
  -- number of aggregated amounts (must be at least 2 if present).
  _aggregatedAmount :: !(Maybe (BufferedRef EncryptedAmount, Word32))
  } deriving Show

-- | Create an empty PersistentAccountEncryptedAmount
initialPersistentAccountEncryptedAmount :: MonadBlobStore r m => m PersistentAccountEncryptedAmount
initialPersistentAccountEncryptedAmount = do
  _selfAmount <- makeBufferedRef mempty
  return PersistentAccountEncryptedAmount {
    _startIndex = 0,
    _incomingEncryptedAmounts = Seq.Empty,
    _aggregatedAmount = Nothing,
    ..
    }

-- | Given an AccountEncryptedAmount, create a Persistent version of it
storePersistentAccountEncryptedAmount :: MonadBlobStore r m => AccountEncryptedAmount -> m PersistentAccountEncryptedAmount
storePersistentAccountEncryptedAmount AccountEncryptedAmount{..} = do
  _selfAmount <- makeBufferedRef _selfAmount
  _incomingEncryptedAmounts <- mapM makeBufferedRef _incomingEncryptedAmounts
  _aggregatedAmount <- case _aggregatedAmount of
                        Nothing -> return Nothing
                        Just (e, n) -> Just . (,n) <$> makeBufferedRef e
  return PersistentAccountEncryptedAmount{..}

-- | Given a PersistentAccountEncryptedAmount, load its equivalent AccountEncryptedAmount
loadPersistentAccountEncryptedAmount :: MonadBlobStore r m => PersistentAccountEncryptedAmount -> m AccountEncryptedAmount
loadPersistentAccountEncryptedAmount PersistentAccountEncryptedAmount{..} = do
  _selfAmount <- loadBufferedRef _selfAmount
  _incomingEncryptedAmounts <- mapM loadBufferedRef _incomingEncryptedAmounts
  _aggregatedAmount <- case _aggregatedAmount of
                        Nothing -> return Nothing
                        Just (e, n) -> Just . (,n) <$> loadBufferedRef e
  return AccountEncryptedAmount{..}

instance MonadBlobStore r m => BlobStorable r m PersistentAccountEncryptedAmount where
  storeUpdate PersistentAccountEncryptedAmount{..} = do
    (pSelf, _selfAmount) <- storeUpdate _selfAmount
    (pAmounts, _incomingEncryptedAmounts) <- Seq.unzip <$> mapM storeUpdate _incomingEncryptedAmounts
    (pAgg, _aggregatedAmount) <- case _aggregatedAmount of
      Nothing -> return (putWord32be  0, _aggregatedAmount)
      Just (e, n) -> do
        (pE, newE) <- storeUpdate e
        return (putWord32be n >> pE, Just (newE, n))
    return . (, PersistentAccountEncryptedAmount {..}) $ do
      pSelf
      put _startIndex
      putWord32be $ fromIntegral $ length pAmounts
      sequence_ pAmounts
      pAgg

  store a = fst <$> storeUpdate a
  load = do
    pSelf <- load
    _startIndex <- get
    numAmounts <- fromIntegral <$> getWord32be
    pAmounts <- replicateM numAmounts load
    numAggregatedAmount <- getWord32be
    agg <- case numAggregatedAmount of
      0 -> return Nothing
      n | n > 1 -> Just <$> load
        | otherwise -> fail "Number of aggregated amounts cannot be 1"
    return $ do
      _selfAmount <- pSelf
      _incomingEncryptedAmounts <- Seq.fromList <$> sequence pAmounts
      _aggregatedAmount <- case agg of
        Just v -> do
          vVal <- v
          return $ Just (vVal, numAggregatedAmount)
        Nothing -> return Nothing
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

instance MonadBlobStore m => BlobStorable m PersistentAccount where
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
        mAccDataPtr <- load
        mAccountEncryptedAmountPtr <- load
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
sameAccount :: MonadBlobStore m => Transient.Account -> PersistentAccount -> m Bool
sameAccount bAcc pAcc@PersistentAccount{..} = do
  _accountPersisting <- loadBufferedRef _persistingData
  _accountEncryptedAmount <- loadPersistentAccountEncryptedAmount =<< loadBufferedRef _accountEncryptedAmount
  return $ sameAccountHash bAcc pAcc && Transient.Account{..} == bAcc

-- |Checks whether the two arguments represent the same account by comparing the account hashes.
-- (Used for testing.)
sameAccountHash :: Transient.Account -> PersistentAccount -> Bool
sameAccountHash bAcc pAcc = getHash bAcc == _accountHash pAcc

-- |Load a field from an account's 'PersistingAccountData' pointer. E.g., @acc ^^. accountAddress@ returns the account's address.
(^^.) :: MonadBlobStore m
      => PersistentAccount
      -> Getting b PersistingAccountData b
      -> m b
acc ^^. l = (^. l) <$> loadBufferedRef (acc ^. persistingData)

{-# INLINE (^^.) #-}
infixl 8 ^^.

-- |Update a field of an account's 'PersistingAccountData' pointer, creating a new pointer.
-- Used to implement '.~~' and '%~~'.
setPAD :: MonadBlobStore m
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
(.~~) :: MonadBlobStore m
      => ASetter PersistingAccountData PersistingAccountData a b
      -> b
      -> PersistentAccount
      -> m PersistentAccount
(.~~) l v = setPAD (l .~ v)

{-# INLINE (.~~) #-}
infixr 4 .~~

-- |Modify a field of an account's 'PersistingAccountData' pointer, creating a new pointer.
-- E.g., @acc & accountInstances %~~ Set.insert i@ inserts an instance @i@ to the set of an account's instances.
(%~~) :: MonadBlobStore m
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
addIncomingEncryptedAmount newAmount old = do
  newAmountRef <- makeBufferedRef newAmount
  case _aggregatedAmount old of
      Nothing -> -- we need to aggregate if we have 'maxNumIncoming' or more incoming amounts
        if Seq.length (_incomingEncryptedAmounts old) >= maxNumIncoming then do
          -- irrefutable because of check above
          let ~(x Seq.:<| y Seq.:<| rest) = _incomingEncryptedAmounts old
          xVal <- loadBufferedRef x
          yVal <- loadBufferedRef y
          xPlusY <- makeBufferedRef (xVal <> yVal)
          return old{_incomingEncryptedAmounts = rest Seq.|> newAmountRef,
                     _aggregatedAmount = Just (xPlusY, 2),
                     _startIndex = _startIndex old + 1
                    }
        else return $ old {_incomingEncryptedAmounts = _incomingEncryptedAmounts old Seq.|> newAmountRef}
      Just (e, n) -> do -- we have to aggregate always
        -- irrefutable because of check above
        let ~(x Seq.:<| rest) = _incomingEncryptedAmounts old
        xVal <- loadBufferedRef x
        aggVal <- loadBufferedRef e
        xPlusY <- makeBufferedRef (aggVal <> xVal)
        return old{_incomingEncryptedAmounts = rest Seq.|> newAmountRef,
                   _aggregatedAmount = Just (xPlusY, n + 1),
                   _startIndex = _startIndex old + 1
                  }

-- | Drop the encrypted amount with indices up to (but not including) the given one, and add the new amount at the end.
-- This is used when an account is transfering from from an encrypted balance, and the newly added
-- amount is the remaining balance that was not used.
--
-- As mentioned above, the whole 'selfBalance' must always be used in any
-- outgoing action of the account.
replaceUpTo :: MonadBlobStore r m => EncryptedAmountAggIndex -> EncryptedAmount -> PersistentAccountEncryptedAmount -> m PersistentAccountEncryptedAmount
replaceUpTo newIndex newAmount PersistentAccountEncryptedAmount{..} = do
  _selfAmount <- makeBufferedRef newAmount
  return PersistentAccountEncryptedAmount{
    _startIndex = newStartIndex,
    _incomingEncryptedAmounts = newEncryptedAmounts,
    _aggregatedAmount = newAggregatedAmount,
    ..
  }
  where (newStartIndex, toDrop, dropAggregated) =
          if newIndex > _startIndex
          then
            if isNothing _aggregatedAmount
            then
              (newIndex, fromIntegral (newIndex - _startIndex), False)
            else
              (newIndex, fromIntegral (newIndex - _startIndex) - 1, True)
          else (_startIndex, 0, False)
        newEncryptedAmounts = Seq.drop toDrop _incomingEncryptedAmounts
        newAggregatedAmount = if dropAggregated then Nothing else _aggregatedAmount

-- | Add the given encrypted amount to 'selfAmount'
-- This is used when the account is transferring from public to secret balance.
addToSelfEncryptedAmount :: MonadBlobStore r m => EncryptedAmount -> PersistentAccountEncryptedAmount -> m PersistentAccountEncryptedAmount
addToSelfEncryptedAmount newAmount old@PersistentAccountEncryptedAmount{..} = do
  newSelf <- makeBufferedRef . (<> newAmount) =<< loadBufferedRef _selfAmount
  return old{_selfAmount = newSelf}
