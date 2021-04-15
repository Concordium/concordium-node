{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Concordium.GlobalState.Persistent.Account where

import Data.Serialize
import Lens.Micro.Platform
import Data.Word
import Control.Monad
import qualified Data.Sequence as Seq
import Data.Maybe (isNothing)
import qualified Data.Map.Strict as Map

import Concordium.Utils.Serialization
import Concordium.Utils.Serialization.Put
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.Crypto.EncryptedTransfers
import Concordium.Types.HashableTo
import Concordium.Types hiding (_incomingEncryptedAmounts, _startIndex, _selfAmount, _aggregatedAmount)
import Concordium.Constants
import qualified Concordium.Types as TY (_incomingEncryptedAmounts, _startIndex, _selfAmount, _aggregatedAmount)
import Concordium.ID.Types
import Concordium.ID.Parameters

import qualified Concordium.GlobalState.Basic.BlockState.Account as Transient
import qualified Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule as Transient
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState.AccountReleaseSchedule
import Concordium.GlobalState.Account hiding (addIncomingEncryptedAmount, addToSelfEncryptedAmount, _stakedAmount, _stakeEarnings, _accountBakerInfo, _bakerPendingChange, stakedAmount, stakeEarnings, accountBakerInfo, bakerPendingChange)
import Concordium.GlobalState.BakerInfo

-- | The persistent version of the encrypted amount structure per account.
data PersistentAccountEncryptedAmount = PersistentAccountEncryptedAmount {
  -- | Encrypted amount that is a result of this accounts' actions.
  -- In particular this list includes the aggregate of
  --
  -- - remaining amounts that result when transfering to public balance
  -- - remaining amounts when transfering to another account
  -- - encrypted amounts that are transferred from public balance
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
initialPersistentAccountEncryptedAmount :: MonadBlobStore m => m PersistentAccountEncryptedAmount
initialPersistentAccountEncryptedAmount = do
  _selfAmount <- makeBufferedRef mempty
  return PersistentAccountEncryptedAmount {
    _startIndex = 0,
    _incomingEncryptedAmounts = Seq.Empty,
    _aggregatedAmount = Nothing,
    ..
    }

-- |Serialize a 'PersistentAccountEncryptedAmount' if it is not the empty
-- encrypted amount (in which case, @Nothing@ is returned).
--
-- This should match the serialization format of 'AccountEncryptedAmount' exactly.
putAccountEncryptedAmountV0 :: (MonadBlobStore m) => PersistentAccountEncryptedAmount -> m (Maybe Put)
putAccountEncryptedAmountV0 PersistentAccountEncryptedAmount{..} = do
    sAmt <- refLoad _selfAmount
    if isZeroEncryptedAmount sAmt && _startIndex == 0 && Seq.null _incomingEncryptedAmounts && isNothing _aggregatedAmount then
      return Nothing
    else do
      ieas <- mapM refLoad _incomingEncryptedAmounts
      putAgg <- case _aggregatedAmount of
          Nothing -> return $ putWord32be 0
          Just (eref, n) -> do
            e <- refLoad eref
            return $ do
              putWord32be n
              put e
      return . Just $ do
        put sAmt
        put _startIndex
        putWord32be (fromIntegral (Seq.length ieas))
        mapM_ put ieas
        putAgg

-- | Given an AccountEncryptedAmount, create a Persistent version of it
storePersistentAccountEncryptedAmount :: MonadBlobStore m => AccountEncryptedAmount -> m PersistentAccountEncryptedAmount
storePersistentAccountEncryptedAmount AccountEncryptedAmount{..} = do
  _selfAmount <- makeBufferedRef _selfAmount
  _incomingEncryptedAmounts <- mapM makeBufferedRef _incomingEncryptedAmounts
  _aggregatedAmount <- case _aggregatedAmount of
                        Nothing -> return Nothing
                        Just (e, n) -> Just . (,n) <$> makeBufferedRef e
  return PersistentAccountEncryptedAmount{..}

-- | Given a PersistentAccountEncryptedAmount, load its equivalent AccountEncryptedAmount
loadPersistentAccountEncryptedAmount :: MonadBlobStore m => PersistentAccountEncryptedAmount -> m AccountEncryptedAmount
loadPersistentAccountEncryptedAmount PersistentAccountEncryptedAmount{..} = do
  _selfAmount <- loadBufferedRef _selfAmount
  _incomingEncryptedAmounts <- mapM loadBufferedRef _incomingEncryptedAmounts
  _aggregatedAmount <- case _aggregatedAmount of
                        Nothing -> return Nothing
                        Just (e, n) -> Just . (,n) <$> loadBufferedRef e
  return AccountEncryptedAmount{..}

instance MonadBlobStore m => BlobStorable m PersistentAccountEncryptedAmount where
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

instance (MonadBlobStore m) => Cacheable m PersistentAccountEncryptedAmount where
  cache PersistentAccountEncryptedAmount{..} = do
    _selfAmount' <- cache _selfAmount
    _incomingEncryptedAmounts' <- mapM cache _incomingEncryptedAmounts
    _aggregatedAmount' <- mapM (\(a, b) -> (,b) <$> cache a) _aggregatedAmount
    return PersistentAccountEncryptedAmount{
      _selfAmount = _selfAmount',
      _incomingEncryptedAmounts = _incomingEncryptedAmounts',
      _aggregatedAmount = _aggregatedAmount',
      ..
    }

data PersistentAccountBaker = PersistentAccountBaker
  { _stakedAmount :: !Amount
  , _stakeEarnings :: !Bool
  , _accountBakerInfo :: !(BufferedRef BakerInfo)
  , _bakerPendingChange :: !BakerPendingChange
  }
  deriving (Show)

makeLenses ''PersistentAccountBaker

instance MonadBlobStore m => MHashableTo m AccountBakerHash PersistentAccountBaker where
  getHashM PersistentAccountBaker{..} = do
    abi <- loadBufferedRef _accountBakerInfo
    return $ makeAccountBakerHash _stakedAmount _stakeEarnings abi _bakerPendingChange

instance MonadBlobStore m => BlobStorable m PersistentAccountBaker where
  storeUpdate PersistentAccountBaker{..} = do
    (pBakerInfo, newBakerInfo) <- storeUpdate _accountBakerInfo
    return . (, PersistentAccountBaker{_accountBakerInfo = newBakerInfo,..}) $ do
      put _stakedAmount
      put _stakeEarnings
      pBakerInfo
      put _bakerPendingChange
  store a = fst <$> storeUpdate a
  load = do
    _stakedAmount <- get
    _stakeEarnings <- get
    rBakerInfo <- load
    _bakerPendingChange <- get
    return $ do
      _accountBakerInfo <- rBakerInfo
      return PersistentAccountBaker{..}

instance MonadBlobStore m => Cacheable m PersistentAccountBaker where
  cache pab = do
    cachedBaker <- cache (_accountBakerInfo pab)
    return pab{_accountBakerInfo = cachedBaker}

-- |Get the hash of an account's baker.
hashAccountBaker :: MonadBlobStore m => Nullable (BufferedRef PersistentAccountBaker) -> m AccountBakerHash
{-# INLINE hashAccountBaker #-}
hashAccountBaker Null = return nullAccountBakerHash
hashAccountBaker (Some r) = getHashM r

-- |Serialize a 'PersistentAccountBaker'.
putAccountBakerV0 :: (MonadBlobStore m, MonadPut m) => PersistentAccountBaker -> m ()
putAccountBakerV0 PersistentAccountBaker{..} = do
    abi <- refLoad _accountBakerInfo
    liftPut $ do
      put _stakedAmount
      put _stakeEarnings
      put abi
      put _bakerPendingChange

-- |Type for a reference to an account's persisting data.
type AccountPersisting (pv :: ProtocolVersion) = HashedBufferedRef (PersistingAccountData pv)

data PersistentAccount (pv :: ProtocolVersion) = PersistentAccount {
  -- |Next available nonce for this account.
  _accountNonce :: !Nonce
  -- |Current public account balance.
  ,_accountAmount :: !Amount
  -- |List of encrypted amounts on the account.
  ,_accountEncryptedAmount :: !(BufferedRef PersistentAccountEncryptedAmount)
  -- |Schedule of releases on the account.
  ,_accountReleaseSchedule :: !(BufferedRef AccountReleaseSchedule)
  -- |A pointer to account data that changes rarely
  ,_persistingData :: !(AccountPersisting pv)
  -- |The baker info
  ,_accountBaker :: !(Nullable (BufferedRef PersistentAccountBaker))
  -- |A hash of all account data. We store the hash explicitly here because we cannot compute the hash once
  -- the persisting account data is stored behind a pointer
  ,_accountHash :: !Hash.Hash
  }

makeLenses ''PersistentAccount

deriving instance (IsProtocolVersion pv) => Show (PersistentAccount pv)

instance (MonadBlobStore m, IsProtocolVersion pv) => BlobStorable m (PersistentAccount pv) where
    storeUpdate PersistentAccount{..} = do
        (pAccData :: Put, accData) <- storeUpdate _persistingData
        (pEnc, encData) <- storeUpdate _accountEncryptedAmount
        (pSched, schedData) <- storeUpdate _accountReleaseSchedule
        (pBkr, bkrData) <- storeUpdate _accountBaker
        let persistentAcc = PersistentAccount {
                _persistingData = accData,
                _accountEncryptedAmount = encData,
                _accountReleaseSchedule = schedData,
                _accountBaker = bkrData,
                ..
            }
        let putAccs = do
                    put _accountNonce
                    put _accountAmount
                    pAccData
                    pEnc
                    pSched
                    pBkr
        return (putAccs, persistentAcc)
    store a = fst <$> storeUpdate a
    load = do
        _accountNonce <- get
        _accountAmount <- get
        mAccDataPtr <- load
        mAccountEncryptedAmountPtr <- load
        mAccountReleaseSchedulePtr <- load
        mAccountBaker <- load
        return $ do
          _persistingData <- cache =<< mAccDataPtr
          _accountEncryptedAmount <- cache =<< mAccountEncryptedAmountPtr
          _accountReleaseSchedule <- cache =<< mAccountReleaseSchedulePtr
          _accountBaker <- cache =<< mAccountBaker
          
          eData <- loadBufferedRef _accountEncryptedAmount
          eData' <- loadPersistentAccountEncryptedAmount eData
          (sHash :: Transient.AccountReleaseScheduleHash) <- getHashM =<< loadBufferedRef _accountReleaseSchedule
          bakerHash <- hashAccountBaker _accountBaker
          _accountHash <- makeAccountHash (protocolVersion @pv) _accountNonce _accountAmount eData' sHash _persistingData bakerHash
          return PersistentAccount {..}

instance (MonadBlobStore m, IsProtocolVersion pv) => Cacheable m (PersistentAccount pv) where
    cache pa@PersistentAccount{..} = do
        _accountEncryptedAmount' <- cache _accountEncryptedAmount
        _persistingData' <- cache _persistingData
        return pa{
          _accountEncryptedAmount = _accountEncryptedAmount',
          _persistingData = _persistingData'
        }

instance HashableTo Hash.Hash (PersistentAccount pv) where
  getHash = _accountHash

instance Monad m => MHashableTo m Hash.Hash (PersistentAccount pv)

-- |Create an empty account with the given public key, address and credential.
newAccount :: forall m pv. (MonadBlobStore m, IsProtocolVersion pv)
    => GlobalContext -> AccountAddress -> AccountCredential -> m (PersistentAccount pv)
newAccount cryptoParams _accountAddress credential = do
  let creds = Map.singleton initialCredentialIndex credential
  let newPData = PersistingAccountData {
        _accountEncryptionKey = makeEncryptionKey cryptoParams (credId credential),
        _accountCredentials = creds,
        _accountVerificationKeys = getAccountInformation 1 creds,
        _accountRemovedCredentials = emptyHashedRemovedCredentials,
        ..
        } :: PersistingAccountData pv
  _persistingData <- refMake newPData
  let _accountNonce = minNonce
      _accountAmount = 0
  accountEncryptedAmountData <- initialPersistentAccountEncryptedAmount
  baseEncryptedAmountData <- loadPersistentAccountEncryptedAmount accountEncryptedAmountData
  _accountEncryptedAmount <- makeBufferedRef accountEncryptedAmountData
  let relSched = emptyAccountReleaseSchedule
  _accountReleaseSchedule <- makeBufferedRef relSched
  (arsHash :: Transient.AccountReleaseScheduleHash) <- getHashM relSched
  _accountHash <- makeAccountHash
      (protocolVersion @pv)
      _accountNonce
      _accountAmount
      baseEncryptedAmountData
      arsHash
      _persistingData
      nullAccountBakerHash
  return PersistentAccount {
        _accountBaker = Null,
        ..
    }

-- |Make a 'PersistentAccount' from an 'Transient.Account'.
makePersistentAccount :: (MonadBlobStore m, IsProtocolVersion pv) => Transient.Account pv -> m (PersistentAccount pv)
makePersistentAccount tacc@Transient.Account{..} = do
  let _accountHash = getHash tacc
  _persistingData <- refMake (tacc ^. persistingAccountData)
  _accountEncryptedAmount' <- makeBufferedRef =<< storePersistentAccountEncryptedAmount _accountEncryptedAmount
  _accountReleaseSchedule' <- makeBufferedRef =<< storePersistentAccountReleaseSchedule _accountReleaseSchedule
  _accountBaker <- case _accountBaker of
    Nothing -> return Null
    Just Transient.AccountBaker{..} -> do
      abiRef <- makeBufferedRef _accountBakerInfo
      Some <$> makeBufferedRef PersistentAccountBaker{_accountBakerInfo = abiRef, ..}
  return PersistentAccount {_accountEncryptedAmount = _accountEncryptedAmount', _accountReleaseSchedule = _accountReleaseSchedule', ..}

-- |Generate the hash of an account.
makeAccountHash :: (MonadBlobStore m) => SProtocolVersion pv -> Nonce -> Amount -> AccountEncryptedAmount -> Transient.AccountReleaseScheduleHash -> AccountPersisting pv -> AccountBakerHash -> m Hash.Hash
makeAccountHash SP1 n a eas ars pd abh = do
  pdHash <- getHashM pd
  return $ makeAccountHashP1 n a eas ars pdHash abh


-- |Recompute the hash of an account.
rehashAccount :: forall m pv. (MonadBlobStore m, IsProtocolVersion pv) => PersistentAccount pv -> m (PersistentAccount pv)
rehashAccount pac = do
  eac <- loadPersistentAccountEncryptedAmount =<< loadBufferedRef (_accountEncryptedAmount pac)
  sdata <- refLoad (_accountReleaseSchedule pac)
  arsHash <- getHashM sdata
  bkrHash <- hashAccountBaker (_accountBaker pac)
  newHash <- makeAccountHash
      (protocolVersion @pv)
      (_accountNonce pac)
      (_accountAmount pac)
      eac
      arsHash
      (_persistingData pac)
      bkrHash
  return pac{_accountHash = newHash}

-- |Set the baker of an account.
setPersistentAccountBaker :: forall m pv. (MonadBlobStore m, IsProtocolVersion pv) => PersistentAccount pv-> Nullable PersistentAccountBaker -> m (PersistentAccount pv)
setPersistentAccountBaker pac pab = do
  eac <- loadPersistentAccountEncryptedAmount =<< loadBufferedRef (_accountEncryptedAmount pac)
  sdata <- refLoad (_accountReleaseSchedule pac)
  arsHash <- getHashM sdata
  pabRef <- mapM refMake pab
  bkrHash <- hashAccountBaker pabRef
  newHash <- makeAccountHash
      (protocolVersion @pv)
      (_accountNonce pac)
      (_accountAmount pac)
      eac
      arsHash
      (_persistingData pac)
      bkrHash
  return pac{_accountHash = newHash, _accountBaker = pabRef}

-- |Checks whether the two arguments represent the same account. (Used for testing.)
sameAccount :: forall m pv. (MonadBlobStore m, IsProtocolVersion pv) => Transient.Account pv -> PersistentAccount pv -> m Bool
sameAccount bAcc pAcc@PersistentAccount{..} = do
  _accountPersisting <- Transient.makeAccountPersisting <$> refLoad _persistingData
  _accountEncryptedAmount <- loadPersistentAccountEncryptedAmount =<< loadBufferedRef _accountEncryptedAmount
  _accountReleaseSchedule <- loadPersistentAccountReleaseSchedule =<< loadBufferedRef _accountReleaseSchedule
  _accountBaker <- case _accountBaker of
    Null -> return Nothing
    Some ref -> do
      PersistentAccountBaker{..} <- loadBufferedRef ref
      _accountBakerInfo <- loadBufferedRef _accountBakerInfo
      return $ Just Transient.AccountBaker{..}
  return $ sameAccountHash bAcc pAcc && Transient.Account{..} == bAcc

-- |Checks whether the two arguments represent the same account by comparing the account hashes.
-- (Used for testing.)
sameAccountHash :: IsProtocolVersion pv => Transient.Account pv -> PersistentAccount pv -> Bool
sameAccountHash bAcc pAcc = getHash bAcc == _accountHash pAcc

-- |Load a field from an account's 'PersistingAccountData' pointer. E.g., @acc ^^. accountAddress@ returns the account's address.
(^^.) :: (MonadBlobStore m, IsProtocolVersion pv)
      => PersistentAccount pv
      -> Getting b (PersistingAccountData pv) b
      -> m b
acc ^^. l = (^. l) <$> refLoad (acc ^. persistingData)

{-# INLINE (^^.) #-}
infixl 8 ^^.

-- |Update a field of an account's 'PersistingAccountData' pointer, creating a new pointer.
-- Used to implement '.~~' and '%~~'.
setPAD :: forall m pv. (MonadBlobStore m, IsProtocolVersion pv)
          => (PersistingAccountData pv -> PersistingAccountData pv)
          -> PersistentAccount pv
          -> m (PersistentAccount pv)
setPAD f acc@PersistentAccount{..} = do
  pData <- refLoad (acc ^. persistingData)
  eac <- loadPersistentAccountEncryptedAmount =<< loadBufferedRef _accountEncryptedAmount
  rs <- getHashM =<< refLoad _accountReleaseSchedule
  let newPData = f pData
  newPDataRef <- refMake newPData
  bkrHash <- hashAccountBaker _accountBaker
  newAccountHash <- makeAccountHash (protocolVersion @pv) _accountNonce _accountAmount eac rs newPDataRef bkrHash
  return $ acc & persistingData .~ newPDataRef
               & accountHash .~ newAccountHash

-- |Set a field of an account's 'PersistingAccountData' pointer, creating a new pointer.
-- E.g., @acc & accountStakeDelegate .~~ Nothing@ sets the
-- account's stake delegate to 'Nothing'.
(.~~) :: (MonadBlobStore m, IsProtocolVersion pv)
      => ASetter (PersistingAccountData pv) (PersistingAccountData pv) a b
      -> b
      -> PersistentAccount pv
      -> m (PersistentAccount pv)
(.~~) l v = setPAD (l .~ v)

{-# INLINE (.~~) #-}
infixr 4 .~~

-- |Modify a field of an account's 'PersistingAccountData' pointer, creating a new pointer.
-- E.g., @acc & accountInstances %~~ Set.insert i@ inserts an instance @i@ to the set of an account's instances.
(%~~) :: (MonadBlobStore m, IsProtocolVersion pv)
      => ASetter (PersistingAccountData pv) (PersistingAccountData pv) a b
      -> (a -> b)
      -> PersistentAccount pv
      -> m (PersistentAccount pv)
(%~~) l f = setPAD (l %~ f)

{-# INLINE (%~~) #-}
infixr 4 %~~

-- | Add an encrypted amount to the end of the list.
-- This is used when an incoming transfer is added to the account. If this would
-- go over the threshold for the maximum number of incoming amounts then
-- aggregate the first two incoming amounts.
addIncomingEncryptedAmount :: MonadBlobStore m => EncryptedAmount -> PersistentAccountEncryptedAmount -> m PersistentAccountEncryptedAmount
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
replaceUpTo :: MonadBlobStore m => EncryptedAmountAggIndex -> EncryptedAmount -> PersistentAccountEncryptedAmount -> m PersistentAccountEncryptedAmount
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
addToSelfEncryptedAmount :: MonadBlobStore m => EncryptedAmount -> PersistentAccountEncryptedAmount -> m PersistentAccountEncryptedAmount
addToSelfEncryptedAmount newAmount old@PersistentAccountEncryptedAmount{..} = do
  newSelf <- makeBufferedRef . (<> newAmount) =<< loadBufferedRef _selfAmount
  return old{_selfAmount = newSelf}


-- * Serialization

-- |Serialize an account. The serialization format may depend on the protocol version.
--
-- This format allows accounts to be stored in a reduced format by
-- eliding (some) data that can be inferred from context, or is
-- the default value.  Note that there can be multiple representations
-- of the same account.
-- This format does not store the smart contract instances, which are
-- implied by the instance table.
serializeAccount :: forall m pv. (MonadBlobStore m, MonadPut m, IsProtocolVersion pv) => GlobalContext -> PersistentAccount pv -> m ()
serializeAccount cryptoParams PersistentAccount{..} = do
    PersistingAccountData {..} <- refLoad _persistingData
    let
        initialCredId = credId (Map.findWithDefault
                (error "Account missing initial credential")
                initialCredentialIndex
                _accountCredentials
              )
        asfExplicitAddress = _accountAddress /= addressFromRegId initialCredId
        asfExplicitEncryptionKey = _accountEncryptionKey /= makeEncryptionKey cryptoParams initialCredId
        (asfMultipleCredentials, putCredentials) = case Map.toList _accountCredentials of
          [(i, cred)] | i == initialCredentialIndex -> (False, put cred)
          _ -> (True, putSafeMapOf put put _accountCredentials)
        asfThresholdIsOne = aiThreshold _accountVerificationKeys == 1
        asfHasRemovedCredentials = _accountRemovedCredentials ^. unhashed /= EmptyRemovedCredentials
    aea <- refLoad _accountEncryptedAmount
    (asfExplicitEncryptedAmount, putEA) <- putAccountEncryptedAmountV0 aea <&> \case
        Nothing -> (False, return ())
        Just p -> (True, p)
    arSched <- loadPersistentAccountReleaseSchedule =<< refLoad _accountReleaseSchedule
    let
        asfExplicitReleaseSchedule = arSched /= Transient.emptyAccountReleaseSchedule
        asfHasBaker = case _accountBaker of {Null -> False; _ -> True}
    liftPut $ do
        put AccountSerializationFlags {..}
        when asfExplicitAddress $ put _accountAddress
        when asfExplicitEncryptionKey $ put _accountEncryptionKey
        unless asfThresholdIsOne $ put (aiThreshold _accountVerificationKeys)
        putCredentials
        when asfHasRemovedCredentials $ put (_accountRemovedCredentials ^. unhashed)
        put _accountNonce
        put _accountAmount
        putEA
        when asfExplicitReleaseSchedule $ put arSched
    forM_ _accountBaker $ refLoad >=> putAccountBakerV0
