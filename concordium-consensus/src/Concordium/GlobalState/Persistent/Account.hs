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
import Concordium.Types hiding (_incomingEncryptedAmounts, _startIndex, _selfAmount, _numAggregated)
import qualified Concordium.Types as TY (_incomingEncryptedAmounts, _startIndex, _selfAmount, _numAggregated)
import Concordium.GlobalState.Account hiding (addIncomingEncryptedAmount, addToSelfEncryptedAmount)
import Concordium.Crypto.EncryptedTransfers
import qualified Data.Sequence as Seq

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
  -- | Starting index for incoming encrypted amounts.
  _startIndex :: !EncryptedAmountAggIndex,
  -- | Amounts starting at @startIndex@. They are assumed to be numbered sequentially.
  -- This list will never contain more than 'maxNumIncoming' values.
  _incomingEncryptedAmounts :: !(Seq.Seq (BufferedRef EncryptedAmount)),
  -- |If 'Just', the number of incoming amounts that have been aggregated. In
  -- that case the number is always >= 2.
  _numAggregated :: !(Maybe Word32)
  } deriving Show

-- | Create an empty PersistentAccountEncryptedAmount
initialPersistentAccountEncryptedAmount :: MonadBlobStore r m => m PersistentAccountEncryptedAmount
initialPersistentAccountEncryptedAmount = do
  _selfAmount <- makeBufferedRef mempty
  return PersistentAccountEncryptedAmount {
    _startIndex = 0,
    _incomingEncryptedAmounts = Seq.Empty,
    _numAggregated = Nothing,
    ..
    }

-- | Given an AccountEncryptedAmount, create a Persistent version of it
storePersistentAccountEncryptedAmount :: MonadBlobStore r m => AccountEncryptedAmount -> m PersistentAccountEncryptedAmount
storePersistentAccountEncryptedAmount AccountEncryptedAmount{..} = do
  _selfAmount <- makeBufferedRef _selfAmount
  _incomingEncryptedAmounts <- mapM makeBufferedRef _incomingEncryptedAmounts
  return PersistentAccountEncryptedAmount{..}

-- | Given a PersistentAccountEncryptedAmount, load its equivalent AccountEncryptedAmount
loadPersistentAccountEncryptedAmount :: MonadBlobStore r m => PersistentAccountEncryptedAmount -> m AccountEncryptedAmount
loadPersistentAccountEncryptedAmount PersistentAccountEncryptedAmount{..} = do
  _selfAmount <- loadBufferedRef _selfAmount
  _incomingEncryptedAmounts <- mapM loadBufferedRef _incomingEncryptedAmounts
  return AccountEncryptedAmount{..}

instance MonadBlobStore r m => BlobStorable r m PersistentAccountEncryptedAmount where
  storeUpdate PersistentAccountEncryptedAmount{..} = do
    (pSelf, _selfAmount) <- storeUpdate _selfAmount
    (pAmounts, _incomingEncryptedAmounts) <- Seq.unzip <$> mapM storeUpdate _incomingEncryptedAmounts
    return . (, PersistentAccountEncryptedAmount {..}) $ do
      pSelf
      put _startIndex
      putWord32be $ fromIntegral $ length pAmounts
      sequence_ pAmounts
      put _numAggregated

  store a = fst <$> storeUpdate a
  load = do
    pSelf <- load
    _startIndex <- get
    numAmounts <- fromIntegral <$> getWord32be
    pAmounts <- replicateM numAmounts load
    _numAggregated <- get
    return $ do
      _selfAmount <- pSelf
      _incomingEncryptedAmounts <- Seq.fromList <$> sequence pAmounts
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

-- | Add an encrypted amount to the end of the list.
-- This is used when an incoming transfer is added to the account. If this would
-- go over the threshold for the maximum number of incoming amounts then
-- aggregate the first two incoming amounts.
addIncomingEncryptedAmount :: MonadBlobStore r m => EncryptedAmount -> PersistentAccountEncryptedAmount -> m PersistentAccountEncryptedAmount
addIncomingEncryptedAmount newAmount old = do
  newAmountRef <- makeBufferedRef newAmount
  if Seq.length (_incomingEncryptedAmounts old) >= maxNumIncoming then
    case _incomingEncryptedAmounts old of
      x Seq.:<| y Seq.:<| rest -> do
        xVal <- loadBufferedRef x
        yVal <- loadBufferedRef y
        xPlusY <- makeBufferedRef (xVal <> yVal)
        return old{_incomingEncryptedAmounts = (xPlusY Seq.<| rest) Seq.|> newAmountRef,
            _numAggregated = Just (maybe 2 (+1) (_numAggregated old)),
            _startIndex = _startIndex old + 1
            }
      _ -> error "Cannot happen due to check above."
  else return $ old {_incomingEncryptedAmounts = _incomingEncryptedAmounts old Seq.|> newAmountRef}


-- | Drop the encrypted amount with indices up to the given one, and add the new amount at the end.
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
    _numAggregated = newNumAggregated,
    ..
  }
  where (newStartIndex, toDrop) =
          if newIndex > _startIndex
          then (newIndex, fromIntegral (newIndex - _startIndex))
          else (_startIndex, 0)
        newEncryptedAmounts = Seq.drop toDrop _incomingEncryptedAmounts
        newNumAggregated = if toDrop > 0 then Nothing else _numAggregated

-- | Add the given encrypted amount to 'selfAmount'
-- This is used when the account is transferring from public to secret balance.
addToSelfEncryptedAmount :: MonadBlobStore r m => EncryptedAmount -> PersistentAccountEncryptedAmount -> m PersistentAccountEncryptedAmount
addToSelfEncryptedAmount newAmount old@PersistentAccountEncryptedAmount{..} = do
  newSelf <- makeBufferedRef . (<> newAmount) =<< loadBufferedRef _selfAmount
  return old{_selfAmount = newSelf}

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
