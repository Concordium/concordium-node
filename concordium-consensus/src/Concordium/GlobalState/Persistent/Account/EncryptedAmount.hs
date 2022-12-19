{-# LANGUAGE BangPatterns #-}

module Concordium.GlobalState.Persistent.Account.EncryptedAmount where

import Control.Monad
import Data.Maybe
import qualified Data.Sequence as Seq
import Data.Serialize
import Data.Word

import Concordium.Crypto.EncryptedTransfers
import Concordium.Types (
    AccountEncryptedAmount (AccountEncryptedAmount),
 )
import qualified Concordium.Types as Types
import Concordium.Utils

import Concordium.Constants
import Concordium.GlobalState.Persistent.BlobStore

-- * Encrypted amounts

-- | The persistent version of the encrypted amount structure per account.
-- We use 'EagerBufferedRef's for the encrypted amounts so that when a 'PersistentAccount' is
-- loaded, the entire encrypted amount will also be loaded.
-- This is useful, since the encrypted amount structure is used for computing the
-- hash of the account. (See $PersistentAccountCacheable.)
data PersistentAccountEncryptedAmount = PersistentAccountEncryptedAmount
    { -- | Encrypted amount that is a result of this accounts' actions.
      -- In particular this list includes the aggregate of
      --
      -- - remaining amounts that result when transferring to public balance
      -- - remaining amounts when transferring to another account
      -- - encrypted amounts that are transferred from public balance
      --
      -- When a transfer is made all of these must always be used.
      _selfAmount :: !(EagerBufferedRef EncryptedAmount),
      -- | Starting index for incoming encrypted amounts. If an aggregated amount is present
      -- then this index is associated with such an amount and the list of incoming encrypted amounts
      -- starts at the index @_startIndex + 1@.
      _startIndex :: !EncryptedAmountAggIndex,
      -- | Amounts starting at @startIndex@ (or at @startIndex + 1@ if there is an aggregated amount present).
      -- They are assumed to be numbered sequentially. This list will never contain more than 'maxNumIncoming'
      -- (or @maxNumIncoming - 1@ if there is an aggregated amount present) values.
      _incomingEncryptedAmounts :: !(Seq.Seq (EagerBufferedRef EncryptedAmount)),
      -- |If 'Just', the amount that has resulted from aggregating other amounts and the
      -- number of aggregated amounts (must be at least 2 if present).
      _aggregatedAmount :: !(Maybe (EagerBufferedRef EncryptedAmount, Word32))
    }
    deriving (Show)

-- | Create a PersistentAccountEncryptedAmount with the initial, 0 encrypted balance (with
-- randomness 0) and no incoming amounts.
initialPersistentAccountEncryptedAmount :: MonadBlobStore m => m PersistentAccountEncryptedAmount
initialPersistentAccountEncryptedAmount = do
    _selfAmount <- refMake mempty
    return $!
        PersistentAccountEncryptedAmount
            { _startIndex = 0,
              _incomingEncryptedAmounts = Seq.Empty,
              _aggregatedAmount = Nothing,
              ..
            }

-- |Check whether the account encrypted amount is identically the initial encrypted amount.
isInitialPersistentAccountEncryptedAmount :: MonadBlobStore m => PersistentAccountEncryptedAmount -> m Bool
isInitialPersistentAccountEncryptedAmount PersistentAccountEncryptedAmount{..} =
    if _startIndex == 0 && Seq.null _incomingEncryptedAmounts && isNothing _aggregatedAmount
        then isZeroEncryptedAmount <$> refLoad _selfAmount
        else return False

-- Checks whether the account encrypted amount is zero. This checks that there
-- are no incoming amounts, and that the self amount is a specific encryption of
-- 0, with randomness 0.
isZeroPersistentAccountEncryptedAmount :: MonadBlobStore m => PersistentAccountEncryptedAmount -> m Bool
isZeroPersistentAccountEncryptedAmount PersistentAccountEncryptedAmount{..} =
    if Seq.null _incomingEncryptedAmounts && isNothing _aggregatedAmount
        then isZeroEncryptedAmount <$> refLoad _selfAmount
        else return False

-- |Serialize a 'PersistentAccountEncryptedAmount' if it is not the initial
-- encrypted amount (in which case, @Nothing@ is returned).
--
-- This should match the serialization format of 'AccountEncryptedAmount' exactly.
putAccountEncryptedAmountV0 ::
    (MonadBlobStore m) =>
    PersistentAccountEncryptedAmount ->
    m (Maybe Put)
putAccountEncryptedAmountV0 ea@PersistentAccountEncryptedAmount{..} = do
    isInitial <- isInitialPersistentAccountEncryptedAmount ea
    if isInitial
        then return Nothing
        else do
            sAmt <- refLoad _selfAmount
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
storePersistentAccountEncryptedAmount ::
    MonadBlobStore m =>
    AccountEncryptedAmount ->
    m PersistentAccountEncryptedAmount
storePersistentAccountEncryptedAmount AccountEncryptedAmount{..} = do
    _selfAmount <- refMake _selfAmount
    _incomingEncryptedAmounts <- mapM refMake _incomingEncryptedAmounts
    _aggregatedAmount <- case _aggregatedAmount of
        Nothing -> return Nothing
        Just (e, n) -> Just . (,n) <$!> refMake e
    return $! PersistentAccountEncryptedAmount{..}

-- | Given a PersistentAccountEncryptedAmount, load its equivalent AccountEncryptedAmount
loadPersistentAccountEncryptedAmount ::
    MonadBlobStore m =>
    PersistentAccountEncryptedAmount ->
    m AccountEncryptedAmount
loadPersistentAccountEncryptedAmount PersistentAccountEncryptedAmount{..} = do
    _selfAmount <- refLoad _selfAmount
    _incomingEncryptedAmounts <- mapM refLoad _incomingEncryptedAmounts
    _aggregatedAmount <- case _aggregatedAmount of
        Nothing -> return Nothing
        Just (e, n) -> Just . (,n) <$> refLoad e
    return $! AccountEncryptedAmount{..}

instance MonadBlobStore m => BlobStorable m PersistentAccountEncryptedAmount where
    storeUpdate PersistentAccountEncryptedAmount{..} = do
        (pSelf, _selfAmount) <- storeUpdate _selfAmount
        (pAmounts, _incomingEncryptedAmounts) <- Seq.unzip <$> mapM storeUpdate _incomingEncryptedAmounts
        (pAgg, _aggregatedAmount) <- case _aggregatedAmount of
            Nothing -> return (putWord32be 0, _aggregatedAmount)
            Just (e, n) -> do
                (pE, newE) <- storeUpdate e
                return (putWord32be n >> pE, Just (newE, n))
        return . (,PersistentAccountEncryptedAmount{..}) $ do
            pSelf
            put _startIndex
            putWord32be $ fromIntegral $ length pAmounts
            sequence_ pAmounts
            pAgg

    load = do
        pSelf <- load
        _startIndex <- get
        numAmounts <- fromIntegral <$> getWord32be
        pAmounts <- replicateM numAmounts load
        numAggregatedAmount <- getWord32be
        agg <- case numAggregatedAmount of
            0 -> return Nothing
            n
                | n > 1 -> Just <$> load
                | otherwise -> fail "Number of aggregated amounts cannot be 1"
        return $ do
            _selfAmount <- pSelf
            _incomingEncryptedAmounts <- Seq.fromList <$> sequence pAmounts
            _aggregatedAmount <- case agg of
                Just v -> do
                    vVal <- v
                    return $ Just (vVal, numAggregatedAmount)
                Nothing -> return Nothing
            return PersistentAccountEncryptedAmount{..}

instance (MonadBlobStore m) => Cacheable m PersistentAccountEncryptedAmount

-- | Add an encrypted amount to the end of the list.
-- This is used when an incoming transfer is added to the account. If this would
-- go over the threshold for the maximum number of incoming amounts then
-- aggregate the first two incoming amounts.
addIncomingEncryptedAmount ::
    MonadBlobStore m =>
    EncryptedAmount ->
    PersistentAccountEncryptedAmount ->
    m PersistentAccountEncryptedAmount
addIncomingEncryptedAmount !newAmount old = do
    !newAmountRef <- refMake newAmount
    case _aggregatedAmount old of
        Nothing ->
            -- we need to aggregate if we have 'maxNumIncoming' or more incoming amounts
            if Seq.length (_incomingEncryptedAmounts old) >= maxNumIncoming
                then case _incomingEncryptedAmounts old of
                    (x Seq.:<| y Seq.:<| rest) -> do
                        xVal <- refLoad x
                        yVal <- refLoad y
                        xPlusY <- refMake (xVal <> yVal)
                        return $!
                            old
                                { _incomingEncryptedAmounts = rest Seq.|> newAmountRef,
                                  _aggregatedAmount = Just (xPlusY, 2),
                                  _startIndex = _startIndex old + 1
                                }
                    -- this does not happen due to the check above
                    _ -> error "_incomingEncryptedAmounts should have two or more elements."
                else return $ old{_incomingEncryptedAmounts = _incomingEncryptedAmounts old Seq.|> newAmountRef}
        Just (e, n) -> do
            -- we have to aggregate always=
            case _incomingEncryptedAmounts old of
                (x Seq.:<| rest) -> do
                    xVal <- refLoad x
                    aggVal <- refLoad e
                    xPlusY <- refMake (aggVal <> xVal)
                    return $!
                        old
                            { _incomingEncryptedAmounts = rest Seq.|> newAmountRef,
                              _aggregatedAmount = Just $!! (xPlusY, n + 1),
                              _startIndex = _startIndex old + 1
                            }
                -- this does not happen, since if _aggregatedAmount is @Just@, then the length of
                -- `incomingEncryptedAmounts` is 31 or 32, see @AccountEncryptedAmount@.
                Seq.Empty -> error "_incomingEncryptedAmounts should have one or more elements."

-- | Drop the encrypted amount with indices up to (but not including) the given one, and add the new amount at the end.
-- This is used when an account is transfering from from an encrypted balance, and the newly added
-- amount is the remaining balance that was not used.
--
-- As mentioned above, the whole 'selfBalance' must always be used in any
-- outgoing action of the account.
replaceUpTo ::
    MonadBlobStore m =>
    EncryptedAmountAggIndex ->
    EncryptedAmount ->
    PersistentAccountEncryptedAmount ->
    m PersistentAccountEncryptedAmount
replaceUpTo newIndex newAmount PersistentAccountEncryptedAmount{..} = do
    _selfAmount <- refMake newAmount
    return $!
        PersistentAccountEncryptedAmount
            { _startIndex = newStartIndex,
              _incomingEncryptedAmounts = newEncryptedAmounts,
              _aggregatedAmount = newAggregatedAmount,
              ..
            }
  where
    (newStartIndex, toDrop, dropAggregated) =
        if newIndex > _startIndex
            then
                if isNothing _aggregatedAmount
                    then (newIndex, fromIntegral (newIndex - _startIndex), False)
                    else (newIndex, fromIntegral (newIndex - _startIndex) - 1, True)
            else (_startIndex, 0, False)
    newEncryptedAmounts = Seq.drop toDrop _incomingEncryptedAmounts
    newAggregatedAmount = if dropAggregated then Nothing else _aggregatedAmount

-- | Add the given encrypted amount to 'selfAmount'
-- This is used when the account is transferring from public to secret balance.
addToSelfEncryptedAmount ::
    MonadBlobStore m =>
    EncryptedAmount ->
    PersistentAccountEncryptedAmount ->
    m PersistentAccountEncryptedAmount
addToSelfEncryptedAmount newAmount old@PersistentAccountEncryptedAmount{..} = do
    newSelf <- refMake . (<> newAmount) =<< refLoad _selfAmount
    return $! old{_selfAmount = newSelf}

-- |See documentation of @migratePersistentBlockState@.
migratePersistentEncryptedAmount ::
    SupportMigration m t =>
    PersistentAccountEncryptedAmount ->
    t m PersistentAccountEncryptedAmount
migratePersistentEncryptedAmount PersistentAccountEncryptedAmount{..} = do
    newSelfAmount <- migrateEagerBufferedRef return _selfAmount
    newIncomingEncryptedAmounts <- mapM (migrateEagerBufferedRef return) _incomingEncryptedAmounts
    newAggregatedAmount <- mapM (\(ea, numAgg) -> (,numAgg) <$> migrateEagerBufferedRef return ea) _aggregatedAmount
    return $!
        PersistentAccountEncryptedAmount
            { _selfAmount = newSelfAmount,
              _startIndex = _startIndex,
              _incomingEncryptedAmounts = newIncomingEncryptedAmounts,
              _aggregatedAmount = newAggregatedAmount
            }
