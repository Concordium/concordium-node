{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Concordium.GlobalState.Persistent.Bakers where

import Control.Exception
import Control.Monad
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Vector as Vec
import Lens.Micro.Platform
import Data.Serialize
import Data.Foldable (foldlM)

import Concordium.GlobalState.BakerInfo
import qualified Concordium.GlobalState.Basic.BlockState.Bakers as Basic
import qualified Concordium.Types.Accounts as BaseAccounts
import Concordium.GlobalState.Persistent.Account
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types
import Concordium.Types.Execution (DelegationTarget(..))
import Concordium.Utils.BinarySearch
import Concordium.Utils.Serialization

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.Types.HashableTo
import Concordium.Utils.Serialization.Put

-- |A list of references to 'BakerInfo's, ordered by increasing 'BakerId'.
newtype BakerInfos (av :: AccountVersion)
    = BakerInfos (Vec.Vector (PersistentBakerInfoEx av))
    deriving (Show)

instance (IsAccountVersion av, MonadBlobStore m) => BlobStorable m (BakerInfos av) where
    storeUpdate (BakerInfos v) = do
      v' <- mapM storeUpdate v
      let pv = do
              putLength (Vec.length v')
              mapM_ fst v'
      return (pv, BakerInfos (snd <$> v'))
    store bi = fst <$> storeUpdate bi
    load = do
      len <- getLength
      v <- Vec.replicateM len load
      return $ BakerInfos <$> sequence v

-- |This hashing should match (part of) the hashing for 'Basic.EpochBakers'.
instance (IsAccountVersion av, MonadBlobStore m) => MHashableTo m H.Hash (BakerInfos av) where
    getHashM (BakerInfos v) = do
      v' <- mapM loadPersistentBakerInfoEx v
      return $ H.hashLazy $ runPutLazy $ mapM_ put v'

instance (IsAccountVersion av, MonadBlobStore m) => Cacheable m (BakerInfos av) where
    cache (BakerInfos v) = BakerInfos <$> mapM cache v

-- |A list of stakes for bakers.
newtype BakerStakes = BakerStakes (Vec.Vector Amount) deriving (Show)

-- |This hashing should match (part of) the hashing for 'Basic.EpochBakers'.
instance HashableTo H.Hash BakerStakes where
    getHash (BakerStakes v) = H.hashLazy $ runPutLazy $ mapM_ put v

instance Monad m => MHashableTo m H.Hash BakerStakes

instance Serialize BakerStakes where
    put (BakerStakes v) = putLength (Vec.length v) >> mapM_ put v
    get = do
        len <- getLength
        BakerStakes <$> Vec.replicateM len get

instance MonadBlobStore m => BlobStorable m BakerStakes

instance (Applicative m) => Cacheable m BakerStakes

-- |The set of bakers that are eligible to bake in a particular epoch.
--
-- The hashing scheme separately hashes the baker info and baker stakes.
data PersistentEpochBakers (av :: AccountVersion) = PersistentEpochBakers {
    _bakerInfos :: !(HashedBufferedRef (BakerInfos av)),
    _bakerStakes :: !(HashedBufferedRef BakerStakes),
    _bakerTotalStake :: !Amount
} deriving (Show)

makeLenses ''PersistentEpochBakers

epochBaker :: forall m av. (IsAccountVersion av, MonadBlobStore m) => BakerId -> PersistentEpochBakers av -> m (Maybe (BaseAccounts.BakerInfo, Amount))
epochBaker bid PersistentEpochBakers{..} = do
    (BakerInfos infoVec) <- refLoad _bakerInfos
    let loadBakerInfo = refLoad . bakerInfoRef
    minfo <- binarySearchIM loadBakerInfo BaseAccounts._bakerIdentity infoVec bid
    forM minfo $ \(idx, binfo) -> do
        (BakerStakes stakeVec) <- refLoad _bakerStakes
        return (binfo, stakeVec Vec.! idx)

-- |Serialize 'PersistentEpochBakers'.
putEpochBakers :: (IsAccountVersion av, MonadBlobStore m, MonadPut m) => PersistentEpochBakers av -> m ()
putEpochBakers peb = do
        BakerInfos bi <- refLoad (peb ^. bakerInfos)
        bInfos <- mapM (refLoad . bakerInfoRef) bi
        BakerStakes bStakes <- refLoad (peb ^. bakerStakes)
        assert (Vec.length bInfos == Vec.length bStakes) $
            liftPut $ putLength (Vec.length bInfos)
        mapM_ sPut bInfos
        mapM_ (liftPut . put) bStakes

instance (IsAccountVersion av, MonadBlobStore m) => MHashableTo m H.Hash (PersistentEpochBakers av) where
    getHashM PersistentEpochBakers{..} = do
      hbkrInfos <- getHashM _bakerInfos
      hbkrStakes <- getHashM _bakerStakes
      return $ H.hashOfHashes hbkrInfos hbkrStakes

instance (IsAccountVersion av, MonadBlobStore m) => BlobStorable m (PersistentEpochBakers av) where
    storeUpdate PersistentEpochBakers{..} = do
        (pBkrInfos, newBkrInfos) <- storeUpdate _bakerInfos
        (pBkrStakes, newBkrStakes) <- storeUpdate _bakerStakes
        let pBkrs = do
                pBkrInfos
                pBkrStakes
                put _bakerTotalStake
        return (pBkrs, PersistentEpochBakers{_bakerInfos = newBkrInfos, _bakerStakes = newBkrStakes,..})
    store eb = fst <$> storeUpdate eb
    load = do
        mBkrInfos <- load
        mBkrStakes <- load
        _bakerTotalStake <- get
        return $ do
          _bakerInfos <- mBkrInfos
          _bakerStakes <- mBkrStakes
          return PersistentEpochBakers{..}

instance (IsAccountVersion av, MonadBlobStore m) => Cacheable m (PersistentEpochBakers av) where
    cache peb = do
        cBkrInfos <- cache (_bakerInfos peb)
        cBkrStakes <- cache (_bakerStakes peb)
        return peb {_bakerInfos = cBkrInfos, _bakerStakes = cBkrStakes}

-- |Derive a 'FullBakers' from a 'PersistentEpochBakers'.
epochToFullBakers :: (IsAccountVersion av, MonadBlobStore m) => PersistentEpochBakers av -> m FullBakers
epochToFullBakers PersistentEpochBakers{..} = do
    BakerInfos infoRefs <- refLoad _bakerInfos
    infos <- mapM (refLoad . bakerInfoRef) infoRefs
    BakerStakes stakes <- refLoad _bakerStakes
    return FullBakers{
            fullBakerInfos = Vec.zipWith mkFullBakerInfo infos stakes,
            bakerTotalStake = _bakerTotalStake
        }
    where
        mkFullBakerInfo info stake = FullBakerInfo info stake

-- |Derive a 'FullBakers' from a 'PersistentEpochBakers'.
epochToFullBakersEx :: (MonadBlobStore m) => PersistentEpochBakers 'AccountV1 -> m FullBakersEx
epochToFullBakersEx PersistentEpochBakers{..} = do
    BakerInfos infoRefs <- refLoad _bakerInfos
    infos <- mapM loadPersistentBakerInfoEx infoRefs
    BakerStakes stakes <- refLoad _bakerStakes
    return FullBakersEx{
            bakerInfoExs = Vec.zipWith mkFullBakerInfoEx infos stakes,
            bakerPoolTotalStake = _bakerTotalStake
        }
    where
        mkFullBakerInfoEx :: BaseAccounts.BakerInfoEx 'AccountV1 -> Amount -> FullBakerInfoEx
        mkFullBakerInfoEx (BaseAccounts.BakerInfoExV1 info extra) stake =
            FullBakerInfoEx (FullBakerInfo info stake) (extra ^. BaseAccounts.poolCommissionRates)

-- |Derive a 'PersistentEpochBakers' from a 'Basic.EpochBakers'.
makePersistentEpochBakers :: (IsAccountVersion av, MonadBlobStore m) => Basic.EpochBakers av -> m (PersistentEpochBakers av)
makePersistentEpochBakers ebs = do
    _bakerInfos <- refMake . BakerInfos =<< mapM makePersistentBakerInfoEx (Basic._bakerInfos ebs)
    _bakerStakes <- refMake $ BakerStakes (Basic._bakerStakes ebs)
    let _bakerTotalStake = Basic._bakerTotalStake ebs
    return PersistentEpochBakers{..}

type DelegatorIdTrieSet = Trie.TrieN (BufferedBlobbed BlobRef) DelegatorId ()

type BakerIdTrieMap av = Trie.TrieN (BufferedBlobbed BlobRef) BakerId (PersistentActiveDelegators av)

data PersistentActiveDelegators (av :: AccountVersion) where
    PersistentActiveDelegatorsV0 :: PersistentActiveDelegators 'AccountV0
    PersistentActiveDelegatorsV1 ::
        { adDelegators :: !DelegatorIdTrieSet,
          adDelegatorTotalCapital :: !Amount
        } ->
        PersistentActiveDelegators 'AccountV1

emptyPersistentActiveDelegators :: forall av. IsAccountVersion av => PersistentActiveDelegators av
emptyPersistentActiveDelegators =
    case accountVersion @av of
        SAccountV0 -> PersistentActiveDelegatorsV0
        SAccountV1 -> PersistentActiveDelegatorsV1 Trie.empty 0

deriving instance Show (PersistentActiveDelegators av)

instance (IsAccountVersion av, MonadBlobStore m) => BlobStorable m (PersistentActiveDelegators av) where
    storeUpdate PersistentActiveDelegatorsV0 =
        return (return (), PersistentActiveDelegatorsV0)
    storeUpdate PersistentActiveDelegatorsV1{..} = do
        (pDas, newDs) <- storeUpdate adDelegators
        return (pDas <> put adDelegatorTotalCapital, PersistentActiveDelegatorsV1 newDs adDelegatorTotalCapital)
    store a = fst <$> storeUpdate a
    load =
        case accountVersion @av of
            SAccountV0 -> return (return PersistentActiveDelegatorsV0)
            SAccountV1 -> do
                madDelegators <- load
                adDelegatorTotalCapital <- get
                return $ do
                    adDelegators <- madDelegators
                    return PersistentActiveDelegatorsV1{..}

data TotalActiveCapital (av :: AccountVersion) where
    TotalActiveCapitalV0 :: TotalActiveCapital 'AccountV0
    TotalActiveCapitalV1 :: !Amount -> TotalActiveCapital 'AccountV1

deriving instance Show (TotalActiveCapital av)

instance IsAccountVersion av => Serialize (TotalActiveCapital av) where
    put TotalActiveCapitalV0 = return ()
    put (TotalActiveCapitalV1 amt) = put amt
    get = case accountVersion @av of
        SAccountV0 -> return TotalActiveCapitalV0
        SAccountV1 -> TotalActiveCapitalV1 <$> get

-- |Add an amount to a 'TotalActiveCapital'.
addActiveCapital :: Amount -> TotalActiveCapital av -> TotalActiveCapital av
addActiveCapital _ TotalActiveCapitalV0 = TotalActiveCapitalV0
addActiveCapital amt0 (TotalActiveCapitalV1 amt1) = TotalActiveCapitalV1 $ amt0 + amt1

-- |Subtract a given 'Amount' from a 'TotalActiveCapital'.
subtractActiveCapital :: Amount -> TotalActiveCapital av -> TotalActiveCapital av
subtractActiveCapital _ TotalActiveCapitalV0 = TotalActiveCapitalV0
subtractActiveCapital amt0 (TotalActiveCapitalV1 amt1) = TotalActiveCapitalV1 $ amt1 - amt0

tacAmount :: Lens' (TotalActiveCapital 'AccountV1) Amount
tacAmount f (TotalActiveCapitalV1 amt) = TotalActiveCapitalV1 <$> f amt

data PersistentActiveBakers (av :: AccountVersion) = PersistentActiveBakers {
    _activeBakers :: !(BakerIdTrieMap av),
    _aggregationKeys :: !(Trie.TrieN (BufferedBlobbed BlobRef) BakerAggregationVerifyKey ()),
    _passiveDelegators :: !(PersistentActiveDelegators av),
    _totalActiveCapital :: !(TotalActiveCapital av)
} deriving (Show)

makeLenses ''PersistentActiveBakers

totalActiveCapitalV1 :: Lens' (PersistentActiveBakers 'AccountV1) Amount
totalActiveCapitalV1 = totalActiveCapital . tac
    where
        tac :: Lens' (TotalActiveCapital 'AccountV1) Amount
        tac f (TotalActiveCapitalV1 v) = TotalActiveCapitalV1 <$> f v

-- |A helper function that adds a delegator to a 'PersistentActiveDelegators'.
-- It is assumed that the delegator is not already in the delegators.
addDelegatorHelper :: (MonadBlobStore m) => DelegatorId -> Amount -> PersistentActiveDelegators 'AccountV1 -> m (PersistentActiveDelegators 'AccountV1)
addDelegatorHelper did amt (PersistentActiveDelegatorsV1 dset tot) = do
    newDset <- Trie.insert did () dset
    return $ PersistentActiveDelegatorsV1 newDset (tot + amt)

-- |Add a delegator to the persistent active bakers at a particular target.
-- It is assumed that the delegator is not already delegated to this target.
-- If the target is not valid (i.e. it is a baker, but not in the active bakers) then the result
-- is @Left bid@, where @bid@ is the id of the target baker.  Otherwise, the return value is
-- @Right pab'@ where @pab'@ is the updated 'PersistentActiveBakers'.
--
-- IMPORTANT: This does not update the total active capital!
addDelegator ::
    (MonadBlobStore m) =>
    DelegationTarget ->
    DelegatorId ->
    Amount ->
    PersistentActiveBakers 'AccountV1 ->
    m (Either BakerId (PersistentActiveBakers 'AccountV1))
addDelegator DelegatePassive did amt pab =
    Right <$> passiveDelegators (addDelegatorHelper did amt) pab
addDelegator (DelegateToBaker bid) did amt pab =
    Trie.lookup bid (pab ^. activeBakers) >>= \case
        Nothing -> return $ Left bid
        Just pad -> do
            pad' <- addDelegatorHelper did amt pad
            newActiveBakers <- Trie.insert bid pad' (pab ^. activeBakers)
            return $ Right $ pab & activeBakers .~ newActiveBakers

-- |A helper function that removes a delegator from a 'PersistentActiveDelegators'.
-- It is assumed that the delegator is in the delegators with the specified amount.
removeDelegatorHelper :: (MonadBlobStore m) => DelegatorId -> Amount -> PersistentActiveDelegators 'AccountV1 -> m (PersistentActiveDelegators 'AccountV1)
removeDelegatorHelper did amt (PersistentActiveDelegatorsV1 dset tot) = do
    newDset <- Trie.delete did dset
    return $ PersistentActiveDelegatorsV1 newDset (tot - amt)

-- |Remove a delegator from 'PersistentActiveBakers'. It is assumed that the delegator
-- belongs to the pool, and the 'Amount' correctly represents the delegator's contribution.
--
-- IMPORTANT: This does not update the total active capital!
removeDelegator ::
    (MonadBlobStore m) =>
    DelegationTarget ->
    DelegatorId ->
    Amount ->
    PersistentActiveBakers 'AccountV1 ->
    m (PersistentActiveBakers 'AccountV1)
removeDelegator DelegatePassive did amt pab = passiveDelegators (removeDelegatorHelper did amt) pab
removeDelegator (DelegateToBaker bid) did amt pab = do
    let rdh Nothing = return ((), Trie.NoChange)
        rdh (Just pad) = do
            pad' <- removeDelegatorHelper did amt pad
            return ((), Trie.Insert pad')
    newActiveBakers <- snd <$> Trie.adjust rdh bid (pab ^. activeBakers)
    return $ pab & activeBakers .~ newActiveBakers

-- |Transfer all delegators from a baker to passive delegation in the 'PersistentActiveBakers'. This does
-- not affect the total stake, and does not remove the baker itself. This returns the list of
-- affected delegators.  (This will have no effect if the baker is not actually a baker, although
-- this function should not be used in that case.)
transferDelegatorsToPassive ::
    (MonadBlobStore m) =>
    BakerId ->
    PersistentActiveBakers 'AccountV1 ->
    m ([DelegatorId], PersistentActiveBakers 'AccountV1)
transferDelegatorsToPassive bid pab = do
    (transferred, newAB) <- Trie.adjust extract bid (pab ^. activeBakers)
    transList <- Trie.keysAsc (adDelegators transferred)
    let oldLPD = pab ^. passiveDelegators . to adDelegators
    newLPD <- foldM (\a d -> Trie.insert d () a) oldLPD transList
    let pab' =
            pab & activeBakers .~ newAB
                & passiveDelegators
                    %~ ( \(PersistentActiveDelegatorsV1 _ tot) ->
                            PersistentActiveDelegatorsV1 newLPD (tot + adDelegatorTotalCapital transferred)
                       )
    return (transList, pab')
  where
    extract Nothing = return (emptyPersistentActiveDelegators, Trie.NoChange)
    extract (Just t) = return (t, Trie.Insert emptyPersistentActiveDelegators)

instance
        (IsAccountVersion av, MonadBlobStore m) =>
        BlobStorable m (PersistentActiveBakers av) where
    storeUpdate oldPAB@PersistentActiveBakers{..} = do
        (pActiveBakers, newActiveBakers) <- storeUpdate _activeBakers
        (pAggregationKeys, newAggregationKeys) <- storeUpdate _aggregationKeys
        (ppassiveDelegators, newpassiveDelegators) <- storeUpdate _passiveDelegators
        let pPAB = pActiveBakers >> pAggregationKeys >> ppassiveDelegators >> put _totalActiveCapital
        let newPAB = oldPAB{
          _activeBakers = newActiveBakers,
          _aggregationKeys = newAggregationKeys,
          _passiveDelegators = newpassiveDelegators
        }
        return (pPAB, newPAB)
    store pab = fst <$> storeUpdate pab
    load = do
        mActiveBakers <- load
        mAggregationKeys <- load
        mpassiveDelegators <- load
        _totalActiveCapital <- get
        return $ do
            _activeBakers <- mActiveBakers
            _aggregationKeys <- mAggregationKeys
            _passiveDelegators <- mpassiveDelegators
            return PersistentActiveBakers{..}

instance (IsAccountVersion av, Applicative m) => Cacheable m (PersistentActiveBakers av)

makePersistentActiveBakers ::
    forall av m.
    (IsAccountVersion av, MonadBlobStore m) =>
    Basic.ActiveBakers ->
    m (PersistentActiveBakers av)
makePersistentActiveBakers ab = do
    let addActiveBaker acc (bid, dels) = case accountVersion @av of
            SAccountV0 ->
                Trie.insert bid PersistentActiveDelegatorsV0 acc
            SAccountV1 -> do
                pDels <- Trie.fromList $ (,()) <$> Set.toList (Basic._apDelegators dels)
                Trie.insert bid (PersistentActiveDelegatorsV1 pDels (Basic._apDelegatorTotalCapital dels)) acc
    _activeBakers <- foldlM addActiveBaker Trie.empty (Map.toList (Basic._activeBakers ab))
    _aggregationKeys <- Trie.fromList $ (,()) <$> Set.toList (Basic._aggregationKeys ab)
    _passiveDelegators <- case accountVersion @av of
        SAccountV0 ->
            return PersistentActiveDelegatorsV0
        SAccountV1 ->
            PersistentActiveDelegatorsV1
                <$> Trie.fromList ((,()) <$> Set.toList (Basic._apDelegators passive))
                <*> pure (Basic._apDelegatorTotalCapital passive)
          where
            passive = Basic._passiveDelegators ab
    let _totalActiveCapital = case accountVersion @av of
            SAccountV0 -> TotalActiveCapitalV0
            SAccountV1 -> TotalActiveCapitalV1 (Basic._totalActiveCapital ab)
    return PersistentActiveBakers{..}
