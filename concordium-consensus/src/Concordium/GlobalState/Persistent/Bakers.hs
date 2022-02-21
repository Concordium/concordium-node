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
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.Types
import Concordium.Utils.BinarySearch
import Concordium.Utils.Serialization

import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.Types.HashableTo
import Concordium.Utils.Serialization.Put

-- |A list of references to 'BakerInfo's, ordered by increasing 'BakerId'.
newtype BakerInfos = BakerInfos (Vec.Vector (BufferedRef BaseAccounts.BakerInfo)) deriving (Show)

instance (MonadBlobStore m) => BlobStorable m BakerInfos where
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

instance (MonadBlobStore m) => MHashableTo m H.Hash BakerInfos where
    getHashM (BakerInfos v) = do
      v' <- mapM loadBufferedRef v
      return $ H.hashLazy $ runPutLazy $ mapM_ put v'

instance (MonadBlobStore m) => Cacheable m BakerInfos where
    cache (BakerInfos v) = BakerInfos <$> mapM cache v

-- |A list of stakes for bakers.
newtype BakerStakes = BakerStakes (Vec.Vector Amount) deriving (Show)

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
data PersistentEpochBakers = PersistentEpochBakers {
    _bakerInfos :: !(HashedBufferedRef BakerInfos),
    _bakerStakes :: !(HashedBufferedRef BakerStakes),
    _bakerTotalStake :: !Amount
} deriving (Show)

makeLenses ''PersistentEpochBakers

epochBaker :: MonadBlobStore m => BakerId -> PersistentEpochBakers -> m (Maybe (BaseAccounts.BakerInfo, Amount))
epochBaker bid PersistentEpochBakers{..} = do
    (BakerInfos infoVec) <- refLoad _bakerInfos
    minfo <- binarySearchIM refLoad BaseAccounts._bakerIdentity infoVec bid
    forM minfo $ \(idx, binfo) -> do
        (BakerStakes stakeVec) <- refLoad _bakerStakes
        return (binfo, stakeVec Vec.! idx)

-- |Serialize 'PersistentEpochBakers'.
putEpochBakers :: (MonadBlobStore m, MonadPut m) => PersistentEpochBakers -> m ()
putEpochBakers peb = do
        BakerInfos bi <- refLoad (peb ^. bakerInfos)
        bInfos <- mapM (fmap (^. BaseAccounts.bakerInfo) . refLoad) bi
        BakerStakes bStakes <- refLoad (peb ^. bakerStakes)
        assert (Vec.length bInfos == Vec.length bStakes) $
            liftPut $ putLength (Vec.length bInfos)
        mapM_ sPut bInfos
        mapM_ (liftPut . put) bStakes

instance MonadBlobStore m => MHashableTo m H.Hash PersistentEpochBakers where
    getHashM PersistentEpochBakers{..} = do
      hbkrInfos <- getHashM _bakerInfos
      hbkrStakes <- getHashM _bakerStakes
      return $ H.hashOfHashes hbkrInfos hbkrStakes

instance MonadBlobStore m => BlobStorable m PersistentEpochBakers where
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

instance MonadBlobStore m => Cacheable m PersistentEpochBakers where
    cache peb = do
        cBkrInfos <- cache (_bakerInfos peb)
        cBkrStakes <- cache (_bakerStakes peb)
        return peb {_bakerInfos = cBkrInfos, _bakerStakes = cBkrStakes}

-- |Derive a 'FullBakers' from a 'PersistentEpochBakers'.
epochToFullBakers :: MonadBlobStore m => PersistentEpochBakers -> m FullBakers
epochToFullBakers PersistentEpochBakers{..} = do
    BakerInfos infoRefs <- refLoad _bakerInfos
    infos <- mapM (fmap (^. BaseAccounts.bakerInfo) . refLoad) infoRefs
    BakerStakes stakes <- refLoad _bakerStakes
    return FullBakers{
            fullBakerInfos = Vec.zipWith mkFullBakerInfo infos stakes,
            bakerTotalStake = _bakerTotalStake
        }
    where
        mkFullBakerInfo info stake = FullBakerInfo info stake

-- |Derive a 'PersistentEpochBakers' from a 'Basic.EpochBakers'.
makePersistentEpochBakers :: MonadBlobStore m => Basic.EpochBakers -> m PersistentEpochBakers
makePersistentEpochBakers ebs = do
    _bakerInfos <- refMake . BakerInfos =<< mapM refMake (Basic._bakerInfos ebs)
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

emptyPersistentAccountDelegators :: forall av. IsAccountVersion av => PersistentActiveDelegators av
emptyPersistentAccountDelegators =
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
    _lPoolDelegators :: !(PersistentActiveDelegators av),
    _totalActiveCapital :: !(TotalActiveCapital av)
} deriving (Show)

makeLenses ''PersistentActiveBakers

totalActiveCapitalV1 :: Lens' (PersistentActiveBakers 'AccountV1) Amount
totalActiveCapitalV1 = totalActiveCapital . tac
    where
        tac :: Lens' (TotalActiveCapital 'AccountV1) Amount
        tac f (TotalActiveCapitalV1 v) = TotalActiveCapitalV1 <$> f v

instance
        (IsAccountVersion av, MonadBlobStore m) =>
        BlobStorable m (PersistentActiveBakers av) where
    storeUpdate oldPAB@PersistentActiveBakers{..} = do
        (pActiveBakers, newActiveBakers) <- storeUpdate _activeBakers
        (pAggregationKeys, newAggregationKeys) <- storeUpdate _aggregationKeys
        (pLPoolDelegators, newLPoolDelegators) <- storeUpdate _lPoolDelegators
        let pPAB = pActiveBakers >> pAggregationKeys >> pLPoolDelegators >> put _totalActiveCapital
        let newPAB = oldPAB{
          _activeBakers = newActiveBakers,
          _aggregationKeys = newAggregationKeys,
          _lPoolDelegators = newLPoolDelegators
        }
        return (pPAB, newPAB)
    store pab = fst <$> storeUpdate pab
    load = do
        mActiveBakers <- load
        mAggregationKeys <- load
        mLPoolDelegators <- load
        _totalActiveCapital <- get
        return $ do
            _activeBakers <- mActiveBakers
            _aggregationKeys <- mAggregationKeys
            _lPoolDelegators <- mLPoolDelegators
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
    _lPoolDelegators <- case accountVersion @av of
        SAccountV0 ->
            return PersistentActiveDelegatorsV0
        SAccountV1 ->
            PersistentActiveDelegatorsV1
                <$> Trie.fromList ((,()) <$> Set.toList (Basic._apDelegators lpool))
                <*> pure (Basic._apDelegatorTotalCapital lpool)
          where
            lpool = Basic._lPoolDelegators ab
    let _totalActiveCapital = case accountVersion @av of
            SAccountV0 -> TotalActiveCapitalV0
            SAccountV1 -> TotalActiveCapitalV1 (Basic._totalActiveCapital ab)
    return PersistentActiveBakers{..}
