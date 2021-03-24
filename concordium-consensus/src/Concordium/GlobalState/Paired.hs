{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |This module pairs together two global state implementations
-- for testing purposes.
module Concordium.GlobalState.Paired where

import Lens.Micro.Platform
import Control.Exception
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.IO.Class
import Control.Monad
import Data.Coerce
import Data.Function
import qualified Data.Sequence as Seq
import qualified Data.List as List
import Data.Proxy

import qualified Concordium.Crypto.SHA256 as H
import Concordium.Types.HashableTo
import Concordium.Types

import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.Instance
import qualified Concordium.GlobalState.Classes as C
import Concordium.GlobalState.Block
import Concordium.GlobalState.BlockMonads
import Concordium.GlobalState.BlockState
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.TreeState as TS
import Concordium.GlobalState
import Concordium.Logger (MonadLogger(..))

-- |Monad for coercing reader and state types.
newtype ReviseRSM r s m a = ReviseRSM (m a)
    deriving (Functor, Applicative, Monad, MonadIO, MonadLogger)

instance (MonadReader r' m, Coercible r' r) =>
        MonadReader r (ReviseRSM r s m) where
    ask = ReviseRSM (fmap coerce (ask :: m r'))
    local f (ReviseRSM a) = ReviseRSM (local (coerce f) a)
    reader r = ReviseRSM (reader (coerce r))

instance (MonadState s' m, Coercible s' s) =>
        MonadState s (ReviseRSM r s m) where
    get = ReviseRSM (fmap coerce (get :: m s'))
    put = ReviseRSM . put . coerce
    state f = ReviseRSM (state (coerce f))

data PairGSContext lc rc = PairGSContext {
        _pairContextLeft :: !lc,
        _pairContextRight :: !rc
    }
makeLenses ''PairGSContext

data PairGState ls rs = PairGState {
        _pairStateLeft :: !ls,
        _pairStateRight :: !rs
    }
makeLenses ''PairGState

newtype FocusLeft a = FocusLeft { unFocusLeft :: a }
newtype FocusRight a = FocusRight { unFocusRight :: a }

instance C.HasGlobalStateContext (PairGSContext lc rc) a => C.HasGlobalStateContext lc (FocusLeft a) where
    globalStateContext = lens unFocusLeft (const FocusLeft) . C.globalStateContext . pairContextLeft

instance C.HasGlobalStateContext (PairGSContext lc rc) a => C.HasGlobalStateContext rc (FocusRight a) where
    globalStateContext = lens unFocusRight (const FocusRight) . C.globalStateContext . pairContextRight

type BSML pv lc r ls s m = BlockStateM pv lc (FocusLeft r) ls (FocusLeft s) (ReviseRSM (FocusLeft r) (FocusLeft s) m)
type BSMR pv rc r rs s m = BlockStateM pv rc (FocusRight r) rs (FocusRight s) (ReviseRSM (FocusRight r) (FocusRight s) m)

instance (C.HasGlobalStateContext (PairGSContext lc rc) r)
        => BlockStateTypes (BlockStateM pv (PairGSContext lc rc) r (PairGState lg rg) s m) where
    type BlockState (BlockStateM pv (PairGSContext lc rc) r (PairGState lg rg) s m)
            = (BlockState (BSML pv lc r lg s m),
                BlockState (BSMR pv rc r rg s m))
    type UpdatableBlockState (BlockStateM pv (PairGSContext lc rc) r (PairGState lg rg) s m)
            = (UpdatableBlockState (BSML pv lc r lg s m),
                UpdatableBlockState (BSMR pv rc r rg s m))
    type Account (BlockStateM pv (PairGSContext lc rc) r (PairGState lg rg) s m)
            = (Account (BSML pv lc r lg s m),
                Account (BSMR pv rc r rg s m))

instance C.HasGlobalState (PairGState ls rs) s => C.HasGlobalState ls (FocusLeft s) where
    globalState = lens unFocusLeft (const FocusLeft) . C.globalState . pairStateLeft

instance C.HasGlobalState (PairGState ls rs) s => C.HasGlobalState rs (FocusRight s) where
    globalState = lens unFocusRight (const FocusRight) . C.globalState . pairStateRight

-- * Block Metadata
newtype PairBlockMetadata l r = PairBlockMetadata (l, r)
instance (BlockMetadata l, BlockMetadata r) => BlockMetadata (PairBlockMetadata l r) where
    blockPointer (PairBlockMetadata (l, r)) = assert (blockPointer l == blockPointer r) $ blockPointer l
    blockBaker (PairBlockMetadata (l, r)) = assert (blockBaker l == blockBaker r) $ blockBaker l
    blockBakerKey (PairBlockMetadata (l, r)) = assert (blockBakerKey l == blockBakerKey r) $ blockBakerKey l
    blockProof (PairBlockMetadata (l, r)) = assert (blockProof l == blockProof r) $ blockProof l
    blockNonce (PairBlockMetadata (l, r)) = assert (blockNonce l == blockNonce r) $ blockNonce l
    blockFinalizationData (PairBlockMetadata (l, r)) = assert (blockFinalizationData l == blockFinalizationData r) $ blockFinalizationData l

-- * Block Data
newtype PairBlockData l r = PairBlockData (l, r)
    deriving (BlockMetadata) via (PairBlockMetadata l r)

type instance BlockFieldType (PairBlockData l r) = PairBlockMetadata (BlockFieldType l) (BlockFieldType r)

instance (BlockData l, BlockData r) => BlockData (PairBlockData l r) where
    blockSlot (PairBlockData (l, r)) = assert (blockSlot l == blockSlot r) $ blockSlot l
    blockSignature (PairBlockData (l, r)) = assert (blockSignature l == blockSignature r) $ blockSignature l
    blockFields (PairBlockData (l, r)) = case (blockFields l, blockFields r) of
        (Nothing, Nothing) -> Nothing
        (Just ml, Just mr) -> Just $ PairBlockMetadata (ml, mr)
        _ -> error "blockFields do not match"
    blockTransactions (PairBlockData (l, r)) = assert (blockTransactions l == blockTransactions r) $
        blockTransactions l
    blockTransactionOutcomesHash (PairBlockData (l, r)) = assert (blockTransactionOutcomesHash l == blockTransactionOutcomesHash r) $ blockTransactionOutcomesHash l
    blockStateHash (PairBlockData (l, r)) = assert (blockStateHash l == blockStateHash r) $ blockStateHash l
    verifyBlockSignature (PairBlockData (l, r)) = assert (vbsl == verifyBlockSignature r) vbsl
        where
            vbsl = verifyBlockSignature l

instance (EncodeBlock pv l) => EncodeBlock pv (PairBlockData l r) where
    putBlock spv (PairBlockData (l, _)) = putBlock spv l
    putVersionedBlock spv (PairBlockData (l, _)) = putVersionedBlock spv l

instance (HashableTo BlockHash l, HashableTo BlockHash r) => HashableTo BlockHash (PairBlockData l r) where
    getHash (PairBlockData (l, r)) = assert ((getHash l :: BlockHash) == getHash r) $ getHash l

instance (Show l, Show r) => Show (PairBlockData l r) where
    show (PairBlockData (l, r)) = "(" ++ show l ++ ", " ++ show r ++ ")"

instance (BlockPendingData l, BlockPendingData r) => BlockPendingData (PairBlockData l r) where
    blockReceiveTime (PairBlockData (l, r)) = assert (blockReceiveTime l == blockReceiveTime r) $ blockReceiveTime l

instance (Eq l, Eq r) => Eq (PairBlockData l r) where
    (PairBlockData (l1, r1)) == (PairBlockData (l2, r2)) = assert ((l1 == l2) == (r1 == r2)) $ l1 == l2

instance (Ord l, Ord r) => Ord (PairBlockData l r) where
    compare (PairBlockData (l1, _)) (PairBlockData (l2, _)) = compare l1 l2

instance (BlockPointerData l, BlockPointerData r) => BlockPointerData (PairBlockData l r) where
    bpHash (PairBlockData (l, r)) = assert (bpHash l == bpHash r) $ bpHash l
    bpLastFinalizedHash (PairBlockData (l, r)) = assert (bpLastFinalizedHash l == bpLastFinalizedHash r) $ bpLastFinalizedHash l
    bpHeight (PairBlockData (l, r)) = assert (bpHeight l == bpHeight r) $ bpHeight l
    bpReceiveTime (PairBlockData (l, r)) = assert (bpReceiveTime l == bpReceiveTime r) $ bpReceiveTime l
    bpArriveTime (PairBlockData (l, r)) = assert (bpArriveTime l == bpArriveTime r) $ bpArriveTime l
    bpTransactionCount (PairBlockData (l, r)) = assert (bpTransactionCount l == bpTransactionCount r) $ bpTransactionCount l
    bpTransactionsEnergyCost (PairBlockData (l, r)) = assert (bpTransactionsEnergyCost l == bpTransactionsEnergyCost r) $ bpTransactionsEnergyCost l
    bpTransactionsSize (PairBlockData (l, r)) = assert (bpTransactionsSize l == bpTransactionsSize r) $ bpTransactionsSize l

type GSML pv lc r ls s m = GlobalStateM pv NoLogContext lc (FocusLeft r) ls (FocusLeft s) (ReviseRSM (FocusLeft r) (FocusLeft s) m)
type GSMR pv rc r rs s m = GlobalStateM pv NoLogContext rc (FocusRight r) rs (FocusRight s) (ReviseRSM (FocusRight r) (FocusRight s) m)

instance (GlobalStateTypes (GSML pv lc r ls s m), GlobalStateTypes (GSMR pv rc r rs s m))
        => GlobalStateTypes (TreeStateBlockStateM pv (PairGState ls rs) (PairGSContext lc rc) r s m) where
    type BlockPointerType (TreeStateBlockStateM pv (PairGState ls rs) (PairGSContext lc rc) r s m) = PairBlockData (BlockPointerType (GSML pv lc r ls s m)) (BlockPointerType (GSMR pv rc r rs s m))

instance ATITypes (TreeStateBlockStateM pv (PairGState ls rs) (PairGSContext lc rc) r s m) where
  type ATIStorage (TreeStateBlockStateM pv (PairGState ls rs) (PairGSContext lc rc) r s m) = ()

{-# INLINE coerceBSML #-}
coerceBSML :: BSML pv lc r ls s m a -> BlockStateM pv (PairGSContext lc rc) r (PairGState ls rs) s m a
coerceBSML = coerce

{-# INLINE coerceBSMR #-}
coerceBSMR :: BSMR pv rc r rs s m a -> BlockStateM pv (PairGSContext lc rc) r (PairGState ls rs) s m a
coerceBSMR = coerce

instance (Monad m, C.HasGlobalStateContext (PairGSContext lc rc) r, BlockStateQuery (BSML pv lc r ls s m), BlockStateQuery (BSMR pv rc r rs s m), HashableTo H.Hash (Account (BSML pv lc r ls s m)), HashableTo H.Hash (Account (BSMR pv rc r rs s m)))
        => BlockStateQuery (BlockStateM pv (PairGSContext lc rc) r (PairGState ls rs) s m) where
    getModule (ls, rs) modRef = do
        m1 <- coerceBSML (getModule ls modRef)
        m2 <- coerceBSMR (getModule rs modRef)
        assert (m1 == m2) $ return m1
    getAccount (ls, rs) addr = do
        a1 <- coerceBSML (getAccount ls addr)
        a2 <- coerceBSMR (getAccount rs addr)
        case (a1, a2) of
          (Just a1', Just a2') ->
            assert ((getHash a1' :: H.Hash) == getHash a2') $
              return $ Just (a1', a2')
          (Nothing, Nothing) ->
            return Nothing
          (Nothing, _) -> error $ "Cannot get account with address " ++ show addr ++ " in left implementation"
          (_, Nothing) -> error $ "Cannot get account with address " ++ show addr ++ " in right implementation"
    getAccountByCredId (ls, rs) cid = do
        a1 <- coerceBSML (getAccountByCredId ls cid)
        a2 <- coerceBSMR (getAccountByCredId rs cid)
        case (a1, a2) of
          (Just a1', Just a2') ->
            assert ((getHash a1' :: H.Hash) == getHash a2') $
              return $ Just (a1', a2')
          (Nothing, Nothing) ->
            return Nothing
          (Nothing, _) -> error $ "Cannot get account with credid " ++ show cid ++ " in left implementation"
          (_, Nothing) -> error $ "Cannot get account with credid " ++ show cid ++ " in right implementation"
    getBakerAccount (ls, rs) bid = do
        a1 <- coerceBSML (getBakerAccount ls bid)
        a2 <- coerceBSMR (getBakerAccount rs bid)
        case (a1, a2) of
          (Just a1', Just a2') ->
            assert ((getHash a1' :: H.Hash) == getHash a2') $
              return $ Just (a1', a2')
          (Nothing, Nothing) ->
            return Nothing
          (Nothing, _) -> error $ "Cannot get account for baker " ++ show bid ++ " in left implementation"
          (_, Nothing) -> error $ "Cannot get account for baker " ++ show bid ++ " in right implementation"
    getContractInstance (ls, rs) caddr = do
        c1 <- coerceBSML (getContractInstance ls caddr)
        c2 <- coerceBSMR (getContractInstance rs caddr)
        assert (((==) `on` fmap instanceHash) c1 c2) $ return c1
    getModuleList (ls, rs) = do
        m1 <- coerceBSML (getModuleList ls)
        m2 <- coerceBSMR (getModuleList rs)
        assert (m1 == m2) $ return m1
    getAccountList (ls, rs) = do
        a1 <- coerceBSML (getAccountList ls)
        a2 <- coerceBSMR (getAccountList rs)
        assert (a1 == a2) $ return a1
    getContractInstanceList (ls, rs) = do
        a1 <- coerceBSML (getContractInstanceList ls)
        a2 <- coerceBSMR (getContractInstanceList rs)
        assert (((==) `on` fmap instanceHash) a1 a2) $ return a1
    getSeedState (ls, rs) = do
        ss1 <- coerceBSML (getSeedState ls)
        ss2 <- coerceBSMR (getSeedState rs)
        assert (ss1 == ss2) $ return ss1
    getCurrentEpochBakers (ls, rs) = do
        b1 <- coerceBSML (getCurrentEpochBakers ls)
        b2 <- coerceBSMR (getCurrentEpochBakers rs)
        assert (b1 == b2) $ return b1
    getSlotBakers (ls, rs) s = do
        b1 <- coerceBSML (getSlotBakers ls s)
        b2 <- coerceBSMR (getSlotBakers rs s)
        assert (b1 == b2) $ return b1
    getRewardStatus (ls, rs) = do
        a1 <- coerceBSML (getRewardStatus ls)
        a2 <- coerceBSMR (getRewardStatus rs)
        assert (a1 == a2) $ return a1
    getTransactionOutcome (ls, rs) th = do
        a1 <- coerceBSML (getTransactionOutcome ls th)
        a2 <- coerceBSMR (getTransactionOutcome rs th)
        assert (a1 == a2) $ return a1
    getTransactionOutcomesHash (ls, rs) = do
        a1 <- coerceBSML (getTransactionOutcomesHash ls)
        a2 <- coerceBSMR (getTransactionOutcomesHash rs)
        assert (a1 == a2) $ return a1
    getStateHash (ls, rs) = do
        a1 <- coerceBSML (getStateHash ls)
        a2 <- coerceBSMR (getStateHash rs)
        unless (a1 == a2) $ error $ "State hash mismatch:\n  " ++ show a1 ++ "\n  " ++ show a2
        return a1
    getOutcomes (ls, rs) = do
        a1 <- coerceBSML (getOutcomes ls)
        a2 <- coerceBSMR (getOutcomes rs)
        assert (a1 == a2) $ return a1
    getSpecialOutcomes (ls, rs) = do
        a1 <- coerceBSML (getSpecialOutcomes ls)
        a2 <- coerceBSMR (getSpecialOutcomes rs)
        assert (a1 == a2) $ return a1
    getAllIdentityProviders (bs1, bs2) = do
        r1 <- coerceBSML $ getAllIdentityProviders bs1
        r2 <- coerceBSMR $ getAllIdentityProviders bs2
        assert (r1 == r2) $ return r1
    getAllAnonymityRevokers (bs1, bs2) = do
        r1 <- coerceBSML $ getAllAnonymityRevokers bs1
        r2 <- coerceBSMR $ getAllAnonymityRevokers bs2
        assert (r1 == r2) $ return r1
    getElectionDifficulty (bps1, bps2) ts = do
        e1 <- coerceBSML (getElectionDifficulty bps1 ts)
        e2 <- coerceBSMR (getElectionDifficulty bps2 ts)
        assert (e1 == e2) $ return e1
    getNextUpdateSequenceNumber (bps1, bps2) uty = do
        sn1 <- coerceBSML (getNextUpdateSequenceNumber bps1 uty)
        sn2 <- coerceBSMR (getNextUpdateSequenceNumber bps2 uty)
        assert (sn1 == sn2) $ return sn1
    getCurrentElectionDifficulty (bps1, bps2) = do
        e1 <- coerceBSML (getCurrentElectionDifficulty bps1)
        e2 <- coerceBSMR (getCurrentElectionDifficulty bps2)
        assert (e1 == e2) $ return e1
    getUpdates (bps1, bps2) = do
        u1 <- coerceBSML (getUpdates bps1)
        u2 <- coerceBSMR (getUpdates bps2)
        assert (u1 == u2) $ return u1
    getProtocolUpdateStatus (bps1, bps2) = do
        us1 <- coerceBSML (getProtocolUpdateStatus bps1)
        us2 <- coerceBSMR (getProtocolUpdateStatus bps2)
        assert (us1 == us2) $ return us1

    getCryptographicParameters (bps1, bps2) = do
        u1 <- coerceBSML (getCryptographicParameters bps1)
        u2 <- coerceBSMR (getCryptographicParameters bps2)
        assert (u1 == u2) $ return u1

instance (Monad m, C.HasGlobalStateContext (PairGSContext lc rc) r, AccountOperations (BSML pv lc r ls s m), AccountOperations (BSMR pv rc r rs s m), HashableTo H.Hash (Account (BSML pv lc r ls s m)), HashableTo H.Hash (Account (BSMR pv rc r rs s m)))
  => AccountOperations (BlockStateM pv (PairGSContext lc rc) r (PairGState ls rs) s m) where

    getAccountAddress (acc1, acc2) = do
        addr1 <- coerceBSML (getAccountAddress acc1)
        addr2 <- coerceBSMR (getAccountAddress acc2)
        assert (addr1 == addr2) $ return addr1

    getAccountAmount (acc1, acc2) = do
        amnt1 <- coerceBSML (getAccountAmount acc1)
        amnt2 <- coerceBSMR (getAccountAmount acc2)
        assert (amnt1 == amnt2) $ return amnt1

    getAccountNonce (acc1, acc2) = do
        n1 <- coerceBSML (getAccountNonce acc1)
        n2 <- coerceBSMR (getAccountNonce acc2)
        assert (n1 == n2) $ return n1

    getAccountCredentials (acc1, acc2) = do
        cs1 <- coerceBSML (getAccountCredentials acc1)
        cs2 <- coerceBSMR (getAccountCredentials acc2)
        assert (cs1 == cs2) $ return cs1

    getAccountVerificationKeys (acc1, acc2) = do
        ks1 <- coerceBSML (getAccountVerificationKeys acc1)
        ks2 <- coerceBSMR (getAccountVerificationKeys acc2)
        assert (ks1 == ks2) $ return ks1

    getAccountEncryptedAmount (acc1, acc2) = do
        amnts1 <- coerceBSML (getAccountEncryptedAmount acc1)
        amnts2 <- coerceBSMR (getAccountEncryptedAmount acc2)
        assert (amnts1 == amnts2) $ return amnts1

    getAccountEncryptionKey (acc1, acc2) = do
        k1 <- coerceBSML (getAccountEncryptionKey acc1)
        k2 <- coerceBSMR (getAccountEncryptionKey acc2)
        assert (k1 == k2) $ return k1

    getAccountReleaseSchedule (acc1, acc2) = do
        ars1 <- coerceBSML (getAccountReleaseSchedule acc1)
        ars2 <- coerceBSMR (getAccountReleaseSchedule acc2)
        assert (ars1 == ars2) $ return ars1

    getAccountBaker (acc1, acc2) = do
        ab1 <- coerceBSML (getAccountBaker acc1)
        ab2 <- coerceBSMR (getAccountBaker acc2)
        assert (ab1 == ab2) $ return ab1

instance (MonadLogger m, C.HasGlobalStateContext (PairGSContext lc rc) r, BlockStateOperations (BSML pv lc r ls s m), BlockStateOperations (BSMR pv rc r rs s m), HashableTo H.Hash (Account (BSML pv lc r ls s m)), HashableTo H.Hash (Account (BSMR pv rc r rs s m)))
        => BlockStateOperations (BlockStateM pv (PairGSContext lc rc) r (PairGState ls rs) s m) where
    bsoGetModule (bs1, bs2) mref = do
        r1 <- coerceBSML $ bsoGetModule bs1 mref
        r2 <- coerceBSMR $ bsoGetModule bs2 mref
        assert (r1 == r2) $ return r1
    bsoGetAccount (bs1, bs2) aref = do
        r1 <- coerceBSML $ bsoGetAccount bs1 aref
        r2 <- coerceBSMR $ bsoGetAccount bs2 aref
        case (r1, r2) of
          (Just r1', Just r2') ->
            assert ((getHash r1' :: H.Hash) == getHash r2') $
              return $ Just (r1', r2')
          (Nothing, Nothing) ->
            return Nothing
          (Nothing, _) ->
            error $ "Cannot get account with address " ++ show aref ++ " in left implementation"
          (_, Nothing) ->
            error $ "Cannot get account with address " ++ show aref ++ " in right implementation"
    bsoGetAccountIndex (bs1, bs2) aref = do
        r1 <- coerceBSML $ bsoGetAccountIndex bs1 aref
        r2 <- coerceBSMR $ bsoGetAccountIndex bs2 aref
        assert (r1 == r2) $ return r1
    bsoGetInstance (bs1, bs2) iref = do
        r1 <- coerceBSML $ bsoGetInstance bs1 iref
        r2 <- coerceBSMR $ bsoGetInstance bs2 iref
        assert (((==) `on` fmap instanceHash) r1 r2) $ return r1
    bsoRegIdExists (bs1, bs2) regid = do
        r1 <- coerceBSML $ bsoRegIdExists bs1 regid
        r2 <- coerceBSMR $ bsoRegIdExists bs2 regid
        assert (r1 == r2) $ return r1
    bsoCreateAccount (bs1, bs2) gc addr cred = do
        (r1, bs1') <- coerceBSML $ bsoCreateAccount bs1 gc addr cred
        (r2, bs2') <- coerceBSMR $ bsoCreateAccount bs2 gc addr cred
        case (r1, r2) of
            (Just a1, Just a2) ->
                assert ((getHash a1 :: H.Hash) == getHash a2) $ return (Just (a1, a2), (bs1', bs2'))
            (Nothing, Nothing) -> return (Nothing, (bs1', bs2'))
            (Nothing, _) ->
                error "Account creation failed in left implementation but not right"
            (_, Nothing) ->
                error "Account creation failed in right implementation but not left"
    bsoPutNewInstance (bs1, bs2) f = do
        (r1, bs1') <- coerceBSML $ bsoPutNewInstance bs1 f
        (r2, bs2') <- coerceBSMR $ bsoPutNewInstance bs2 f
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoPutNewModule (bs1, bs2) iface = do
        (r1, bs1') <- coerceBSML $ bsoPutNewModule bs1 iface
        (r2, bs2') <- coerceBSMR $ bsoPutNewModule bs2 iface
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoModifyAccount (bs1, bs2) upd = do
        bs1' <- coerceBSML $ bsoModifyAccount bs1 upd
        bs2' <- coerceBSMR $ bsoModifyAccount bs2 upd
        return (bs1', bs2')
    bsoSetAccountCredentialKeys (bs1, bs2) aaddr credIx credKeys = do
        bs1' <- coerceBSML $ bsoSetAccountCredentialKeys bs1 aaddr credIx credKeys
        bs2' <- coerceBSMR $ bsoSetAccountCredentialKeys bs2 aaddr credIx credKeys
        return (bs1', bs2')
    bsoUpdateAccountCredentials (bs1, bs2) aaddr remove add thrsh = do
        bs1' <- coerceBSML $ bsoUpdateAccountCredentials bs1 aaddr remove add thrsh
        bs2' <- coerceBSMR $ bsoUpdateAccountCredentials bs2 aaddr remove add thrsh
        return (bs1', bs2')
    bsoModifyInstance (bs1, bs2) caddr delta model = do
        bs1' <- coerceBSML $ bsoModifyInstance bs1 caddr delta model
        bs2' <- coerceBSMR $ bsoModifyInstance bs2 caddr delta model
        return (bs1', bs2')
    bsoNotifyEncryptedBalanceChange (bs1, bs2) amt = do
        bs1' <- coerceBSML $ bsoNotifyEncryptedBalanceChange bs1 amt
        bs2' <- coerceBSMR $ bsoNotifyEncryptedBalanceChange bs2 amt
        return (bs1', bs2')
    bsoGetSeedState (bs1, bs2) = do
        r1 <- coerceBSML $ bsoGetSeedState bs1
        r2 <- coerceBSMR $ bsoGetSeedState bs2
        assert (r1 == r2) $ return r1
    bsoSetSeedState (bs1, bs2) ss = do
        bs1' <- coerceBSML $ bsoSetSeedState bs1 ss
        bs2' <- coerceBSMR $ bsoSetSeedState bs2 ss
        return (bs1', bs2')
    bsoTransitionEpochBakers (bs1, bs2) epoch = do
        bs1' <- coerceBSML $ bsoTransitionEpochBakers bs1 epoch
        bs2' <- coerceBSMR $ bsoTransitionEpochBakers bs2 epoch
        return (bs1', bs2')
    bsoAddBaker (bs1, bs2) addr bkrAdd = do
        (r1, bs1') <- coerceBSML $ bsoAddBaker bs1 addr bkrAdd
        (r2, bs2') <- coerceBSMR $ bsoAddBaker bs2 addr bkrAdd
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoUpdateBakerKeys (bs1, bs2) addr bkrKUpd = do
        (r1, bs1') <- coerceBSML $ bsoUpdateBakerKeys bs1 addr bkrKUpd
        (r2, bs2') <- coerceBSMR $ bsoUpdateBakerKeys bs2 addr bkrKUpd
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoUpdateBakerStake (bs1, bs2) addr bkrSUpd = do
        (r1, bs1') <- coerceBSML $ bsoUpdateBakerStake bs1 addr bkrSUpd
        (r2, bs2') <- coerceBSMR $ bsoUpdateBakerStake bs2 addr bkrSUpd
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoUpdateBakerRestakeEarnings (bs1, bs2) aaddr restake = do
        (r1, bs1') <- coerceBSML $ bsoUpdateBakerRestakeEarnings bs1 aaddr restake
        (r2, bs2') <- coerceBSMR $ bsoUpdateBakerRestakeEarnings bs2 aaddr restake
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoRemoveBaker (bs1, bs2) addr = do
        (r1, bs1') <- coerceBSML $ bsoRemoveBaker bs1 addr
        (r2, bs2') <- coerceBSMR $ bsoRemoveBaker bs2 addr
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoRewardBaker (bs1, bs2) bid reward = do
        (r1, bs1') <- coerceBSML $ bsoRewardBaker bs1 bid reward
        (r2, bs2') <- coerceBSMR $ bsoRewardBaker bs2 bid reward
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoRewardFoundationAccount (bs1, bs2) reward = do
        bs1' <- coerceBSML $ bsoRewardFoundationAccount bs1 reward
        bs2' <- coerceBSMR $ bsoRewardFoundationAccount bs2 reward
        return (bs1', bs2')
    bsoGetFoundationAccount (bs1, bs2) = do
        a1 <- coerceBSML $ bsoGetFoundationAccount bs1
        a2 <- coerceBSMR $ bsoGetFoundationAccount bs2
        return (a1, a2)
    bsoMint (bs1, bs2) amt = do
        bs1' <- coerceBSML $ bsoMint bs1 amt
        bs2' <- coerceBSMR $ bsoMint bs2 amt
        return (bs1', bs2')
    bsoGetIdentityProvider (bs1, bs2) ipid = do
        r1 <- coerceBSML $ bsoGetIdentityProvider bs1 ipid
        r2 <- coerceBSMR $ bsoGetIdentityProvider bs2 ipid
        assert (r1 == r2) $ return r1
    bsoGetAnonymityRevokers (bs1, bs2) arIds = do
        r1 <- coerceBSML $ bsoGetAnonymityRevokers bs1 arIds
        r2 <- coerceBSMR $ bsoGetAnonymityRevokers bs2 arIds
        assert (r1 == r2) $ return r1
    bsoGetCryptoParams (bs1, bs2) = do
        r1 <- coerceBSML $ bsoGetCryptoParams bs1
        r2 <- coerceBSMR $ bsoGetCryptoParams bs2
        assert (r1 == r2) $ return r1
    bsoSetTransactionOutcomes (bs1, bs2) tos = do
        bs1' <- coerceBSML $ bsoSetTransactionOutcomes bs1 tos
        bs2' <- coerceBSMR $ bsoSetTransactionOutcomes bs2 tos
        return (bs1', bs2')
    bsoAddSpecialTransactionOutcome (bs1, bs2) sto = do
        bs1' <- coerceBSML $ bsoAddSpecialTransactionOutcome bs1 sto
        bs2' <- coerceBSMR $ bsoAddSpecialTransactionOutcome bs2 sto
        return (bs1', bs2')
    bsoProcessUpdateQueues (bs1, bs2) ts = do
        (cs1, bs1') <- coerceBSML $ bsoProcessUpdateQueues bs1 ts
        (cs2, bs2') <- coerceBSMR $ bsoProcessUpdateQueues bs2 ts
        assert (cs1 == cs2) $ return (cs1, (bs1', bs2'))
    bsoProcessReleaseSchedule (bs1, bs2) ts = do
        bs1' <- coerceBSML $ bsoProcessReleaseSchedule bs1 ts
        bs2' <- coerceBSMR $ bsoProcessReleaseSchedule bs2 ts
        return (bs1', bs2')
    bsoGetUpdateKeyCollection (bs1, bs2) = do
        a1 <- coerceBSML $ bsoGetUpdateKeyCollection bs1
        a2 <- coerceBSMR $ bsoGetUpdateKeyCollection bs2
        assert (a1 == a2) $ return a1
    bsoGetNextUpdateSequenceNumber (bs1, bs2) uty = do
        a1 <- coerceBSML $ bsoGetNextUpdateSequenceNumber bs1 uty
        a2 <- coerceBSMR $ bsoGetNextUpdateSequenceNumber bs2 uty
        assert (a1 == a2) $ return a1
    bsoEnqueueUpdate (bs1, bs2) tt p = do
        bs1' <- coerceBSML $ bsoEnqueueUpdate bs1 tt p
        bs2' <- coerceBSMR $ bsoEnqueueUpdate bs2 tt p
        return (bs1', bs2')
    bsoAddReleaseSchedule (bs1, bs2) tt = do
        bs1' <- coerceBSML $ bsoAddReleaseSchedule bs1 tt
        bs2' <- coerceBSMR $ bsoAddReleaseSchedule bs2 tt
        return (bs1', bs2')
    bsoGetEnergyRate (bs1, bs2) = do
        r1 <- coerceBSML $ bsoGetEnergyRate bs1
        r2 <- coerceBSMR $ bsoGetEnergyRate bs2
        assert (r1 == r2) $ return r1
    bsoGetChainParameters (bs1, bs2) = do
        r1 <- coerceBSML $ bsoGetChainParameters bs1
        r2 <- coerceBSMR $ bsoGetChainParameters bs2
        assert (r1 == r2) $ return r1
    bsoGetEpochBlocksBaked (bs1, bs2) = do
        r1 <- coerceBSML $ bsoGetEpochBlocksBaked bs1
        r2 <- coerceBSMR $ bsoGetEpochBlocksBaked bs2
        assert (r1 == r2) $ return r1
    bsoNotifyBlockBaked (bs1, bs2) bid = do
        bs1' <- coerceBSML $ bsoNotifyBlockBaked bs1 bid
        bs2' <- coerceBSMR $ bsoNotifyBlockBaked bs2 bid
        return (bs1', bs2')
    bsoClearEpochBlocksBaked (bs1, bs2) = do
        bs1' <- coerceBSML $ bsoClearEpochBlocksBaked bs1
        bs2' <- coerceBSMR $ bsoClearEpochBlocksBaked bs2
        return (bs1', bs2')
    bsoGetBankStatus (bs1, bs2) = do
        r1 <- coerceBSML $ bsoGetBankStatus bs1
        r2 <- coerceBSMR $ bsoGetBankStatus bs2
        assert (r1 == r2) $ return r1
    bsoSetRewardAccounts (bs1, bs2) rew = do
        bs1' <- coerceBSML $ bsoSetRewardAccounts bs1 rew
        bs2' <- coerceBSMR $ bsoSetRewardAccounts bs2 rew
        return (bs1', bs2')


type instance BlockStatePointer (a, b) = (BlockStatePointer a, BlockStatePointer b)

instance (MonadLogger m,
    C.HasGlobalStateContext (PairGSContext lc rc) r,
    BlockStateStorage (BSML pv lc r ls s m),
    BlockStateStorage (BSMR pv rc r rs s m),
    HashableTo H.Hash (Account (BSML pv lc r ls s m)),
    HashableTo H.Hash (Account (BSMR pv rc r rs s m)))
        => BlockStateStorage (BlockStateM pv (PairGSContext lc rc) r (PairGState ls rs) s m) where
    thawBlockState (bs1, bs2) = do
        ubs1 <- coerceBSML $ thawBlockState bs1
        ubs2 <- coerceBSMR $ thawBlockState bs2
        return (ubs1, ubs2)
    freezeBlockState (ubs1, ubs2) = do
        bs1 <- coerceBSML $ freezeBlockState ubs1
        bs2 <- coerceBSMR $ freezeBlockState ubs2
        return (bs1, bs2)
    dropUpdatableBlockState (ubs1, ubs2) = do
        coerceBSML $ dropUpdatableBlockState ubs1
        coerceBSMR $ dropUpdatableBlockState ubs2
    purgeBlockState (bs1, bs2) = do
        coerceBSML $ purgeBlockState bs1
        coerceBSMR $ purgeBlockState bs2
    archiveBlockState (bs1, bs2) = do
        coerceBSML $ archiveBlockState bs1
        coerceBSMR $ archiveBlockState bs2
    saveBlockState (bs1, bs2) = do
        p1 <- coerceBSML $ saveBlockState bs1
        p2 <- coerceBSMR $ saveBlockState bs2
        return (p1, p2)
    loadBlockState h (p1, p2) = do
        bs1 <- coerceBSML $ loadBlockState h p1
        bs2 <- coerceBSMR $ loadBlockState h p2
        return (bs1, bs2)
    cacheBlockState (bs1, bs2) = do
        bs1' <- coerceBSML $ cacheBlockState bs1
        bs2' <- coerceBSMR $ cacheBlockState bs2
        return (bs1', bs2')
    serializeBlockState (bps1, bps2) = do
        s1 <- coerceBSML (serializeBlockState bps1)
        s2 <- coerceBSMR (serializeBlockState bps2)
        -- While we check for equality, in future it could be
        -- acceptable for implementations to give different results.
        assert (s1 == s2) $ return s2
    -- We only write out 
    writeBlockState h (bps1, _) = coerceBSML (writeBlockState h bps1)

{-# INLINE coerceGSML #-}
coerceGSML :: GSML pv lc r ls s m a -> TreeStateBlockStateM pv (PairGState ls rs) (PairGSContext lc rc) r s m a
coerceGSML = coerce

{-# INLINE coerceGSMR #-}
coerceGSMR :: GSMR pv rc r rs s m a -> TreeStateBlockStateM pv (PairGState ls rs) (PairGSContext lc rc) r s m a
coerceGSMR = coerce

-- default instance since ATIStorage = ()
instance Monad m => PerAccountDBOperations (TreeStateBlockStateM pv (PairGState ls rs) (PairGSContext lc rc) r s m)

instance (C.HasGlobalStateContext (PairGSContext lc rc) r,
          MonadReader r m,
          C.HasGlobalState (PairGState ls rs) s,
          MonadState s m,
          MonadIO m,
          BlockPointerMonad (GSML pv lc r ls s m),
          BlockPointerMonad (GSMR pv rc r rs s m))
          => BlockPointerMonad (TreeStateBlockStateM pv (PairGState ls rs) (PairGSContext lc rc) r s m) where
    blockState (PairBlockData (bp1, bp2)) = do
        bs1 <- coerceGSML $ blockState bp1
        bs2 <- coerceGSMR $ blockState bp2
        return (bs1, bs2)
    bpParent (PairBlockData (bp1, bp2)) = do
        bs1 <- coerceGSML $ bpParent bp1
        bs2 <- coerceGSMR $ bpParent bp2
        return $ PairBlockData (bs1, bs2)
    bpLastFinalized (PairBlockData (bp1, bp2)) = do
        bs1 <- coerceGSML $ bpLastFinalized bp1
        bs2 <- coerceGSMR $ bpLastFinalized bp2
        return $ PairBlockData (bs1, bs2)

instance (C.HasGlobalStateContext (PairGSContext lc rc) r,
        MonadReader r m,
        C.HasGlobalState (PairGState ls rs) s,
        MonadState s m,
        MonadIO m,
        IsProtocolVersion pv,
        BlockStateStorage (TreeStateBlockStateM pv (PairGState ls rs) (PairGSContext lc rc) r s m),
        TreeStateMonad pv (GSML pv lc r ls s m),
        ATIStorage (GSML pv lc r ls s m) ~ (),
        TreeStateMonad pv (GSMR pv rc r rs s m),
        ATIStorage (GSMR pv rc r rs s m) ~ ())
        => TreeStateMonad pv (TreeStateBlockStateM pv (PairGState ls rs) (PairGSContext lc rc) r s m) where
    makePendingBlock sk sl parent bid bp bn lf trs sthash trouthash brtime = do
      pb1 <- coerceGSML $ TS.makePendingBlock sk sl parent bid bp bn lf trs sthash trouthash brtime
      pb2 <- coerceGSMR $ TS.makePendingBlock sk sl parent bid bp bn lf trs sthash trouthash brtime
      assert (pb1 == pb2) $ return pb1

    getFinalizedAtHeight bHeight = do
      b1 <- coerceGSML $ getFinalizedAtHeight bHeight
      b2 <- coerceGSMR $ getFinalizedAtHeight bHeight
      case (b1, b2) of
        (Nothing, Nothing) -> return Nothing
        (Just bp1, Just bp2) -> assert (bpHash bp1 == bpHash bp2) $ return (Just (PairBlockData (bp1, bp2)))
        (Nothing, Just bp) -> error $ "Cannot get finalized block at height in left implementation: " ++ show (bpHash bp)
        (Just bp, Nothing) -> error $ "Cannot get finalized block at height in right implementation: " ++ show (bpHash bp)

    getNextAccountNonce addr = do
      b1 <- coerceGSML $ getNextAccountNonce addr
      b2 <- coerceGSMR $ getNextAccountNonce addr
      assert (b1 == b2) $ return b1

    getCredential key = do
      b1 <- coerceGSML $ getCredential key
      b2 <- coerceGSMR $ getCredential key
      assert (b1 == b2) $ return b1

    getBlockStatus bh = do
        bs1 <- coerceGSML $ getBlockStatus bh
        bs2 <- coerceGSMR $ getBlockStatus bh
        case (bs1, bs2) of
            (Nothing, Nothing) -> return Nothing
            (Just (BlockAlive bp1), Just (BlockAlive bp2)) -> return $ Just (BlockAlive (PairBlockData (bp1, bp2)))
            (Just BlockDead, Just BlockDead) -> return $ Just BlockDead
            (Just (BlockFinalized bp1 fr1), Just (BlockFinalized bp2 fr2)) ->
                assert (fr1 == fr2) $ return $ Just $ BlockFinalized (PairBlockData (bp1, bp2)) fr1
            (Just (BlockPending pb1), Just (BlockPending pb2)) -> assert (pb1 == pb2) $ return $ Just (BlockPending pb1)
            _ -> error $ "getBlockStatus (Paired): block statuses do not match: " ++ show bs1 ++ ", " ++ show bs2
    makeLiveBlock pb (PairBlockData (parent1, parent2)) (PairBlockData (lf1, lf2)) (bs1, bs2) () t e = do
        r1 <- coerceGSML $ makeLiveBlock pb parent1 lf1 bs1 () t e
        r2 <- coerceGSMR $ makeLiveBlock pb parent2 lf2 bs2 () t e
        return (PairBlockData (r1, r2))
    markDead bh = do
        coerceGSML $ markDead bh
        coerceGSMR $ markDead bh
    markFinalized bh fr = do
        coerceGSML $ markFinalized bh fr
        coerceGSMR $ markFinalized bh fr
    markPending pb = do
        coerceGSML $ markPending pb
        coerceGSMR $ markPending pb
    getGenesisBlockPointer = do
        gen1 <- coerceGSML getGenesisBlockPointer
        gen2 <- coerceGSMR getGenesisBlockPointer
        return (PairBlockData (gen1, gen2))
    getGenesisData = do
        gd1 <- coerceGSML getGenesisData
        gd2 <- coerceGSMR getGenesisData
        assert (gd1 == gd2) $ return gd1
    getLastFinalized = do
        (bp1, fr1) <- coerceGSML getLastFinalized
        (bp2, fr2) <- coerceGSMR getLastFinalized
        assert (fr1 == fr2) $ return (PairBlockData (bp1, bp2), fr1)
    getLastFinalizedSlot = do
        r1 <- coerceGSML getLastFinalizedSlot
        r2 <- coerceGSMR getLastFinalizedSlot
        assert (r1 == r2) $ return r1
    getLastFinalizedHeight = do
        r1 <- coerceGSML getLastFinalizedHeight
        r2 <- coerceGSMR getLastFinalizedHeight
        assert (r1 == r2) $ return r1
    getNextFinalizationIndex = do
        r1 <- coerceGSML getNextFinalizationIndex
        r2 <- coerceGSMR getNextFinalizationIndex
        assert (r1 == r2) $ return r1
    addFinalization (PairBlockData (bp1, bp2)) fr = do
        coerceGSML $ addFinalization bp1 fr
        coerceGSMR $ addFinalization bp2 fr
    getFinalizedAtIndex fi = do
        r1 <- coerceGSML $ getFinalizedAtIndex fi
        r2 <- coerceGSMR $ getFinalizedAtIndex fi
        case (r1, r2) of
            (Nothing, Nothing) -> return Nothing
            (Just bp1, Just bp2) ->
              assert (bpHash bp1 == bpHash bp2) $
                return $ Just (PairBlockData (bp1, bp2))
            _ -> error $ "getFinalizationAtindex (Paired): no match " ++ show r1 ++ ", " ++ show r2
    getRecordAtIndex fi = do
        r1 <- coerceGSML $ getRecordAtIndex fi
        r2 <- coerceGSMR $ getRecordAtIndex fi
        case (r1, r2) of
            (Nothing, Nothing) -> return Nothing
            (Just rec1, Just rec2) ->
              assert (rec1 == rec2) $ -- TODO: Perhaps they don't have to be the same
                return $ Just rec1
            _ -> error $ "getRecordAtindex (Paired): no match " ++ show r1 ++ ", " ++ show r2
    getBranches = do
        r1 <- coerceGSML getBranches
        r2 <- coerceGSMR getBranches
        return $ Seq.zipWith (zipWith (curry PairBlockData)) r1 r2
    putBranches brs = do
        let (br1, br2) = Seq.unzipWith unzip (coerce brs)
        coerceGSML $ putBranches br1
        coerceGSMR $ putBranches br2
    takePendingChildren bh = do
        pc1 <- coerceGSML $ takePendingChildren bh
        pc2 <- coerceGSMR $ takePendingChildren bh
        let
            checkedZip [] [] = []
            checkedZip (c1 : cs1) (c2 : cs2) = assert ((getHash c1 :: BlockHash) == getHash c2) $ c1 : checkedZip cs1 cs2
            checkedZip _ _ = error "takePendingChildren: lists have different lengths"
            sortPBs :: (HashableTo BlockHash z) => [z] -> [z]
            sortPBs = List.sortBy ((compare :: BlockHash -> BlockHash -> Ordering) `on` getHash)
        return $ checkedZip (sortPBs pc1) (sortPBs pc2)
    addPendingBlock pb = do
        coerceGSML $ addPendingBlock pb
        coerceGSMR $ addPendingBlock pb
    takeNextPendingUntil sl = do
        r1 <- coerceGSML $ takeNextPendingUntil sl
        r2 <- coerceGSMR $ takeNextPendingUntil sl
        case (r1, r2) of
            (Nothing, Nothing) -> return Nothing
            (Just pb1, Just pb2) -> assert (pb1 == pb2) $ return $ Just pb1
            _ -> error "takeNextPendingUntil (Paired): implementations returned different results"
    getFocusBlock = do
        fb1 <- coerceGSML $ getFocusBlock
        fb2 <- coerceGSMR $ getFocusBlock
        return $ PairBlockData (fb1, fb2)
    putFocusBlock (PairBlockData (fb1, fb2)) = do
        coerceGSML $ putFocusBlock fb1
        coerceGSMR $ putFocusBlock fb2
    getPendingTransactions = do
        r1 <- coerceGSML getPendingTransactions
        r2 <- coerceGSMR getPendingTransactions
        assert (r1 == r2) $ return r1
    putPendingTransactions pts = do
        coerceGSML $ putPendingTransactions pts
        coerceGSMR $ putPendingTransactions pts
    getAccountNonFinalized acct nonce = do
        r1 <- coerceGSML $ getAccountNonFinalized acct nonce
        r2 <- coerceGSMR $ getAccountNonFinalized acct nonce
        assert (r1 == r2) $ return r1
    getNonFinalizedChainUpdates uty sn = do
        r1 <- coerceGSML $ getNonFinalizedChainUpdates uty sn
        r2 <- coerceGSMR $ getNonFinalizedChainUpdates uty sn
        assert (r1 == r2) $ return r1
    addTransaction tr = do
        r1 <- coerceGSML $ addTransaction tr
        r2 <- coerceGSMR $ addTransaction tr
        assert (r1 == r2) $ return r1
    finalizeTransactions bh slot trs = do
        coerceGSML $ finalizeTransactions bh slot trs
        coerceGSMR $ finalizeTransactions bh slot trs
    commitTransaction slot bh transaction res = do
        coerceGSML $ commitTransaction slot bh transaction res
        coerceGSMR $ commitTransaction slot bh transaction res
    addCommitTransaction tr sl = do
        r1 <- coerceGSML $ addCommitTransaction tr sl
        r2 <- coerceGSMR $ addCommitTransaction tr sl
        assert (r1 == r2) $ return r1
    purgeTransaction tr = do
        r1 <- coerceGSML $ purgeTransaction tr
        r2 <- coerceGSMR $ purgeTransaction tr
        assert (r1 == r2) $ return r1
    lookupTransaction h = do
        r1 <- coerceGSML $ lookupTransaction h
        r2 <- coerceGSMR $ lookupTransaction h
        assert (r1 == r2) $ return r1
    markDeadTransaction bh tr = do
      coerceGSML $ markDeadTransaction bh tr
      coerceGSMR $ markDeadTransaction bh tr
    -- For getting statistics, we will only use one side
    getConsensusStatistics = coerceGSML $ getConsensusStatistics
    putConsensusStatistics stats = do
        coerceGSML $ putConsensusStatistics stats
        coerceGSMR $ putConsensusStatistics stats
    -- For runtime parameters, we will only use one side
    getRuntimeParameters = coerceGSML getRuntimeParameters

    purgeTransactionTable t i = do
      coerceGSML (purgeTransactionTable t i)
      coerceGSMR (purgeTransactionTable t i)

newtype PairGSConfig c1 c2 = PairGSConfig (c1, c2)

instance (GlobalStateConfig c1, GlobalStateConfig c2) => GlobalStateConfig (PairGSConfig c1 c2) where
    type GSContext (PairGSConfig c1 c2) = PairGSContext (GSContext c1) (GSContext c2)
    type GSState (PairGSConfig c1 c2) = PairGState (GSState c1) (GSState c2)
    -- FIXME: The below could also be improved to add pairs.
    type GSLogContext (PairGSConfig c1 c2) = (GSLogContext c1, GSLogContext c2)
    initialiseGlobalState (PairGSConfig (conf1, conf2)) = do
            (ctx1, s1, c1) <- initialiseGlobalState conf1
            (ctx2, s2, c2) <- initialiseGlobalState conf2
            return (PairGSContext ctx1 ctx2, PairGState s1 s2, (c1, c2))
    shutdownGlobalState _ (PairGSContext ctx1 ctx2) (PairGState s1 s2) (c1, c2) = do
            shutdownGlobalState (Proxy :: Proxy c1) ctx1 s1 c1
            shutdownGlobalState (Proxy :: Proxy c2) ctx2 s2 c2
