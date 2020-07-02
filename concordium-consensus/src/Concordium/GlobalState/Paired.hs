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
    deriving (Functor, Applicative, Monad, MonadIO)

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

-- Since we use the 'ReviseRSM' type just for testing with the Paired state, and the global-state monad implementations
-- below provide testing assertions for the Paired state with the 'ReviseRSM' type, we just provide a placeholder
-- logging implementation for 'ReviseRSM' here. However, if logging is required, this needs to be implemented.
instance Monad m => MonadLogger (ReviseRSM r s m) where
  logEvent _ _ _ = return ()

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

type BSML lc r ls s m = BlockStateM lc (FocusLeft r) ls (FocusLeft s) (ReviseRSM (FocusLeft r) (FocusLeft s) m)
type BSMR rc r rs s m = BlockStateM rc (FocusRight r) rs (FocusRight s) (ReviseRSM (FocusRight r) (FocusRight s) m)

instance (C.HasGlobalStateContext (PairGSContext lc rc) r)
        => BlockStateTypes (BlockStateM (PairGSContext lc rc) r (PairGState lg rg) s m) where
    type BlockState (BlockStateM (PairGSContext lc rc) r (PairGState lg rg) s m)
            = (BlockState (BSML lc r lg s m),
                BlockState (BSMR rc r rg s m))
    type UpdatableBlockState (BlockStateM (PairGSContext lc rc) r (PairGState lg rg) s m)
            = (UpdatableBlockState (BSML lc r lg s m),
                UpdatableBlockState (BSMR rc r rg s m))
    type BirkParameters (BlockStateM (PairGSContext lc rc) r (PairGState lg rg) s m)
            = (BirkParameters (BSML lc r lg s m),
                BirkParameters (BSMR rc r rg s m))
    type Bakers (BlockStateM (PairGSContext lc rc) r (PairGState lg rg) s m)
            = (Bakers (BSML lc r lg s m),
                Bakers (BSMR rc r rg s m))
    type Account (BlockStateM (PairGSContext lc rc) r (PairGState lg rg) s m)
            = (Account (BSML lc r lg s m),
                Account (BSMR rc r rg s m))

instance C.HasGlobalState (PairGState ls rs) s => C.HasGlobalState ls (FocusLeft s) where
    globalState = lens unFocusLeft (const FocusLeft) . C.globalState . pairStateLeft

instance C.HasGlobalState (PairGState ls rs) s => C.HasGlobalState rs (FocusRight s) where
    globalState = lens unFocusRight (const FocusRight) . C.globalState . pairStateRight

-- * Block Metadata
newtype PairBlockMetadata l r = PairBlockMetadata (l, r)
instance (BlockMetadata l, BlockMetadata r) => BlockMetadata (PairBlockMetadata l r) where
    blockPointer (PairBlockMetadata (l, r)) = assert (blockPointer l == blockPointer r) $ blockPointer l
    blockBaker (PairBlockMetadata (l, r)) = assert (blockBaker l == blockBaker r) $ blockBaker l
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
    verifyBlockSignature k (PairBlockData (l, r)) = assert (vbsl == verifyBlockSignature k r) $ vbsl
        where
            vbsl = verifyBlockSignature k l
    putBlock (PairBlockData (l, _)) = putBlock l

instance (HashableTo BlockHash l, HashableTo BlockHash r) => HashableTo BlockHash (PairBlockData l r) where
    getHash (PairBlockData (l, r)) = assert ((getHash l :: BlockHash) == getHash r) $ getHash l

instance (Show l, Show r) => Show (PairBlockData l r) where
    show (PairBlockData (l, r)) = "(" ++ show l ++ ", " ++ show r ++ ")"

instance (BlockPendingData l, BlockPendingData r) => BlockPendingData (PairBlockData l r) where
    blockReceiveTime (PairBlockData (l, r)) = assert (blockReceiveTime l == blockReceiveTime r) $ blockReceiveTime l

instance (Eq l, Eq r) => Eq (PairBlockData l r) where
    (PairBlockData (l1, r1)) == (PairBlockData (l2, r2)) = assert ((l1 == l2) == (r1 == r2)) $ (l1 == l2)

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

type GSML lc r ls s m = GlobalStateM NoLogContext lc (FocusLeft r) ls (FocusLeft s) (ReviseRSM (FocusLeft r) (FocusLeft s) m)
type GSMR rc r rs s m = GlobalStateM NoLogContext rc (FocusRight r) rs (FocusRight s) (ReviseRSM (FocusRight r) (FocusRight s) m)

instance (GlobalStateTypes (GSML lc r ls s m), GlobalStateTypes (GSMR rc r rs s m))
        => GlobalStateTypes (TreeStateBlockStateM (PairGState ls rs) (PairGSContext lc rc) r s m) where
    type BlockPointerType (TreeStateBlockStateM (PairGState ls rs) (PairGSContext lc rc) r s m) = PairBlockData (BlockPointerType (GSML lc r ls s m)) (BlockPointerType (GSMR rc r rs s m))

instance ATITypes (TreeStateBlockStateM (PairGState ls rs) (PairGSContext lc rc) r s m) where
  type ATIStorage (TreeStateBlockStateM (PairGState ls rs) (PairGSContext lc rc) r s m) = ()

{-# INLINE coerceBSML #-}
coerceBSML :: BSML lc r ls s m a -> BlockStateM (PairGSContext lc rc) r (PairGState ls rs) s m a
coerceBSML = coerce

{-# INLINE coerceBSMR #-}
coerceBSMR :: BSMR rc r rs s m a -> BlockStateM (PairGSContext lc rc) r (PairGState ls rs) s m a
coerceBSMR = coerce

instance (Monad m, C.HasGlobalStateContext (PairGSContext lc rc) r, BlockStateQuery (BSML lc r ls s m), BlockStateQuery (BSMR rc r rs s m), HashableTo H.Hash (Account (BSML lc r ls s m)), HashableTo H.Hash (Account (BSMR rc r rs s m)))
        => BlockStateQuery (BlockStateM (PairGSContext lc rc) r (PairGState ls rs) s m) where
    getModule (ls, rs) modRef = do
        m1 <- coerceBSML (getModule ls modRef)
        m2 <- coerceBSMR (getModule rs modRef)
        assert (((==) `on` (fmap moduleSource)) m1 m2) $ return m1
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
    getBlockBirkParameters (ls, rs) = do
        a1 <- coerceBSML $ getBlockBirkParameters ls
        a2 <- coerceBSMR $ getBlockBirkParameters rs
        seedState1 <- coerceBSML (getSeedState a1)
        seedState2 <- coerceBSMR (getSeedState a2)
        assert (seedState1 == seedState2) $ return (a1, a2)
    getRewardStatus (ls, rs) = do
        a1 <- coerceBSML (getRewardStatus ls)
        a2 <- coerceBSMR (getRewardStatus rs)
        assert (a1 == a2) $ return a1
    getTransactionOutcome (ls, rs) th = do
        a1 <- coerceBSML (getTransactionOutcome ls th)
        a2 <- coerceBSMR (getTransactionOutcome rs th)
        assert (a1 == a2) $ return a1
    getOutcomes (ls, rs) = do
        a1 <- coerceBSML (getOutcomes ls)
        a2 <- coerceBSMR (getOutcomes rs)
        assert (a1 == a2) $ return a1
    getSpecialOutcomes (ls, rs) = do
        a1 <- coerceBSML (getSpecialOutcomes ls)
        a2 <- coerceBSMR (getSpecialOutcomes rs)
        assert (a1 == a2) $ return a1

instance (Monad m, C.HasGlobalStateContext (PairGSContext lc rc) r, AccountOperations (BSML lc r ls s m), AccountOperations (BSMR rc r rs s m), HashableTo H.Hash (Account (BSML lc r ls s m)), HashableTo H.Hash (Account (BSMR rc r rs s m)))
  => AccountOperations (BlockStateM (PairGSContext lc rc) r (PairGState ls rs) s m) where

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

    getAccountStakeDelegate (acc1, acc2) = do
        bid1 <- coerceBSML (getAccountStakeDelegate acc1)
        bid2 <- coerceBSMR (getAccountStakeDelegate acc2)
        assert (bid1 == bid2) $ return bid1

    getAccountInstances (acc1, acc2) = do
        ais1 <- coerceBSML (getAccountInstances acc1)
        ais2 <- coerceBSMR (getAccountInstances acc2)
        assert (ais1 == ais2) $ return ais1

    createNewAccount keys addr regId = do
        acc1 <- coerceBSML (createNewAccount keys addr regId)
        acc2 <- coerceBSMR (createNewAccount keys addr regId)
        assert ((getHash acc1 :: H.Hash) == getHash acc2) $
          return (acc1, acc2)

    updateAccountAmount (acc1, acc2) amnt = do
        acc1' <- coerceBSML (updateAccountAmount acc1 amnt)
        acc2' <- coerceBSMR (updateAccountAmount acc2 amnt) 
        assert ((getHash acc1 :: H.Hash) == getHash acc2) $
          return (acc1', acc2')

instance (Monad m, C.HasGlobalStateContext (PairGSContext lc rc) r, BakerQuery (BSML lc r ls s m), BakerQuery (BSMR rc r rs s m))
        => BakerQuery (BlockStateM (PairGSContext lc rc) r (PairGState ls rs) s m) where
  getBakerStake (bkrs1, bkrs2) bid = do
    s1 <- coerceBSML (getBakerStake bkrs1 bid)
    s2 <- coerceBSMR (getBakerStake bkrs2 bid)
    assert (s1 == s2) $ return s1

  getBakerFromKey (bkrs1, bkrs2) k = do
    b1 <- coerceBSML (getBakerFromKey bkrs1 k)
    b2 <- coerceBSMR (getBakerFromKey bkrs2 k)
    assert (b1 == b2) $ return b1

  getTotalBakerStake (bkrs1, bkrs2) = do
    s1 <- coerceBSML (getTotalBakerStake bkrs1)
    s2 <- coerceBSMR (getTotalBakerStake bkrs2)
    assert (s1 == s2) $ return s1

  getBakerInfo (bkrs1, bkrs2) bid = do
    bi1 <- coerceBSML (getBakerInfo bkrs1 bid)
    bi2 <- coerceBSMR (getBakerInfo bkrs2 bid)
    assert (bi1 == bi2) $ return bi1

  getFullBakerInfos (bkrs1, bkrs2) = do
    bi1 <- coerceBSML (getFullBakerInfos bkrs1)
    bi2 <- coerceBSMR (getFullBakerInfos bkrs2)
    assert (bi1 == bi2) $ return bi1

instance (Monad m,
          C.HasGlobalStateContext (PairGSContext lc rc) r,
          BirkParametersOperations (BSML lc r ls s m),
          BirkParametersOperations (BSMR rc r rs s m))
        => BirkParametersOperations (BlockStateM (PairGSContext lc rc) r (PairGState ls rs) s m) where
  getSeedState (bps1, bps2) = do
    s1 <- coerceBSML (getSeedState bps1)
    s2 <- coerceBSMR (getSeedState bps2)
    assert (s1 == s2) $ return s1

  updateBirkParametersForNewEpoch ss (bps1, bps2) = do
    bps1' <- coerceBSML (updateBirkParametersForNewEpoch ss bps1)
    bps2' <- coerceBSMR (updateBirkParametersForNewEpoch ss bps2)
    return (bps1', bps2')

  getElectionDifficulty (bps1, bps2) = do
    e1 <- coerceBSML (getElectionDifficulty bps1)
    e2 <- coerceBSMR (getElectionDifficulty bps2)
    assert (e1 == e2) $ return e1

  getCurrentBakers (bps1, bps2) = do
    cb1 <- coerceBSML (getCurrentBakers bps1)
    cb2 <- coerceBSMR (getCurrentBakers bps2)
    fbi1 <- coerceBSML (getFullBakerInfos cb1)
    fbi2 <- coerceBSMR (getFullBakerInfos cb2)
    assert (fbi1 == fbi2) $ return (cb1, cb2)

  getLotteryBakers (bps1, bps2) = do
    lb1 <- coerceBSML (getLotteryBakers bps1)
    lb2 <- coerceBSMR (getLotteryBakers bps2)
    fbi1 <- coerceBSML (getFullBakerInfos lb1)
    fbi2 <- coerceBSMR (getFullBakerInfos lb2)
    assert (fbi1 == fbi2) $ return (lb1, lb2)

  updateSeedState ss (bps1, bps2) = do
    bps1' <- coerceBSML (updateSeedState ss bps1)
    bps2' <- coerceBSMR (updateSeedState ss bps2)
    return (bps1', bps2')


instance (MonadLogger m, C.HasGlobalStateContext (PairGSContext lc rc) r, BlockStateOperations (BSML lc r ls s m), BlockStateOperations (BSMR rc r rs s m), HashableTo H.Hash (Account (BSML lc r ls s m)), HashableTo H.Hash (Account (BSMR rc r rs s m)), HashableTo H.Hash (Account (BSML lc r ls s m)), HashableTo H.Hash (Account (BSMR rc r rs s m)))
        => BlockStateOperations (BlockStateM (PairGSContext lc rc) r (PairGState ls rs) s m) where
    bsoGetModule (bs1, bs2) mref = do
        r1 <- coerceBSML $ bsoGetModule bs1 mref
        r2 <- coerceBSMR $ bsoGetModule bs2 mref
        assert (((==) `on` (fmap moduleSource)) r1 r2) $ return r1
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
    bsoGetInstance (bs1, bs2) iref = do
        r1 <- coerceBSML $ bsoGetInstance bs1 iref
        r2 <- coerceBSMR $ bsoGetInstance bs2 iref
        assert (((==) `on` fmap instanceHash) r1 r2) $ return r1
    bsoRegIdExists (bs1, bs2) regid = do
        r1 <- coerceBSML $ bsoRegIdExists bs1 regid
        r2 <- coerceBSMR $ bsoRegIdExists bs2 regid
        assert (r1 == r2) $ return r1
    bsoPutNewAccount (bs1, bs2) (acct1, acct2) = do
        (r1, bs1') <- coerceBSML $ bsoPutNewAccount bs1 acct1
        (r2, bs2') <- coerceBSMR $ bsoPutNewAccount bs2 acct2
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoSetElectionDifficulty (bs1, bs2) diff = do
        bs1' <- coerceBSML $ bsoSetElectionDifficulty bs1 diff
        bs2' <- coerceBSMR $ bsoSetElectionDifficulty bs2 diff
        return (bs1', bs2')
    bsoPutNewInstance (bs1, bs2) f = do
        (r1, bs1') <- coerceBSML $ bsoPutNewInstance bs1 f
        (r2, bs2') <- coerceBSMR $ bsoPutNewInstance bs2 f
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoPutNewModule (bs1, bs2) mref iface uvi modul = do
        (r1, bs1') <- coerceBSML $ bsoPutNewModule bs1 mref iface uvi modul
        (r2, bs2') <- coerceBSMR $ bsoPutNewModule bs2 mref iface uvi modul
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoTryGetLinkedExpr (bs1, bs2) modref name = do
        r1 <- coerceBSML $ bsoTryGetLinkedExpr bs1 modref name
        r2 <- coerceBSMR $ bsoTryGetLinkedExpr bs2 modref name
        case (r1, r2) of
            (Just le1, Just le2) -> assert (le1 == le2) $ return r1
            _ -> return r1
    bsoPutLinkedExpr (bs1, bs2) modref name le = do
        bs1' <- coerceBSML $ bsoPutLinkedExpr bs1 modref name le
        bs2' <- coerceBSMR $ bsoPutLinkedExpr bs2 modref name le
        return (bs1', bs2')
    bsoTryGetLinkedContract (bs1, bs2) modref name = do
        r1 <- coerceBSML $ bsoTryGetLinkedContract bs1 modref name
        r2 <- coerceBSMR $ bsoTryGetLinkedContract bs2 modref name
        case (r1, r2) of
            (Just le1, Just le2) -> assert (le1 == le2) $ return r1
            _ -> return r1
    bsoPutLinkedContract (bs1, bs2) modref name lc = do
        bs1' <- coerceBSML $ bsoPutLinkedContract bs1 modref name lc
        bs2' <- coerceBSMR $ bsoPutLinkedContract bs2 modref name lc
        return (bs1', bs2')
    bsoModifyAccount (bs1, bs2) upd = do
        bs1' <- coerceBSML $ bsoModifyAccount bs1 upd
        bs2' <- coerceBSMR $ bsoModifyAccount bs2 upd
        return (bs1', bs2')
    bsoModifyInstance (bs1, bs2) caddr delta model = do
        bs1' <- coerceBSML $ bsoModifyInstance bs1 caddr delta model
        bs2' <- coerceBSMR $ bsoModifyInstance bs2 caddr delta model
        return (bs1', bs2')
    bsoNotifyExecutionCost (bs1, bs2) amt = do
        bs1' <- coerceBSML $ bsoNotifyExecutionCost bs1 amt
        bs2' <- coerceBSMR $ bsoNotifyExecutionCost bs2 amt
        return (bs1', bs2')
    bsoNotifyIdentityIssuerCredential (bs1, bs2) idid = do
        bs1' <- coerceBSML $ bsoNotifyIdentityIssuerCredential bs1 idid
        bs2' <- coerceBSMR $ bsoNotifyIdentityIssuerCredential bs2 idid
        return (bs1', bs2')
    bsoGetExecutionCost (bs1, bs2) = do
        r1 <- coerceBSML $ bsoGetExecutionCost bs1
        r2 <- coerceBSMR $ bsoGetExecutionCost bs2
        assert (r1 == r2) $ return r1
    bsoGetBlockBirkParameters (bs1, bs2) = do
        r1 <- coerceBSML $ bsoGetBlockBirkParameters bs1
        r2 <- coerceBSMR $ bsoGetBlockBirkParameters bs2
        return (r1, r2)
    bsoAddBaker (bs1, bs2) bci = do
        (r1, bs1') <- coerceBSML $ bsoAddBaker bs1 bci
        (r2, bs2') <- coerceBSMR $ bsoAddBaker bs2 bci
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoUpdateBaker (bs1, bs2) bupd = do
        (r1, bs1') <- coerceBSML $ bsoUpdateBaker bs1 bupd
        (r2, bs2') <- coerceBSMR $ bsoUpdateBaker bs2 bupd
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoRemoveBaker (bs1, bs2) bid = do
        (r1, bs1') <- coerceBSML $ bsoRemoveBaker bs1 bid
        (r2, bs2') <- coerceBSMR $ bsoRemoveBaker bs2 bid
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoSetInflation (bs1, bs2) rate = do
        bs1' <- coerceBSML $ bsoSetInflation bs1 rate
        bs2' <- coerceBSMR $ bsoSetInflation bs2 rate
        return (bs1', bs2')
    bsoMint (bs1, bs2) amt = do
        (r1, bs1') <- coerceBSML $ bsoMint bs1 amt
        (r2, bs2') <- coerceBSMR $ bsoMint bs2 amt
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoDecrementCentralBankGTU (bs1, bs2) amt = do
        (r1, bs1') <- coerceBSML $ bsoDecrementCentralBankGTU bs1 amt
        (r2, bs2') <- coerceBSMR $ bsoDecrementCentralBankGTU bs2 amt
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoDelegateStake (bs1, bs2) acct bid = do
        (r1, bs1') <- coerceBSML $ bsoDelegateStake bs1 acct bid
        (r2, bs2') <- coerceBSMR $ bsoDelegateStake bs2 acct bid
        assert (r1 == r2) $ return (r1, (bs1', bs2'))
    bsoGetIdentityProvider (bs1, bs2) ipid = do
        r1 <- coerceBSML $ bsoGetIdentityProvider bs1 ipid
        r2 <- coerceBSMR $ bsoGetIdentityProvider bs2 ipid
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
    bsoUpdateBirkParameters (bs1, bs2) (bps1, bps2) = do
        bs1' <- coerceBSML $ bsoUpdateBirkParameters bs1 bps1
        bs2' <- coerceBSMR $ bsoUpdateBirkParameters bs2 bps2
        return (bs1', bs2')

instance (MonadLogger m,
    C.HasGlobalStateContext (PairGSContext lc rc) r,
    BlockStateStorage (BSML lc r ls s m),
    BlockStateStorage (BSMR rc r rs s m),
    HashableTo H.Hash (Account (BSML lc r ls s m)),
    HashableTo H.Hash (Account (BSMR rc r rs s m)))
        => BlockStateStorage (BlockStateM (PairGSContext lc rc) r (PairGState ls rs) s m) where
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
    putBlockState (bs1, bs2) = do
        p1 <- coerceBSML $ putBlockState bs1
        p2 <- coerceBSMR $ putBlockState bs2
        return $ p1 >> p2
    getBlockState = do
        g1 <- getBlockState
        g2 <- getBlockState
        return $ do
            bs1 <- coerceBSML g1
            bs2 <- coerceBSMR g2
            return (bs1, bs2)

{-# INLINE coerceGSML #-}
coerceGSML :: GSML lc r ls s m a -> TreeStateBlockStateM (PairGState ls rs) (PairGSContext lc rc) r s m a
coerceGSML = coerce

{-# INLINE coerceGSMR #-}
coerceGSMR :: GSMR rc r rs s m a -> TreeStateBlockStateM (PairGState ls rs) (PairGSContext lc rc) r s m a
coerceGSMR = coerce

-- default instance since ATIStorage = ()
instance Monad m => PerAccountDBOperations (TreeStateBlockStateM (PairGState ls rs) (PairGSContext lc rc) r s m)

instance (C.HasGlobalStateContext (PairGSContext lc rc) r,
          MonadReader r m,
          C.HasGlobalState (PairGState ls rs) s,
          MonadState s m,
          MonadIO m,
          BlockPointerMonad (GSML lc r ls s m),
          BlockPointerMonad (GSMR rc r rs s m))
          => BlockPointerMonad (TreeStateBlockStateM (PairGState ls rs) (PairGSContext lc rc) r s m) where
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
        BlockStateStorage (TreeStateBlockStateM (PairGState ls rs) (PairGSContext lc rc) r s m),
        TreeStateMonad (GSML lc r ls s m),
        ATIStorage (GSML lc r ls s m) ~ (),
        TreeStateMonad (GSMR rc r rs s m),
        ATIStorage (GSMR rc r rs s m) ~ ())
        => TreeStateMonad (TreeStateBlockStateM (PairGState ls rs) (PairGSContext lc rc) r s m) where
    makePendingBlock sk sl parent bid bp bn lf trs brtime = do
      pb1 <- coerceGSML $ TS.makePendingBlock sk sl parent bid bp bn lf trs brtime
      pb2 <- coerceGSMR $ TS.makePendingBlock sk sl parent bid bp bn lf trs brtime
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

    purgeTransactionTable = coerceGSML purgeTransactionTable

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
