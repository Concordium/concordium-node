{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module SchedulerTests.Delegation where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as Map
import qualified Data.HashSet as Set

import qualified Concordium.Scheduler.EnvironmentImplementation as EI
import qualified Acorn.Utils.Init as Init
import Concordium.Scheduler.Runner
import qualified Acorn.Parser.Runner as PR
import qualified Concordium.Scheduler as Sch
import qualified Concordium.Scheduler.Cost as Cost

import Concordium.GlobalState.Implementation.BlockState
import Concordium.GlobalState.Implementation.Invariants
import Concordium.GlobalState.Account as Acc
import Concordium.GlobalState.Modules as Mod
import Concordium.GlobalState.Rewards as Rew

import Concordium.Crypto.SignatureScheme as Sig
import Concordium.ID.Account
import Concordium.Crypto.Ed25519Signature as EdSig

import Concordium.GlobalState.Bakers
import Concordium.Scheduler.Types hiding (accountAddress, Payload(..))

import System.Random
import Lens.Micro.Platform

import SchedulerTests.DummyData

staticKeys :: [KeyPair]
staticKeys = ks (mkStdGen 1333)
    where
        ks g = let (k, g') = EdSig.randomKeyPair g in k : ks g'

numAccounts :: Int
numAccounts = 10

-- NB: we add two special admin accounts that will have the ability to add and remove
-- bakers.
-- This is the logic that will likely be changed after the beta, or in its later stages,
-- but at the moment we need it since we lack an alternative governance specification.
initialBlockState :: BlockState
initialBlockState =
    emptyBlockState emptyBirkParameters dummyCryptographicParameters &
        (blockAccounts .~ foldr addAcc Acc.emptyAccounts (take numAccounts staticKeys ++ [alesKP, thomasKP])) .
        (blockBank . Rew.totalGTU .~ fromIntegral (numAccounts + 2) * initBal) .
        (blockModules .~ (let (_, _, gs) = Init.baseState in Mod.fromModuleList (Init.moduleList gs)))
    where
        addAcc kp = Acc.putAccount (mkAccount (verifyKey kp) initBal )
        initBal = 10^(12::Int) :: Amount

data Model = Model {
    _mAccounts :: Map.Map AccountAddress (KeyPair, Nonce),
    _mBakers :: [BakerId],
    _mNextBaker :: BakerId,
    _mAdminAccounts :: Map.Map AccountAddress (KeyPair, Nonce)
}
makeLenses ''Model

initialModel :: Model
initialModel = Model {
    _mAccounts = Map.fromList [(accountAddress (verifyKey kp) Ed25519, (kp, minNonce)) | kp <- take numAccounts staticKeys],
    _mBakers = [],
    _mNextBaker = minBound,
    _mAdminAccounts = Map.fromList [(alesAccount, (alesKP, minNonce)), (thomasAccount, (thomasKP, minNonce))]
}

addBaker :: Model -> Gen (TransactionJSON, Model)
addBaker m0 = do
        bkrSeed <- arbitrary
        bkrAcct <- elements (Map.keys $ _mAccounts m0)
        let bkr = mkBaker bkrSeed bkrAcct
        (srcAcct, (srcKp, srcN)) <- elements (Map.toList $ _mAdminAccounts m0)
        return (TJSON {
            payload = AddBaker (bkr ^. bakerElectionVerifyKey) (bkr ^. bakerSignatureVerifyKey) bkrAcct "<dummy proof>",
            metadata = makeHeader srcKp srcN (Cost.checkHeader + Cost.addBaker),
            keypair = srcKp
        }, m0
            & mAdminAccounts . ix srcAcct . _2 %~ (+1)
            & mBakers %~ (_mNextBaker m0 :)
            & mNextBaker %~ (+1))

takeOne :: [a] -> Gen (a, [a])
takeOne l = do
        i <- choose (0, length l - 1)
        return (l !! i, take i l ++ drop (i+1) l)

removeBaker :: Model -> Gen (TransactionJSON, Model)
removeBaker m0 = do
        (bkr, bkrs') <- takeOne (_mBakers m0)
        (srcAcct, (srcKp, srcN)) <- elements (Map.toList $ _mAdminAccounts m0)
        return (TJSON {
            payload = RemoveBaker bkr "<dummy proof>",
            metadata = makeHeader srcKp srcN (Cost.checkHeader + Cost.removeBaker),
            keypair = srcKp
        }, m0
            & mAdminAccounts . ix srcAcct . _2 %~ (+1)
            & mBakers .~ bkrs')

delegateStake :: Model -> Gen (TransactionJSON, Model)
delegateStake m0 = do
        (srcAcct, (srcKp, srcN)) <- elements (Map.toList $ _mAccounts m0)
        bkr <- elements (_mBakers m0)
        return (TJSON {
            payload = DelegateStake bkr,
            metadata = makeHeader srcKp srcN (Cost.checkHeader + Cost.updateStakeDelegate 0),
            keypair = srcKp
        }, m0 & mAccounts . ix srcAcct . _2 %~ (+1))

undelegateStake :: Model -> Gen (TransactionJSON, Model)
undelegateStake m0 = do
        (srcAcct, (srcKp, srcN)) <- elements (Map.toList $ _mAccounts m0)
        return (TJSON {
            payload = UndelegateStake,
            metadata = makeHeader srcKp srcN (Cost.checkHeader + Cost.updateStakeDelegate 0),
            keypair = srcKp
        }, m0 & mAccounts . ix srcAcct . _2 %~ (+1))

simpleTransfer :: Model -> Gen (TransactionJSON, Model)
simpleTransfer m0 = do
        (srcAcct, (srcKp, srcN)) <- elements (Map.toList $ _mAccounts m0)
        destAcct <- elements $ Map.keys $ _mAccounts m0
        amt <- fromIntegral <$> choose (0, 1000 :: Word)
        return (TJSON {
            payload = Transfer {toaddress = AddressAccount destAcct, amount = amt},
            metadata = makeHeader srcKp srcN Cost.checkHeader,
            keypair = srcKp
        }, m0 & mAccounts . ix srcAcct . _2 %~ (+1))

makeTransactions :: Gen [TransactionJSON]
makeTransactions = sized (mt initialModel)
    where
        mt m sz
            | sz > 0 = do
                tg <- elements $ [addBaker, simpleTransfer] ++ if null (_mBakers m) then [] else [SchedulerTests.Delegation.removeBaker, delegateStake, undelegateStake]
                (t, m') <- tg m
                (t :) <$> mt m' (sz - 1)
            | otherwise = return []

testTransactions :: Property
testTransactions = forAll makeTransactions (ioProperty . PR.evalContext Init.initialContextData . tt)
    where
        tt tl = do
            transactions <- processTransactions tl
            let ((Sch.FilteredTransactions{..}, _), gs) =
                  EI.runSI
                    (Sch.filterTransactions blockSize transactions)
                    (Set.fromList [alesAccount, thomasAccount])
                    dummyChainMeta
                    initialBlockState
            let rejs = [(z, decodePayload (btrPayload z), rr) | (z, TxReject rr _ _) <- ftAdded]
            case invariantBlockState gs >> (if null ftFailed then Right () else Left ("some transactions failed: " ++ show ftFailed))
                >> (if null rejs then Right () else Left ("some transactions rejected: " ++ show rejs)) of
                Left f -> return $ counterexample f False
                Right _ -> return $ property True

tests :: Spec
tests = describe "SchedulerTests.Delegation" $ do
    it "Delegation" $ withMaxSuccess 1000 $ testTransactions
