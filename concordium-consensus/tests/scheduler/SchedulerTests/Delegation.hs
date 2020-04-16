{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
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

import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants
import Concordium.GlobalState.Basic.BlockState.Account as Acc

import Concordium.Crypto.SignatureScheme as Sig
import Concordium.ID.Types(randomAccountAddress)

import Concordium.GlobalState.Bakers
import Concordium.Scheduler.Types hiding (accountAddress, Payload(..))

import System.Random
import Lens.Micro.Platform

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

import SchedulerTests.Helpers

-- | The amount of energy to deposit in every test transaction.
energy :: Energy
energy = 10000

staticKeys :: [(KeyPair, AccountAddress)]
staticKeys = ks (mkStdGen 1333)
    where
        ks g = let (k, g') = randomEd25519KeyPair g
                   (addr, g'') = randomAccountAddress g'
               in (uncurry KeyPairEd25519 k, addr) : ks g''

numAccounts :: Int
numAccounts = 10

initialBlockState :: BlockState
initialBlockState = createBlockState
    (foldr addAcc Acc.emptyAccounts (take numAccounts staticKeys))
    (fromIntegral numAccounts * initBal)
    where
        addAcc (kp, addr) = Acc.putAccountWithRegIds (mkAccount (correspondingVerifyKey kp) addr initBal )
        initBal = 10^(12::Int) :: Amount

data Model = Model {
    _mAccounts :: Map.Map AccountAddress (KeyPair, Nonce),
    _mBakers :: [BakerId],
    _mNextBaker :: BakerId,
    _mNextSeed :: Int,
    _mBakerMap :: Map.Map BakerId (AccountAddress, KeyPair) -- mapping of baker ids to their associated account keypairs
}
makeLenses ''Model

initialModel :: Model
initialModel = Model {
    _mAccounts = Map.fromList [(addr, (kp, minNonce)) | (kp, addr) <- take numAccounts staticKeys],
    _mBakers = [],
    _mNextBaker = minBound,
    _mNextSeed = 0,
    _mBakerMap = Map.empty
}

addBaker :: Model -> Gen (TransactionJSON, Model)
addBaker m0 = do
        (bkrAcct, (kp, nonce)) <- elements (Map.toList $ _mAccounts m0)
        -- FIXME: Once we require proof of knowledge of this key the last secret aggregation key
        -- will be needed.
        let (bkr, electionSecretKey, signKey, aggregationKey) = mkFullBaker (m0 ^. mNextSeed) bkrAcct
        return (TJSON {
            payload = AddBaker
                      (bkr ^. bakerElectionVerifyKey)
                      electionSecretKey
                      (bkr ^. bakerSignatureVerifyKey)
                      (bkr ^. bakerAggregationVerifyKey)
                      aggregationKey
                      signKey
                      bkrAcct
                      kp,
            metadata = makeDummyHeader bkrAcct nonce energy,
            keypair = kp
        }, m0
            & mAccounts . ix bkrAcct . _2 %~ (+1)
            & mBakers %~ (_mNextBaker m0 :)
            & mNextBaker %~ (+1)
            & mNextSeed +~ 1
            & mBakerMap %~ (Map.insert (m0 ^. mNextBaker) (bkrAcct, kp))
               )

takeOne :: [a] -> Gen (a, [a])
takeOne l = do
        i <- choose (0, length l - 1)
        return (l !! i, take i l ++ drop (i+1) l)

removeBaker :: Model -> Gen (TransactionJSON, Model)
removeBaker m0 = do
        (bkr, bkrs') <- takeOne (_mBakers m0)
        let (address, srcKp) = m0 ^. mBakerMap . singular (ix bkr)
        let (_, srcN) = m0 ^. mAccounts . singular (ix address)
        return (TJSON {
            payload = RemoveBaker bkr,
            metadata = makeDummyHeader address srcN energy,
            keypair = srcKp
        }, m0
            & mAccounts . ix address . _2 %~ (+1)
            & mBakers .~ bkrs'
            & mBakerMap %~ (Map.delete bkr))

delegateStake :: Model -> Gen (TransactionJSON, Model)
delegateStake m0 = do
        (srcAcct, (srcKp, srcN)) <- elements (Map.toList $ _mAccounts m0)
        bkr <- elements (_mBakers m0)
        return (TJSON {
            payload = DelegateStake bkr,
            metadata = makeDummyHeader srcAcct srcN energy,
            keypair = srcKp
        }, m0 & mAccounts . ix srcAcct . _2 %~ (+1))

undelegateStake :: Model -> Gen (TransactionJSON, Model)
undelegateStake m0 = do
        (srcAcct, (srcKp, srcN)) <- elements (Map.toList $ _mAccounts m0)
        return (TJSON {
            payload = UndelegateStake,
            metadata = makeDummyHeader srcAcct srcN energy,
            keypair = srcKp
        }, m0 & mAccounts . ix srcAcct . _2 %~ (+1))

simpleTransfer :: Model -> Gen (TransactionJSON, Model)
simpleTransfer m0 = do
        (srcAcct, (srcKp, srcN)) <- elements (Map.toList $ _mAccounts m0)
        destAcct <- elements $ Map.keys $ _mAccounts m0
        amt <- fromIntegral <$> choose (0, 1000 :: Word)
        return (TJSON {
            payload = Transfer {toaddress = AddressAccount destAcct, amount = amt},
            metadata = makeDummyHeader srcAcct srcN energy,
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
            transactions <- processUngroupedTransactions tl
            let (Sch.FilteredTransactions{..}, finState) =
                  EI.runSI
                    (Sch.filterTransactions dummyBlockSize transactions)
                    (Set.fromList [alesAccount, thomasAccount])
                    dummyChainMeta
                    maxBound
                    initialBlockState
            let gs = finState ^. EI.ssBlockState
            let rejs = [(z, decodePayload (btrPayload z), rr) | (WithMetadata{wmdData=NormalTransaction z}, TxReject rr) <- getResults ftAdded]
            case invariantBlockState gs >> (if null ftFailed then Right () else Left ("some transactions failed: " ++ show ftFailed))
                >> (if null rejs then Right () else Left ("some transactions rejected: " ++ show rejs)) of
                Left f -> return $ counterexample f False
                Right _ -> return $ property True

tests :: Spec
tests = describe "SchedulerTests.Delegation" $
    it "Delegation" $ withMaxSuccess 1000 testTransactions
