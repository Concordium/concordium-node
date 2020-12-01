{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module SchedulerTests.RandomBakerTransactions where

import Test.Hspec
import Test.QuickCheck

import qualified Data.Map as Map
import Control.Monad

import qualified Concordium.Scheduler.EnvironmentImplementation as EI
import Concordium.Scheduler.Runner
import qualified Concordium.Scheduler as Sch

import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState
import Concordium.GlobalState.Basic.BlockState.Invariants
import Concordium.GlobalState.Basic.BlockState.Accounts as Acc

import Concordium.Crypto.SignatureScheme as Sig
import Concordium.ID.Types(randomAccountAddress)

import Concordium.Scheduler.Types hiding (Payload(..))

import System.Random
import Lens.Micro.Platform

import Concordium.Scheduler.DummyData
import Concordium.GlobalState.DummyData
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
    where
        addAcc (kp, addr) = Acc.putAccountWithRegIds (mkAccount (correspondingVerifyKey kp) addr initBal )
        initBal = 10^(12::Int) :: Amount

data BakerStatus
    = NoBaker
    -- ^No baker on the account
    | Baker Amount
    -- ^Baker with given stake
    | ReducedRemoved
    -- ^The baker's stake was reduced, or the baker was removed.
    -- This means the change should be pending and prevents other changes.
    deriving (Eq,Show)

data Model = Model {
    -- |For each account, the keys, next nonce and whether the account is a baker.
    _mAccounts :: Map.Map AccountAddress (KeyPair, Nonce, BakerStatus),
    -- |Next seed to use for generating baker keys
    _mNextSeed :: Int,
    -- |The transactions that should be rejected (latest first)
    _mRejects :: [(AccountAddress, Nonce)]
}
makeLenses ''Model

initialModel :: Model
initialModel = Model {
    _mAccounts = Map.fromList [(addr, (kp, minNonce, NoBaker)) | (kp, addr) <- take numAccounts staticKeys],
    _mNextSeed = 0,
    _mRejects = []
}

updateBaker :: Model -> Gen (TransactionJSON, Model)
updateBaker m0 = do
        (bkrAcct, (kp, nonce, currentlyBaker)) <- elements $ Map.toList $ m0 ^. mAccounts
        let toTJSON p = TJSON {
            payload = p,
            metadata = makeDummyHeader bkrAcct nonce energy,
            keys = [(0, kp)]
        }
        let changeStakeFail = case currentlyBaker of
                Baker _ -> id
                _ -> mRejects %~ ((bkrAcct, nonce) :)
        let changeBakerFail = case currentlyBaker of
                NoBaker -> mRejects %~ ((bkrAcct, nonce) :)
                _ -> id
        let removeBaker = do
                let changeBakerState (Baker _) = ReducedRemoved
                    changeBakerState s = s
                return (
                    toTJSON RemoveBaker,
                    m0
                    & mAccounts . ix bkrAcct %~ ((_2 +~ 1) . (_3 %~ changeBakerState))
                    & changeStakeFail)
            updateBakerStake = do
                newStake <- elements [0,1,10,100,1000,10000,100000,1000000]
                let changeBakerState (Baker v)
                        | v > newStake = ReducedRemoved
                        | otherwise = Baker newStake
                    changeBakerState s = s
                return (
                    toTJSON (UpdateBakerStake newStake),
                    m0
                    & mAccounts . ix bkrAcct %~ ((_2 +~ 1) . (_3 %~ changeBakerState))
                    & changeStakeFail
                    )
            updateBakerRestakeEarnings = do
                restake <- arbitrary
                return (
                    toTJSON (UpdateBakerRestakeEarnings restake),
                    m0
                    & mAccounts . ix bkrAcct . _2 +~ 1
                    & changeBakerFail
                    )
            updateBakerKeys = do
                let (fbkr, electionSecretKey, signKey, aggregationKey) = mkFullBaker (m0 ^. mNextSeed) 0
                    bkr = fbkr ^. bakerInfo
                return (toTJSON UpdateBakerKeys{
                        bElectionVerifyKey = bkr ^. bakerElectionVerifyKey,
                        bElectionSecretKey = electionSecretKey,
                        bSignVerifyKey = bkr ^. bakerSignatureVerifyKey,
                        bSignSecretKey = signKey,
                        bAggregateVerifyKey = bkr ^. bakerAggregationVerifyKey,
                        bAggregateSecretKey = aggregationKey
                        },
                        m0
                        & mAccounts . ix bkrAcct . _2 +~ 1
                        & mNextSeed +~ 1
                        & changeBakerFail)
            addBaker = do
                let (fbkr, electionSecretKey, signKey, aggregationKey) = mkFullBaker (m0 ^. mNextSeed) 0
                    bkr = fbkr ^. bakerInfo
                initStake <- elements [0,1,10,100,1000,10000,100000,1000000]
                restake <- arbitrary
                let updateBakerState NoBaker = Baker initStake
                    updateBakerState s = s
                return (toTJSON AddBaker{
                        bElectionVerifyKey = bkr ^. bakerElectionVerifyKey,
                        bElectionSecretKey = electionSecretKey,
                        bSignVerifyKey = bkr ^. bakerSignatureVerifyKey,
                        bSignSecretKey = signKey,
                        bAggregateVerifyKey = bkr ^. bakerAggregationVerifyKey,
                        bAggregateSecretKey = aggregationKey,
                        bInitialStake = initStake,
                        bRestakeEarnings = restake
                        },
                        m0
                        & mAccounts . ix bkrAcct %~ ((_2 +~ 1) . (_3 %~ updateBakerState))
                        & mNextSeed +~ 1
                        & case currentlyBaker of
                            NoBaker -> id
                            _ -> mRejects %~ ((bkrAcct, nonce) :)
                        )
        oneof [removeBaker,updateBakerStake,updateBakerRestakeEarnings,updateBakerKeys,addBaker]

simpleTransfer :: Model -> Gen (TransactionJSON, Model)
simpleTransfer m0 = do
        (srcAcct, (srcKp, srcN, _)) <- elements (Map.toList $ _mAccounts m0)
        destAcct <- elements $ Map.keys $ _mAccounts m0
        amt <- fromIntegral <$> choose (0, 1000 :: Word)
        return (TJSON {
            payload = Transfer {toaddress = destAcct, amount = amt},
            metadata = makeDummyHeader srcAcct srcN energy,
            keys = [(0, srcKp)]
        }, m0 & mAccounts . ix srcAcct . _2 %~ (+1))

-- |Generate the transactions and a lits of those that should be rejected.
makeTransactions :: Gen ([TransactionJSON], [(AccountAddress, Nonce)])
makeTransactions = sized (mt initialModel)
    where
        mt m sz
            | sz > 0 = do
                (t, m') <- frequency [(1, simpleTransfer m), (4, updateBaker m)]
                (_1 %~ (t :)) <$> mt m' (sz - 1)
            | otherwise = return ([], reverse (m ^. mRejects))

testTransactions :: Property
testTransactions = forAll makeTransactions (ioProperty . tt)
    where
        tt (tl, predRejects) = do
            transactions <- processUngroupedTransactions tl
            let (Sch.FilteredTransactions{..}, finState) =
                  EI.runSI
                    (Sch.filterTransactions dummyBlockSize transactions)
                    dummyChainMeta
                    maxBound
                    initialBlockState
            let gs = finState ^. EI.ssBlockState
            let rejs = [(z, decodePayload (thPayloadSize . atrHeader $ z) (atrPayload z), rr) | (WithMetadata{wmdData=NormalTransaction z}, TxReject rr) <- getResults ftAdded]
            let checkRejects [] [] = return ()
                checkRejects [] _ = Left "Expected additional rejected transactions"
                checkRejects rs [] = Left $ "Unexpected rejected transactions:" ++ show rs
                checkRejects (r@(r1,_,_):rs) ((pa, pn):prs)
                    | Concordium.Scheduler.Types.thSender (atrHeader r1) == pa
                    , Concordium.Scheduler.Types.thNonce (atrHeader r1) == pn = checkRejects rs prs
                    | otherwise = Left $ "Unexpected rejected transaction:" ++ show r
            let checkResults = do
                    invariantBlockState gs
                    unless (null ftFailed) $ Left $ "some transactions failed: " ++ show ftFailed
                    checkRejects rejs predRejects
            case checkResults of
                Left f -> return $ counterexample f False
                Right _ -> return $ property True

tests :: Spec
tests = describe "SchedulerTests.RandomBakerTransactions" $
    it "Random baker transactions" $ withMaxSuccess 100 testTransactions
