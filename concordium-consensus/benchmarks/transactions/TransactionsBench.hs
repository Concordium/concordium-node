{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Timing of various types of transaction
-- The purpose of the benchmark is primarilty to determine the relative energy costs
-- of the transactions
module Main where

import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.GlobalState.DummyData as DummyData
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.Scheduler
import Concordium.Scheduler.DummyData
import Concordium.Scheduler.Runner (ChainUpdateTransaction (ctSeqNumber))
import qualified Concordium.Scheduler.Runner as Runner
import qualified Concordium.Scheduler.Types as Types
import Concordium.Types
import qualified Concordium.Types.DummyData as DummyData
import qualified Concordium.Types.Execution as Exec
import Concordium.Types.ProtocolLevelTokens.CBOR (TokenUpdateTransaction (TokenUpdateTransaction))
import qualified Concordium.Types.ProtocolLevelTokens.CBOR as CBOR
import Concordium.Types.Tokens
import Control.DeepSeq
import Criterion
import Criterion.Main
import qualified Data.ByteString.Short as BSS
import Data.Foldable
import qualified Data.Sequence as Seq
import qualified SchedulerTests.Helpers as Helpers

initialBlockState ::
    Helpers.PersistentBSM 'Types.P9 (BS.HashedPersistentBlockState 'Types.P9)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [ Helpers.makeTestAccountFromSeed 1_000_000_000 0,
          Helpers.makeTestAccountFromSeed 1_000_000 1
        ]

assertSuccessResult :: Int -> Helpers.SchedulerResult -> BS.PersistentBlockState pv -> Helpers.PersistentBSM pv ()
assertSuccessResult txnCount result _state = do
    let results = Helpers.getResults $ ftAdded (Helpers.srTransactions result)
    if length results /= txnCount
        then error ("expected " ++ show txnCount ++ " results, was " ++ show (length results))
        else
            seq
                ( foldl'
                    ( \_ item -> case snd item of
                        Exec.TxSuccess _ -> ()
                        Exec.TxReject _ -> error ("failed transaction " ++ show item)
                    )
                    ()
                    results
                )
                (return ())

accountAddress0 :: Types.AccountAddress
accountAddress0 = Helpers.accountAddressFromSeed 0

accountAddress1 :: Types.AccountAddress
accountAddress1 = Helpers.accountAddressFromSeed 1

keyPair0 :: SigScheme.KeyPair
keyPair0 = Helpers.keyPairFromSeed 0

tokenInitializationParameters :: AccountAddress -> CBOR.TokenInitializationParameters
tokenInitializationParameters accountAddress =
    CBOR.TokenInitializationParameters
        { tipName = "Protocol-level token",
          tipMetadata = CBOR.createTokenMetadataUrl "https://plt.token",
          tipAllowList = False,
          tipDenyList = False,
          tipGovernanceAccount = CBOR.accountTokenHolder accountAddress,
          tipInitialSupply =
            Just
                TokenAmount
                    { taValue = 1_000_000_000_000,
                      taDecimals = 6
                    },
          tipMintable = True,
          tipBurnable = True
        }

createPltBlockItem :: TokenId -> CBOR.TokenInitializationParameters -> Runner.BlockItemDescription
createPltBlockItem tokenId initializationParameters =
    Runner.ChainUpdateTx
        Runner.ChainUpdateTransaction
            { ctEffectiveTime = 0,
              ctTimeout = DummyData.dummyMaxTransactionExpiryTime,
              ctKeys = [(0, DummyData.dummyAuthorizationKeyPair)],
              ctPayload = Types.CreatePLTUpdatePayload createPlt,
              ctSeqNumber = 1
            }
  where
    toTokenParam = Types.TokenParameter . BSS.toShort . CBOR.tokenInitializationParametersToBytes
    createPlt =
        Types.CreatePLT
            { _cpltTokenModule = TokenModuleRef dummyHash,
              _cpltTokenId = tokenId,
              _cpltInitializationParameters = toTokenParam initializationParameters,
              _cpltDecimals = 6
            }
    dummyHash = Hash.hashShort BSS.empty

transferTxn :: SigScheme.KeyPair -> Nonce -> AccountAddress -> AccountAddress -> Amount -> Runner.TransactionJSON
transferTxn keyPair nonce from to amount =
    Runner.TJSON
        { payload = Runner.Transfer{toaddress = to, ..},
          metadata = makeDummyHeader from nonce Helpers.simpleTransferCost,
          keys = [(0, [(0, keyPair)])]
        }

pltTxn :: SigScheme.KeyPair -> Nonce -> TokenId -> AccountAddress -> [CBOR.TokenOperation] -> Runner.TransactionJSON
pltTxn keyPair nonce tokenId from operations =
    Runner.TJSON
        { metadata = makeDummyHeader from nonce (Helpers.simpleTransferCost * 5 * fromIntegral (1 + length operations)),
          keys = [(0, [(0, keyPair)])],
          ..
        }
  where
    payload =
        Runner.TokenUpdate
            { tuTokenId = tokenId,
              tuOperations =
                toTokenParam
                    TokenUpdateTransaction
                        { tokenOperations =
                            Seq.fromList operations
                        }
            }
    toTokenParam = Types.TokenParameter . BSS.toShort . CBOR.tokenUpdateTransactionToBytes

transferPltOp :: AccountAddress -> TokenRawAmount -> CBOR.TokenOperation
transferPltOp to value =
    CBOR.TokenTransfer
        CBOR.TokenTransferBody
            { ttAmount = TokenAmount{taValue = value, taDecimals = 6},
              ttRecipient = CBOR.accountTokenHolder to,
              ttMemo = Nothing
            }

mintPltOp :: TokenRawAmount -> CBOR.TokenOperation
mintPltOp value =
    CBOR.TokenMint
        { tgoMintAmount = TokenAmount{taValue = value, taDecimals = 6}
        }

burnPltOp :: TokenRawAmount -> CBOR.TokenOperation
burnPltOp value =
    CBOR.TokenBurn
        { tgoBurnAmount = TokenAmount{taValue = value, taDecimals = 6}
        }

addAllowListPltOp :: AccountAddress -> CBOR.TokenOperation
addAllowListPltOp target =
    CBOR.TokenAddAllowList
        { tgoTarget = CBOR.accountTokenHolder target
        }

removeAllowListPltOp :: AccountAddress -> CBOR.TokenOperation
removeAllowListPltOp target =
    CBOR.TokenRemoveAllowList
        { tgoTarget = CBOR.accountTokenHolder target
        }

addDenyListPltOp :: AccountAddress -> CBOR.TokenOperation
addDenyListPltOp target =
    CBOR.TokenAddDenyList
        { tgoTarget = CBOR.accountTokenHolder target
        }

removeDenyListPltOp :: AccountAddress -> CBOR.TokenOperation
removeDenyListPltOp target =
    CBOR.TokenRemoveDenyList
        { tgoTarget = CBOR.accountTokenHolder target
        }

plt1 :: TokenId
plt1 = Types.TokenId "PLT1"

plt2 :: TokenId
plt2 = Types.TokenId "PLT2"

instance NFData (Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)) where
    rnf a = seq ((`seq` ()) <$> a) ()

-- | Number of transactions or operations in each run. Is used to reduce size of the overhead (setup scheduler etc.) of running the
-- operations or transactions compared to actually running them
transactionCount :: Int
transactionCount = 1000

benchTransactions :: String -> [Runner.TransactionJSON] -> Benchmark
benchTransactions label transactions = benchBlockItems label (Runner.AccountTx <$> transactions)

benchBlockItems :: String -> [Runner.BlockItemDescription] -> Benchmark
benchBlockItems label blockItems =
    env (pure initialBlockState) $ \ibs ->
        bench label $
            whnfAppIO
                ( \bis -> do
                    groupedTxns <- Runner.processUngroupedBlockItems bis
                    Helpers.runSchedulerTest
                        Helpers.defaultTestConfig
                        ibs
                        (assertSuccessResult $ length blockItems)
                        groupedTxns
                )
                blockItems

benchTransfer :: Benchmark
benchTransfer = benchTransactions "transfer (CCD)" $ transactions transactionCount
  where
    transactions :: Int -> [Runner.TransactionJSON]
    transactions txnCount =
        map
            ( \nonce ->
                transferTxn keyPair0 (fromIntegral nonce) accountAddress0 accountAddress1 1_000
            )
            [1 .. txnCount]

benchPltTransfer :: Benchmark
benchPltTransfer =
    benchBlockItems
        "PLT transfer"
        [ createPltBlockItem plt1 $ tokenInitializationParameters accountAddress0,
          Runner.AccountTx transaction
        ]
  where
    transaction = pltTxn keyPair0 1 plt1 accountAddress0 (operations transactionCount)
    operations :: Int -> [CBOR.TokenOperation]
    operations txnCount = replicate txnCount $ transferPltOp accountAddress1 1_000

benchPltMint :: Benchmark
benchPltMint =
    benchBlockItems
        "PLT mint"
        [ createPltBlockItem plt1 $ tokenInitializationParameters accountAddress0,
          Runner.AccountTx transaction
        ]
  where
    transaction = pltTxn keyPair0 1 plt1 accountAddress0 (operations transactionCount)
    operations :: Int -> [CBOR.TokenOperation]
    operations txnCount = replicate txnCount $ mintPltOp 1_000

benchPltBurn :: Benchmark
benchPltBurn =
    benchBlockItems
        "PLT burn"
        [ createPltBlockItem plt1 $ tokenInitializationParameters accountAddress0,
          Runner.AccountTx transaction
        ]
  where
    transaction = pltTxn keyPair0 1 plt1 accountAddress0 (operations transactionCount)
    operations :: Int -> [CBOR.TokenOperation]
    operations txnCount = replicate txnCount $ burnPltOp 1_000

benchPltAddRemoveAllowList :: Benchmark
benchPltAddRemoveAllowList =
    benchBlockItems
        "PLT add/remove allow list"
        [ createPltBlockItem plt1 (tokenInitializationParameters accountAddress0){CBOR.tipAllowList = True},
          Runner.AccountTx transaction
        ]
  where
    transaction = pltTxn keyPair0 1 plt1 accountAddress0 (operations transactionCount)
    operations :: Int -> [CBOR.TokenOperation]
    operations txnCount = take txnCount $ cycle [addAllowListPltOp accountAddress1, removeAllowListPltOp accountAddress1]

benchPltAddRemoveDenyList :: Benchmark
benchPltAddRemoveDenyList =
    benchBlockItems
        "PLT add/remove deny list"
        [ createPltBlockItem plt1 $ (tokenInitializationParameters accountAddress0){CBOR.tipDenyList = True},
          Runner.AccountTx transaction
        ]
  where
    transaction = pltTxn keyPair0 1 plt1 accountAddress0 (operations transactionCount)
    operations :: Int -> [CBOR.TokenOperation]
    operations txnCount = take txnCount $ cycle [addDenyListPltOp accountAddress1, removeDenyListPltOp accountAddress1]

benchPltNoOperations :: Benchmark
benchPltNoOperations =
    benchBlockItems
        "PLT no operations"
        ( createPltBlockItem plt1 (tokenInitializationParameters accountAddress0)
            : (Runner.AccountTx <$> transactions transactionCount)
        )
  where
    transactions :: Int -> [Runner.TransactionJSON]
    transactions txnCount =
        map
            ( \nonce ->
                pltTxn keyPair0 (fromIntegral nonce) plt1 accountAddress0 []
            )
            [1 .. txnCount]

benchNoTxns :: Benchmark
benchNoTxns = benchBlockItems "no txns (overhead)" []

main :: IO ()
main =
    defaultMain
        [ benchTransfer,
          benchPltTransfer,
          benchNoTxns,
          benchPltMint,
          benchPltBurn,
          benchPltNoOperations,
          benchPltAddRemoveAllowList,
          benchPltAddRemoveDenyList
        ]