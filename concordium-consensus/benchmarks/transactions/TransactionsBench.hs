{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Timing of various types of transaction
-- The purpose of the benchmark is primarily to determine the relative energy costs
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
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as BSS
import qualified Data.Sequence as Seq
import qualified SchedulerBench.Helpers as Helpers

initialBlockState ::
    Helpers.PersistentBSM 'Types.P9 (BS.HashedPersistentBlockState 'Types.P9)
initialBlockState =
    Helpers.createTestBlockStateWithAccountsM
        [ Helpers.makeTestAccountFromSeed 1_000_000_000 0,
          Helpers.makeTestAccountFromSeed 1_000_000 1
        ]

assertApplied :: Bool -> Int -> Helpers.SchedulerResult -> BS.PersistentBlockState pv -> Helpers.PersistentBSM pv ()
assertApplied assertSuccess txnCount result _state = do
    let results = Helpers.getResults $ ftAdded (Helpers.srTransactions result)
    if length results /= txnCount
        then error ("expected " ++ show txnCount ++ " results, was " ++ show (length results))
        else
            seq
                ( foldl'
                    ( \_ item -> case snd item of
                        Exec.TxReject _ | assertSuccess -> error ("failed transaction " ++ show item)
                        _ -> ()
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

-- | Block item that create a PLT token
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

-- | CCD transfer transaction
transferTxn :: SigScheme.KeyPair -> Nonce -> AccountAddress -> AccountAddress -> Amount -> Runner.TransactionJSON
transferTxn keyPair nonce from to amount =
    Runner.TJSON
        { payload = Runner.Transfer{toaddress = to, ..},
          metadata = makeDummyHeader from nonce Helpers.simpleTransferCost,
          keys = [(0, [(0, keyPair)])]
        }

-- | PLT transaction consisting of given operations
pltTxn :: SigScheme.KeyPair -> Nonce -> TokenId -> AccountAddress -> [CBOR.TokenOperation] -> Runner.TransactionJSON
pltTxn keyPair nonce tokenId from operations =
    pltTxnFromParam
        keyPair
        nonce
        (Helpers.simpleTransferCost * 5 * fromIntegral (1 + length operations))
        tokenId
        from
        ( toTokenParam
            TokenUpdateTransaction
                { tokenOperations =
                    Seq.fromList operations
                }
        )
  where
    toTokenParam = Types.TokenParameter . BSS.toShort . CBOR.tokenUpdateTransactionToBytes

pltTxnFromParam :: SigScheme.KeyPair -> Nonce -> Energy -> TokenId -> AccountAddress -> Types.TokenParameter -> Runner.TransactionJSON
pltTxnFromParam keyPair nonce cost tokenId from param =
    Runner.TJSON
        { metadata = makeDummyHeader from nonce cost,
          keys = [(0, [(0, keyPair)])],
          ..
        }
  where
    payload =
        Runner.TokenUpdate
            { tuTokenId = tokenId,
              tuOperations = param
            }

-- | PLT transfer operation
transferPltOp :: AccountAddress -> TokenRawAmount -> CBOR.TokenOperation
transferPltOp to value =
    CBOR.TokenTransfer
        CBOR.TokenTransferBody
            { ttAmount = TokenAmount{taValue = value, taDecimals = 6},
              ttRecipient = CBOR.accountTokenHolder to,
              ttMemo = Nothing
            }

-- | PLT mint operation
mintPltOp :: TokenRawAmount -> CBOR.TokenOperation
mintPltOp value =
    CBOR.TokenMint
        { toMintAmount = TokenAmount{taValue = value, taDecimals = 6}
        }

-- | PLT burn operation
burnPltOp :: TokenRawAmount -> CBOR.TokenOperation
burnPltOp value =
    CBOR.TokenBurn
        { toBurnAmount = TokenAmount{taValue = value, taDecimals = 6}
        }

-- | PLT add allow list operation
addAllowListPltOp :: AccountAddress -> CBOR.TokenOperation
addAllowListPltOp target =
    CBOR.TokenAddAllowList
        { toTarget = CBOR.accountTokenHolder target
        }

-- | PLT remove allow list operation
removeAllowListPltOp :: AccountAddress -> CBOR.TokenOperation
removeAllowListPltOp target =
    CBOR.TokenRemoveAllowList
        { toTarget = CBOR.accountTokenHolder target
        }

-- | PLT add deny list operation
addDenyListPltOp :: AccountAddress -> CBOR.TokenOperation
addDenyListPltOp target =
    CBOR.TokenAddDenyList
        { toTarget = CBOR.accountTokenHolder target
        }

-- | PLT remove deny list operation
removeDenyListPltOp :: AccountAddress -> CBOR.TokenOperation
removeDenyListPltOp target =
    CBOR.TokenRemoveDenyList
        { toTarget = CBOR.accountTokenHolder target
        }

plt1 :: TokenId
plt1 = Types.TokenId "PLT1"

plt2 :: TokenId
plt2 = Types.TokenId "PLT2"

instance NFData (Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)) where
    rnf a = seq ((`seq` ()) <$> a) ()

-- | Number of transactions or operations in each run. Is used to reduce size of the overhead (setup scheduler etc.) of running the
-- operations or transactions compared to actually running them
operationScaleFactor :: Int
operationScaleFactor = 1000

-- | Run benchmark on given transactions
benchTransactionsAssertSuccess :: String -> [Runner.TransactionJSON] -> Benchmark
benchTransactionsAssertSuccess label transactions = benchBlockItemsAssertSuccess label (Runner.AccountTx <$> transactions)

-- | Run benchmark on given block items
benchBlockItemsAssertSuccess :: String -> [Runner.BlockItemDescription] -> Benchmark
benchBlockItemsAssertSuccess label = benchBlockItems label True

-- | Run benchmark on given block items
benchBlockItems :: String -> Bool -> [Runner.BlockItemDescription] -> Benchmark
benchBlockItems label assertSuccess blockItems =
    env (pure initialBlockState) $ \ibs ->
        bench label $
            whnfAppIO
                ( \bis -> do
                    groupedTxns <- Runner.processUngroupedBlockItems bis
                    Helpers.runSchedulerTest
                        Helpers.defaultTestConfig
                        ibs
                        (assertApplied assertSuccess $ length blockItems)
                        groupedTxns
                )
                blockItems

-- | Benchmark `operationScaleFactor` number of CCD transfer transactions
benchTransfer :: Benchmark
benchTransfer = benchTransactionsAssertSuccess "transfer (CCD)" $ transactions operationScaleFactor
  where
    transactions :: Int -> [Runner.TransactionJSON]
    transactions txnCount =
        map
            ( \nonce ->
                transferTxn keyPair0 (fromIntegral nonce) accountAddress0 accountAddress1 1_000
            )
            [1 .. txnCount]

-- | Benchmark `operationScaleFactor` number of PLT transfer operations in a single transaction
benchPltTransfer :: Benchmark
benchPltTransfer =
    benchBlockItemsAssertSuccess
        "PLT transfer"
        [ createPltBlockItem plt1 $ tokenInitializationParameters accountAddress0,
          Runner.AccountTx transaction
        ]
  where
    transaction = pltTxn keyPair0 1 plt1 accountAddress0 (operations operationScaleFactor)
    operations :: Int -> [CBOR.TokenOperation]
    operations txnCount = replicate txnCount $ transferPltOp accountAddress1 1_000

-- | Benchmark `operationScaleFactor` number of PLT mint operations in a single transaction
benchPltMint :: Benchmark
benchPltMint =
    benchBlockItemsAssertSuccess
        "PLT mint"
        [ createPltBlockItem plt1 $ tokenInitializationParameters accountAddress0,
          Runner.AccountTx transaction
        ]
  where
    transaction = pltTxn keyPair0 1 plt1 accountAddress0 (operations operationScaleFactor)
    operations :: Int -> [CBOR.TokenOperation]
    operations txnCount = replicate txnCount $ mintPltOp 1_000

-- | Benchmark `operationScaleFactor` number of PLT burn operations in a single transaction
benchPltBurn :: Benchmark
benchPltBurn =
    benchBlockItemsAssertSuccess
        "PLT burn"
        [ createPltBlockItem plt1 $ tokenInitializationParameters accountAddress0,
          Runner.AccountTx transaction
        ]
  where
    transaction = pltTxn keyPair0 1 plt1 accountAddress0 (operations operationScaleFactor)
    operations :: Int -> [CBOR.TokenOperation]
    operations txnCount = replicate txnCount $ burnPltOp 1_000

-- | Benchmark `operationScaleFactor` total number of PLT add and remove from allow list operations in a single transaction
benchPltAddRemoveAllowList :: Benchmark
benchPltAddRemoveAllowList =
    benchBlockItemsAssertSuccess
        "PLT add/remove allow list"
        [ createPltBlockItem plt1 (tokenInitializationParameters accountAddress0){CBOR.tipAllowList = True},
          Runner.AccountTx transaction
        ]
  where
    transaction = pltTxn keyPair0 1 plt1 accountAddress0 (operations operationScaleFactor)
    operations :: Int -> [CBOR.TokenOperation]
    operations txnCount = take txnCount $ cycle [addAllowListPltOp accountAddress1, removeAllowListPltOp accountAddress1]

-- | Benchmark `operationScaleFactor` total number of PLT add and remove from deny list operations in a single transaction
benchPltAddRemoveDenyList :: Benchmark
benchPltAddRemoveDenyList =
    benchBlockItemsAssertSuccess
        "PLT add/remove deny list"
        [ createPltBlockItem plt1 $ (tokenInitializationParameters accountAddress0){CBOR.tipDenyList = True},
          Runner.AccountTx transaction
        ]
  where
    transaction = pltTxn keyPair0 1 plt1 accountAddress0 (operations operationScaleFactor)
    operations :: Int -> [CBOR.TokenOperation]
    operations txnCount = take txnCount $ cycle [addDenyListPltOp accountAddress1, removeDenyListPltOp accountAddress1]

-- | Benchmark `operationScaleFactor` number of PLT transactions with no operations
benchPltNoOperations :: Benchmark
benchPltNoOperations =
    benchBlockItemsAssertSuccess
        "PLT no operations"
        ( createPltBlockItem plt1 (tokenInitializationParameters accountAddress0)
            : (Runner.AccountTx <$> transactions operationScaleFactor)
        )
  where
    transactions :: Int -> [Runner.TransactionJSON]
    transactions txnCount =
        map
            ( \nonce ->
                pltTxn keyPair0 (fromIntegral nonce) plt1 accountAddress0 []
            )
            [1 .. txnCount]

-- | Benchmark `operationScaleFactor` number of PLT transaction each with a single PLT transfer operation. Benchmark should be equal
-- to sum of PLT transaction with no operations plus PLT transfer operation
benchPltTxnAndTransfer :: Benchmark
benchPltTxnAndTransfer =
    benchBlockItemsAssertSuccess
        "PLT txn + PLT transfer"
        ( createPltBlockItem plt1 (tokenInitializationParameters accountAddress0)
            : (Runner.AccountTx <$> transactions operationScaleFactor)
        )
  where
    transactions :: Int -> [Runner.TransactionJSON]
    transactions txnCount =
        map
            ( \nonce ->
                pltTxn keyPair0 (fromIntegral nonce) plt1 accountAddress0 [operation]
            )
            [1 .. txnCount]
    operation :: CBOR.TokenOperation
    operation = transferPltOp accountAddress1 1_000

-- | Benchmark `operationScaleFactor` number of PLT transactions with invalid CBOR
benchPltTxnCborDecodeError :: Benchmark
benchPltTxnCborDecodeError =
    benchBlockItems
        "PLT cbor decode error"
        False
        ( createPltBlockItem plt1 (tokenInitializationParameters accountAddress0)
            : (Runner.AccountTx <$> transactions operationScaleFactor)
        )
  where
    transactions :: Int -> [Runner.TransactionJSON]
    transactions txnCount =
        map
            ( \nonce ->
                pltTxnFromParam keyPair0 (fromIntegral nonce) (Helpers.simpleTransferCost * 5) plt1 accountAddress0 invalidParam
            )
            [1 .. txnCount]
    invalidParam = Types.TokenParameter $ BSS.toShort $ BS.snoc param 0
    param =
        CBOR.tokenUpdateTransactionToBytes $
            TokenUpdateTransaction
                { tokenOperations =
                    Seq.fromList [operation]
                }
    operation :: CBOR.TokenOperation
    operation = transferPltOp accountAddress1 1_000

-- | Benchmark running no transactions (test framework overhead)
benchNoTxns :: Benchmark
benchNoTxns = benchBlockItemsAssertSuccess "no txns (overhead)" []

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
          benchPltAddRemoveDenyList,
          benchPltTxnAndTransfer,
          benchPltTxnCborDecodeError
        ]
