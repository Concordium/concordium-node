{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | This module defines tests for 'Concordium.Scheduler.ProtocolLevelTokens.Module', which
--  implements the token module. These tests are implemented by modelling interactions between
--  the Token Module and the Token Kernel by traces. A trace specifies a call by the Module into
--  the Kernel with particular arguments, and defines the response from the Kernel (which can
--  be a return value or aborting the execution).
module SchedulerTests.TokenModule where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import Data.Typeable
import Data.Word
import Test.HUnit
import Test.Hspec

import Concordium.Types

import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens
import Concordium.ID.Types (randomAccountAddress)
import Concordium.Scheduler.ProtocolLevelTokens.Kernel
import Concordium.Scheduler.ProtocolLevelTokens.Module (
    InitializeTokenError (..),
    QueryTokenError (..),
    executeTokenHolderTransaction,
    initializeToken,
    queryTokenModuleState,
 )
import Concordium.Types.ProtocolLevelTokens.CBOR
import Concordium.Types.Tokens
import qualified Data.Sequence as Seq
import System.Random

-- | A value of type @PLTKernelQueryCall acct ret@ represents an invocation of an operation
--  in a 'PLTKernelQuery' monad where @PLTAccount m ~ acct@. The parameter @ret@ is the type
--  of the return value expected.
data PLTKernelQueryCall acct ret where
    GetTokenState :: TokenStateKey -> PLTKernelQueryCall acct (Maybe TokenStateValue)
    GetAccount :: AccountAddress -> PLTKernelQueryCall acct (Maybe acct)
    GetAccountBalance :: acct -> PLTKernelQueryCall acct TokenRawAmount
    GetAccountState :: acct -> TokenStateKey -> PLTKernelQueryCall acct (Maybe TokenStateValue)
    GetAccountCanonicalAddress :: acct -> PLTKernelQueryCall acct AccountAddress
    GetGovernanceAccount :: PLTKernelQueryCall acct acct
    GetCirculatingSupply :: PLTKernelQueryCall acct TokenRawAmount
    GetDecimals :: PLTKernelQueryCall acct Word8

deriving instance (Show acct) => Show (PLTKernelQueryCall acct ret)
deriving instance (Eq acct) => Eq (PLTKernelQueryCall acct ret)

-- | A value of type @PLTKernelUpdateCall acct ret@ represents an invocation of an operation
--  in a 'PLTKernelUpdate' monad, where @PLTAccount m ~ acct@. The parameter @ret@ is the type
--  of the return value expected.
data PLTKernelUpdateCall acct ret where
    SetTokenState :: TokenStateKey -> Maybe TokenStateValue -> PLTKernelUpdateCall acct ()
    SetAccountState :: acct -> TokenStateKey -> Maybe TokenStateValue -> PLTKernelUpdateCall acct ()
    Transfer :: acct -> acct -> TokenRawAmount -> Maybe Memo -> PLTKernelUpdateCall acct Bool

deriving instance (Show acct) => Show (PLTKernelUpdateCall acct ret)
deriving instance (Eq acct) => Eq (PLTKernelUpdateCall acct ret)

-- | A value of type @PLTKernelPrivilegedUpdateCall acct ret@ represents an invocation of an
--  operation in a 'PLTKernelPrivilegedUpdate' monad, where @PLTAccount m ~ acct@. The parameter
--  @ret@ is the type of the return value expected.
data PLTKernelPrivilegedUpdateCall acct ret where
    Mint :: acct -> TokenRawAmount -> PLTKernelPrivilegedUpdateCall acct Bool
    Burn :: acct -> TokenRawAmount -> PLTKernelPrivilegedUpdateCall acct Bool

deriving instance (Show acct) => Show (PLTKernelPrivilegedUpdateCall acct ret)
deriving instance (Eq acct) => Eq (PLTKernelPrivilegedUpdateCall acct ret)

-- | A value of type @PLTKernelFailCall e ret@ represents an invocation of an operation in a
--  @PLTKernelFail e@ monad. The parameter @ret@ is the type of the return value expected.
data PLTKernelFailCall e ret where
    PLTError :: e -> PLTKernelFailCall e a

deriving instance (Show e) => Show (PLTKernelFailCall e ret)
deriving instance (Eq e) => Eq (PLTKernelFailCall e ret)

-- | A union type that combines the calls for all of the PLT-related monads.
data PLTCall e acct ret
    = -- | A 'PLTKernelQuery' call
      PLTQ (PLTKernelQueryCall acct ret)
    | -- | A 'PLTKernelUpdate' call
      PLTU (PLTKernelUpdateCall acct ret)
    | -- | A 'PLTKernelPrivilegedUpdate' call
      PLTPU (PLTKernelPrivilegedUpdateCall acct ret)
    | -- | A 'PLTKernelFail' call.
      PLTF (PLTKernelFailCall e ret)

deriving instance (Show e, Show acct) => Show (PLTCall e acct ret)
deriving instance (Eq e, Eq acct) => Eq (PLTCall e acct ret)

-- | A @TraceEvent call@ is a pair of a call of type @call ret@ and a return value of type @ret@.
--  This models a call of a (monadic) operation that returns a specific value.
--  The return type @ret@ is existentially quantified. The constraints ensure that values can be
--  shown and we can discriminate on the types.
data TraceEvent call
    = forall ret.
        (Show (call ret), Show ret, Typeable call, Typeable ret) =>
      call ret :-> ret

infix 8 :->

deriving instance (Show (TraceEvent call))

-- | An @AbortEvent call@ is a call of type @call ret@ with no return value. This models a call of
--  a (monadic) operation where the implementation aborts execution.
--  The return type @ret@ is existentially quantified. The constraints ensure that values can be
--  shown and we can discriminate on the types.
data AbortEvent call = forall ret. (Show (call ret), Typeable call, Typeable ret) => AbortCall (call ret)

deriving instance Show (AbortEvent call)

-- | A trace consisting of a sequence of call-return interactions between a function and the
--  mocked environment and ending in either a call for which the environment aborts execution, or
--  the function returning a value.
data Trace call ret
    = -- | A trace event followed by the rest of the trace
      TraceEvent call :>>: Trace call ret
    | -- | A call that ends the trace by aborting execution.
      Abort (AbortEvent call)
    | -- | The trace exits normally producing a return value.
      Done ret
    deriving (Show, Functor)

infixr 4 :>>:

type TraceException = String

-- | The @TraceM call res ret@ monad is a continuation-based monad that (partially) consumes an
--  event trace of type @Trace call ret@. A value is a function that is parametrised by:
--
--   1. The continuation to invoke on an exception. This is used to indicate that the code under
--      test did not conform to the specified trace.
--
--   2. The continuation in the event of an abort by the environment. This is used to indicate that
--      the environment terminated execution, and does not indicate any fault in the code under
--      test.
--
--   3. The continuation in the event of normal execution. This takes the updated trace and the
--      return value.
--
--   4. The current event trace.
newtype TraceM call res ret a = TraceM
    { runTraceM ::
        (TraceException -> res) ->
        res ->
        (Trace call ret -> a -> res) ->
        Trace call ret ->
        res
    }
    deriving (Functor)

instance Applicative (TraceM call res ret) where
    pure r = TraceM $ \_ _ cont t -> cont t r
    m1 <*> m2 =
        TraceM $ \except abort cont t ->
            runTraceM
                m1
                except
                abort
                (\t1 x1 -> runTraceM m2 except abort (\t2 x2 -> cont t2 (x1 x2)) t1)
                t

instance Monad (TraceM call res ret) where
    m1 >>= m2 = TraceM $ \except abort cont t ->
        runTraceM m1 except abort (\t1 x1 -> runTraceM (m2 x1) except abort cont t1) t

instance MonadFail (TraceM call res ret) where
    fail e = TraceM $ \except _ _ _ -> except e

-- | Check that a trace is consistent with an operation. The result is @Left e@ if the trace is
--  not consistent with the operation, where @e@ describes the inconsistency. Otherwise, @Right ()@
--  indicates the trace is consistent with the operation.
checkTrace :: (Eq ret, Show ret) => Trace call ret -> TraceM call (Either TraceException ()) ret ret -> Either TraceException ()
checkTrace expected op = runTraceM op Left (Right ()) checkDone expected
  where
    checkDone (Done r) r'
        | r == r' = Right ()
        | otherwise = Left $ "Trace produced incorrect result. Expected: " ++ show r' ++ " Actual: " ++ show r
    checkDone t r = Left $ "Trace terminated prematurely (with result " ++ show r ++ "). Expected continuation: " ++ show t

-- | Handle a single event, ensuring that it matches the next event in the trace.
--
--  - If it does not match, or no further events are expected, an exception is raised indicating
--    what was expected and what occurred. (Implemented by calling the exception continuation with
--    the description of the exception.)
--  - If it matches and the event is a 'TraceEvent', then the return value specified by the event
--    is returned. (Implemented by calling the success continuation with the updated trace (after
--    removing the first event) and the specified return value.)
--  - If it matches and the event is an 'AbortEvent', then the execution aborts. (Implemented by
--    calling the abort continuation.)
--
--  Note that we use type-safe casting (courtesy of 'Typeable') since the event result types are
--  existentially quantified in the trace. This is necessary, since we wish to model operations with
--  different return types.
handleEvent ::
    (Eq (call a), Show (call a), Typeable call, Typeable a, Show ret) =>
    call a ->
    TraceM call res ret a
handleEvent actualCall = TraceM $ \except abort cont -> \case
    (expectedCall :-> result :>>: t) -> case cast (expectedCall, result) of
        Just (expectedCall', result') | expectedCall' == actualCall -> cont t result'
        _ -> except $ "Expected trace call " ++ show expectedCall ++ " but saw " ++ show actualCall
    (Abort (AbortCall expectedCall)) -> case cast expectedCall of
        Just expectedCall' | expectedCall' == actualCall -> abort
        _ -> except $ "Expected trace call " ++ show expectedCall ++ " but saw " ++ show actualCall
    (Done r) -> except $ "Trace should end (with result" ++ show r ++ "). But saw event: " ++ show actualCall

instance
    (Eq e, Eq acct, Show e, Show acct, Show ret, Typeable e, Typeable acct, Typeable ret) =>
    PLTKernelQuery (TraceM (PLTCall e acct) res ret)
    where
    type PLTAccount (TraceM (PLTCall e acct) res ret) = acct
    getTokenState key = handleEvent $ PLTQ $ GetTokenState key
    getAccount addr = handleEvent $ PLTQ $ GetAccount addr
    getAccountBalance acct = handleEvent $ PLTQ $ GetAccountBalance acct
    getAccountState acct key = handleEvent $ PLTQ $ GetAccountState acct key
    getAccountCanonicalAddress acct = handleEvent $ PLTQ $ GetAccountCanonicalAddress acct
    getGovernanceAccount = handleEvent $ PLTQ GetGovernanceAccount
    getCirculatingSupply = handleEvent $ PLTQ GetCirculatingSupply
    getDecimals = handleEvent $ PLTQ GetDecimals

instance
    (Eq e, Eq acct, Show e, Show acct, Show ret, Typeable e, Typeable acct, Typeable ret) =>
    PLTKernelUpdate (TraceM (PLTCall e acct) res ret)
    where
    setTokenState key mValue = handleEvent $ PLTU $ SetTokenState key mValue
    setAccountState acct key mValue = handleEvent $ PLTU $ SetAccountState acct key mValue
    transfer sender receiver amount mMemo = handleEvent $ PLTU $ Transfer sender receiver amount mMemo

instance
    (Eq e, Eq acct, Show e, Show acct, Show ret, Typeable e, Typeable acct, Typeable ret) =>
    PLTKernelPrivilegedUpdate (TraceM (PLTCall e acct) res ret)
    where
    mint acct amount = handleEvent $ PLTPU $ Mint acct amount
    burn acct amount = handleEvent $ PLTPU $ Burn acct amount

instance
    (Eq e, Eq acct, Show e, Show acct, Show ret, Typeable e, Typeable acct, Typeable ret) =>
    PLTKernelFail e (TraceM (PLTCall e acct) res ret)
    where
    pltError e = TraceM $ \except abort _ -> \case
        (expectedCall :-> _ :>>: _) ->
            except $ "Expected trace call " ++ show expectedCall ++ " but saw " ++ show actualCall
        (Abort (AbortCall expectedCall)) -> case cast expectedCall of
            Just expectedCall' | expectedCall' == actualCall -> abort
            _ -> except $ "Expected trace call " ++ show expectedCall ++ " but saw " ++ show actualCall
        (Done r) ->
            except $ "Trace should end (with result" ++ show r ++ "). But saw event: " ++ show actualCall
      where
        actualCall :: PLTCall e acct ret
        actualCall = PLTF $ PLTError e

-- | Assert that calling a given operation results in the given trace.
assertTrace :: (Eq ret, Show ret) => TraceM call (Either TraceException ()) ret ret -> Trace call ret -> IO ()
assertTrace op trace = case checkTrace trace op of
    Left e -> assertFailure e
    Right () -> return ()

-- | Helper to generate a trace that aborts as a result of a 'PLTError' call.
abortPLTError :: forall e acct ret. (Show e, Show acct, Typeable e, Typeable acct, Typeable ret) => e -> Trace (PLTCall e acct) ret
abortPLTError = Abort . AbortCall @_ @ret . PLTF . PLTError

-- | Tests for 'initializeToken'.
testInitializeToken :: Spec
testInitializeToken = describe "initializeToken" $ do
    -- In this example, the parameters are not a valid encoding.
    it "invalid parameters" $ do
        let trace :: Trace (PLTCall InitializeTokenError AccountIndex) ()
            trace =
                abortPLTError $
                    ITEDeserializationFailure "DeserialiseFailure 0 \"end of input\""
        assertTrace
            (initializeToken (TokenParameter mempty))
            trace
    -- An example with valid parameters (no minting).
    it "valid1" $ do
        let params =
                TokenInitializationParameters
                    { tipName = "Protocol-level token",
                      tipMetadata = "https://plt.token",
                      tipAllowList = True,
                      tipDenyList = False,
                      tipInitialSupply = Nothing,
                      tipMintable = True,
                      tipBurnable = True
                    }
            tokenParam = TokenParameter $ SBS.toShort $ tokenInitializationParametersToBytes params
            trace :: Trace (PLTCall InitializeTokenError AccountIndex) ()
            trace =
                (PLTU (SetTokenState "name" $ Just "Protocol-level token") :-> ())
                    :>>: (PLTU (SetTokenState "metadata" $ Just "https://plt.token") :-> ())
                    :>>: (PLTU (SetTokenState "allowList" $ Just "") :-> ())
                    :>>: (PLTU (SetTokenState "mintable" $ Just "") :-> ())
                    :>>: (PLTU (SetTokenState "burnable" $ Just "") :-> ())
                    :>>: Done ()
        assertTrace (initializeToken tokenParam) trace
    -- An example with valid parameters and minting.
    it "valid2" $ do
        let params =
                TokenInitializationParameters
                    { tipName = "Protocol-level token2",
                      tipMetadata = "https://plt2.token",
                      tipAllowList = False,
                      tipDenyList = True,
                      tipInitialSupply = Just TokenAmount{digits = 500000, nrDecimals = 2},
                      tipMintable = False,
                      tipBurnable = False
                    }
            tokenParam = TokenParameter $ SBS.toShort $ tokenInitializationParametersToBytes params
            trace :: Trace (PLTCall InitializeTokenError AccountIndex) ()
            trace =
                (PLTU (SetTokenState "name" $ Just "Protocol-level token2") :-> ())
                    :>>: (PLTU (SetTokenState "metadata" $ Just "https://plt2.token") :-> ())
                    :>>: (PLTU (SetTokenState "denyList" $ Just "") :-> ())
                    :>>: (PLTQ GetDecimals :-> 6)
                    :>>: (PLTQ GetGovernanceAccount :-> AccountIndex 50)
                    :>>: (PLTPU (Mint (AccountIndex 50) (TokenRawAmount 5000000000)) :-> True)
                    :>>: Done ()
        assertTrace (initializeToken tokenParam) trace
    -- In this test, the amount to mint exceeds what is representable in the 'TokenRawAmount',
    -- so it should fail.
    it "mint too large" $ do
        let params =
                TokenInitializationParameters
                    { tipName = "Protocol-level token2",
                      tipMetadata = "https://plt2.token",
                      tipAllowList = False,
                      tipDenyList = False,
                      tipInitialSupply = Just TokenAmount{digits = 500000, nrDecimals = 2},
                      tipMintable = False,
                      tipBurnable = False
                    }
            tokenParam = TokenParameter $ SBS.toShort $ tokenInitializationParametersToBytes params
            trace :: Trace (PLTCall InitializeTokenError AccountIndex) ()
            trace =
                (PLTU (SetTokenState "name" $ Just "Protocol-level token2") :-> ())
                    :>>: (PLTU (SetTokenState "metadata" $ Just "https://plt2.token") :-> ())
                    :>>: (PLTQ GetDecimals :-> 50)
                    :>>: abortPLTError (ITEInvalidMintAmount "Token amount exceeds maximum representable amount")
        assertTrace (initializeToken tokenParam) trace
    -- In this test, the Kernel responds to the minting request indicating that it failed.
    it "mint fails" $ do
        let params =
                TokenInitializationParameters
                    { tipName = "Protocol-level token2",
                      tipMetadata = "https://plt2.token",
                      tipAllowList = False,
                      tipDenyList = False,
                      tipInitialSupply = Just TokenAmount{digits = 500000, nrDecimals = 2},
                      tipMintable = False,
                      tipBurnable = False
                    }
            tokenParam = TokenParameter $ SBS.toShort $ tokenInitializationParametersToBytes params
            trace :: Trace (PLTCall InitializeTokenError AccountIndex) ()
            trace =
                (PLTU (SetTokenState "name" $ Just "Protocol-level token2") :-> ())
                    :>>: (PLTU (SetTokenState "metadata" $ Just "https://plt2.token") :-> ())
                    :>>: (PLTQ GetDecimals :-> 2)
                    :>>: (PLTQ GetGovernanceAccount :-> AccountIndex 2)
                    :>>: (PLTPU (Mint (AccountIndex 2) (TokenRawAmount 500000)) :-> False)
                    :>>: abortPLTError (ITEInvalidMintAmount "Kernel failed to mint")
        assertTrace (initializeToken tokenParam) trace
    -- In this example, the parameters specify an initial supply with higher precision than the
    -- token allows. (nrDecimals is 6, but GetDecimals returns 2.)
    it "too many decimals specified" $ do
        let params =
                TokenInitializationParameters
                    { tipName = "Protocol-level token2",
                      tipMetadata = "https://plt2.token",
                      tipAllowList = False,
                      tipDenyList = False,
                      tipInitialSupply = Just TokenAmount{digits = 500000, nrDecimals = 6},
                      tipMintable = False,
                      tipBurnable = False
                    }
            tokenParam = TokenParameter $ SBS.toShort $ tokenInitializationParametersToBytes params
            trace :: Trace (PLTCall InitializeTokenError AccountIndex) ()
            trace =
                (PLTU (SetTokenState "name" $ Just "Protocol-level token2") :-> ())
                    :>>: (PLTU (SetTokenState "metadata" $ Just "https://plt2.token") :-> ())
                    :>>: (PLTQ GetDecimals :-> 2)
                    :>>: abortPLTError (ITEInvalidMintAmount "Token amount precision exceeds representable precision")
        assertTrace (initializeToken tokenParam) trace

dummyAccountAddress :: Int -> AccountAddress
dummyAccountAddress seed = fst $ randomAccountAddress (mkStdGen seed)

testExecuteTokenHolderTransaction :: Spec
testExecuteTokenHolderTransaction = describe "executeTokenHolderTransaction" $ do
    it "invalid transaction" $ do
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                abortPLTError . encodeTokenHolderFailure $
                    DeserializationFailure (Just "DeserialiseFailure 0 \"end of input\"")
        assertTrace
            (executeTokenHolderTransaction 0 (TokenParameter mempty))
            trace
    it "empty operations" $ do
        let transaction = TokenHolderTransaction Seq.empty
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 2)
                    :>>: Done ()
        assertTrace (executeTokenHolderTransaction 0 (encodeTransaction transaction)) trace
    it "transfer OK" $ do
        let transaction =
                TokenHolderTransaction . Seq.fromList $
                    [mkTransferOp amt10'000 receiver1 Nothing]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                    :>>: (PLTU (Transfer 0 4 10_000_000 Nothing) :-> True)
                    :>>: Done ()
        assertTrace (executeTokenHolderTransaction 0 (encodeTransaction transaction)) trace
    it "transfer OK: long memo, max amount" $ do
        let transaction =
                TokenHolderTransaction . Seq.fromList $
                    [mkTransferOp amtMax receiver2 (Just (UntaggedMemo longMemo))]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 0)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 2)) :-> Just 4)
                    :>>: (PLTU (Transfer 0 4 maxBound (Just longMemo)) :-> True)
                    :>>: Done ()
        assertTrace (executeTokenHolderTransaction 0 (encodeTransaction transaction)) trace
    it "invalid memo" $ do
        let transaction =
                TokenHolderTransaction . Seq.fromList $
                    [mkTransferOp amtMax receiver2 (Just (UntaggedMemo badMemo))]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                abortPLTError . encodeTokenHolderFailure $
                    DeserializationFailure (Just "DeserialiseFailure 277 \"Size of the memo (257 bytes) exceeds maximum allowed size (256 bytes).\"")
        assertTrace (executeTokenHolderTransaction 0 (encodeTransaction transaction)) trace
    -- In this test, although the amount deserializes successfully, it is too large because of
    -- the number of decimals in the token representation.
    it "amount too large" $ do
        let transaction =
                TokenHolderTransaction . Seq.fromList $
                    [mkTransferOp amtMax receiver2 (Just (UntaggedMemo longMemo))]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 2)
                    :>>: ( abortPLTError . encodeTokenHolderFailure $
                            DeserializationFailure (Just "Token amount outside representable range: Token amount exceeds maximum representable amount")
                         )
        assertTrace (executeTokenHolderTransaction 0 (encodeTransaction transaction)) trace
    it "two transfers" $ do
        let transaction =
                TokenHolderTransaction . Seq.fromList $
                    [ mkTransferOp amt10'000 receiver1 (Just (CBORMemo cborMemo)),
                      mkTransferOp amt50 receiver2 (Just (UntaggedMemo simpleMemo))
                    ]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                    :>>: (PLTU (Transfer 0 4 10_000_000 (Just cborMemo)) :-> True)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 2)) :-> Just 121)
                    :>>: (PLTU (Transfer 0 121 50_000_000 (Just simpleMemo)) :-> True)
                    :>>: Done ()
        assertTrace (executeTokenHolderTransaction 0 (encodeTransaction transaction)) trace
    it "two transfers - first fails (insufficient funds)" $ do
        let transaction =
                TokenHolderTransaction . Seq.fromList $
                    [ mkTransferOp amt10'000 receiver1 (Just (CBORMemo cborMemo)),
                      mkTransferOp amt50 receiver2 (Just (UntaggedMemo simpleMemo))
                    ]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                    :>>: (PLTU (Transfer 0 4 10_000_000 (Just cborMemo)) :-> False)
                    :>>: (PLTQ (GetAccountBalance 0) :-> 0)
                    :>>: ( abortPLTError . encodeTokenHolderFailure $
                            TokenBalanceInsufficient
                                { thfOperationIndex = 0,
                                  thfAvailableBalance = TokenAmount 0 6,
                                  thfRequiredBalance = amt10'000
                                }
                         )
        assertTrace (executeTokenHolderTransaction 0 (encodeTransaction transaction)) trace
    it "two transfers - second fails (insufficient funds)" $ do
        let transaction =
                TokenHolderTransaction . Seq.fromList $
                    [ mkTransferOp amt10'000 receiver1 (Just (CBORMemo cborMemo)),
                      mkTransferOp amt50 receiver2 (Just (UntaggedMemo simpleMemo))
                    ]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                    :>>: (PLTU (Transfer 0 4 10_000_000 (Just cborMemo)) :-> True)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 2)) :-> Just 16)
                    :>>: (PLTU (Transfer 0 16 50_000_000 (Just simpleMemo)) :-> False)
                    :>>: (PLTQ (GetAccountBalance 0) :-> 5_000_000)
                    :>>: ( abortPLTError . encodeTokenHolderFailure $
                            TokenBalanceInsufficient
                                { thfOperationIndex = 1,
                                  thfAvailableBalance = TokenAmount 5_000_000 6,
                                  thfRequiredBalance = amt50
                                }
                         )
        assertTrace (executeTokenHolderTransaction 0 (encodeTransaction transaction)) trace
    it "two transfers - second fails (invalid recipient)" $ do
        let transaction =
                TokenHolderTransaction . Seq.fromList $
                    [ mkTransferOp amt10'000 receiver1 (Just (CBORMemo cborMemo)),
                      mkTransferOp amt50 receiver2 (Just (UntaggedMemo simpleMemo))
                    ]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                    :>>: (PLTU (Transfer 0 4 10_000_000 (Just cborMemo)) :-> True)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 2)) :-> Nothing)
                    :>>: ( abortPLTError . encodeTokenHolderFailure $
                            RecipientNotFound
                                { thfOperationIndex = 1,
                                  thfRecipient = receiver2
                                }
                         )
        assertTrace (executeTokenHolderTransaction 0 (encodeTransaction transaction)) trace
    it "5000 transfers" $ do
        let transaction =
                TokenHolderTransaction . Seq.fromList $
                    [ mkTransferOp
                        amt10'000
                        (ReceiverAccount (dummyAccountAddress i) Nothing)
                        Nothing
                      | i <- [1 .. 5000]
                    ]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace = (PLTQ GetDecimals :-> 3) :>>: traceLoop 1
            traceLoop n
                | n > 5000 = Done ()
                | otherwise =
                    (PLTQ (GetAccount (dummyAccountAddress (fromIntegral n))) :-> Just n)
                        :>>: (PLTU (Transfer 123_456 n 10_000 Nothing) :-> True)
                        :>>: traceLoop (n + 1)
        assertTrace (executeTokenHolderTransaction 123_456 (encodeTransaction transaction)) trace
  where
    receiver1 = ReceiverAccount (dummyAccountAddress 1) Nothing
    receiver2 = ReceiverAccount (dummyAccountAddress 2) (Just CoinInfoConcordium)
    amt10'000 = TokenAmount 10_000 3
    amtMax = TokenAmount maxBound 0
    amt50 = TokenAmount 50 0
    simpleMemo = Memo "Test"
    cborMemo = Memo "dTest"
    longMemo = Memo $ SBS.replicate maxMemoSize 60
    badMemo = Memo $ SBS.replicate (maxMemoSize + 1) 60
    mkTransferOp ttAmount ttRecipient ttMemo = TokenHolderTransfer TokenTransferBody{..}
    encodeTransaction = TokenParameter . SBS.toShort . tokenHolderTransactionToBytes

testQueryTokenModuleState :: Spec
testQueryTokenModuleState = describe "queryTokenModuleState" $ do
    it "Example 1" $ do
        let trace :: Trace (PLTCall QueryTokenError AccountIndex) BS.ByteString
            trace =
                (PLTQ (GetTokenState "name") :-> Just "My protocol-level token")
                    :>>: (PLTQ (GetTokenState "metadata") :-> Just "some URL")
                    :>>: (PLTQ (GetTokenState "allowList") :-> Just "")
                    :>>: (PLTQ (GetTokenState "denyList") :-> Nothing)
                    :>>: (PLTQ (GetTokenState "mintable") :-> Just "")
                    :>>: (PLTQ (GetTokenState "burnable") :-> Nothing)
                    :>>: Done
                        ( tokenModuleStateToBytes
                            TokenModuleState
                                { tmsName = "My protocol-level token",
                                  tmsMetadata = "some URL",
                                  tmsAllowList = Just True,
                                  tmsDenyList = Just False,
                                  tmsMintable = Just True,
                                  tmsBurnable = Just False,
                                  tmsAdditional = mempty
                                }
                        )
        assertTrace queryTokenModuleState trace
    it "Example 2" $ do
        let trace :: Trace (PLTCall QueryTokenError AccountIndex) BS.ByteString
            trace =
                (PLTQ (GetTokenState "name") :-> Just "Another PLT")
                    :>>: (PLTQ (GetTokenState "metadata") :-> Just "https://token.metadata")
                    :>>: (PLTQ (GetTokenState "allowList") :-> Nothing)
                    :>>: (PLTQ (GetTokenState "denyList") :-> Just "")
                    :>>: (PLTQ (GetTokenState "mintable") :-> Nothing)
                    :>>: (PLTQ (GetTokenState "burnable") :-> Nothing)
                    :>>: Done
                        ( tokenModuleStateToBytes
                            TokenModuleState
                                { tmsName = "Another PLT",
                                  tmsMetadata = "https://token.metadata",
                                  tmsAllowList = Just False,
                                  tmsDenyList = Just True,
                                  tmsMintable = Just False,
                                  tmsBurnable = Just False,
                                  tmsAdditional = mempty
                                }
                        )
        assertTrace queryTokenModuleState trace

-- | Tests for the Token Module implementation.
tests :: Spec
tests = parallel $ describe "TokenModule" $ do
    testInitializeToken
    testExecuteTokenHolderTransaction
    testQueryTokenModuleState
