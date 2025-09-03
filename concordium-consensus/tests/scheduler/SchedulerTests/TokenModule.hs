{-# LANGUAGE DataKinds #-}
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

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import qualified Data.Sequence as Seq
import Data.Serialize (encode)
import Data.String
import qualified Data.Text as Text
import Data.Typeable
import Data.Word
import Lens.Micro.Platform
import System.Random
import Test.HUnit
import Test.Hspec

import qualified Concordium.Crypto.SHA256 as SHA256
import Concordium.ID.Types (randomAccountAddress)
import Concordium.Types
import Concordium.Types.ProtocolLevelTokens.CBOR
import Concordium.Types.Tokens

import Concordium.Cost
import qualified Concordium.Crypto.SHA256 as Hash
import Concordium.GlobalState.BlockState as BlockState
import Concordium.GlobalState.Persistent.BlockState
import qualified Concordium.GlobalState.Persistent.BlockState as BS
import Concordium.GlobalState.Persistent.BlockState.ProtocolLevelTokens
import Concordium.Scheduler.DummyData
import qualified Concordium.Scheduler.EnvironmentImplementation as EI
import Concordium.Scheduler.ProtocolLevelTokens.Kernel
import Concordium.Scheduler.ProtocolLevelTokens.Module (
    InitializeTokenError (..),
    QueryTokenError (..),
    TransactionContext' (..),
    accountStateKey,
    executeTokenUpdateTransaction,
    initializeToken,
    moduleStateKey,
    queryTokenModuleState,
 )
import qualified Concordium.Scheduler.Runner as Runner
import qualified Concordium.Scheduler.Types as Types
import qualified SchedulerTests.Helpers as Helpers

-- | A value of type @PLTKernelQueryCall acct ret@ represents an invocation of an operation
--  in a 'PLTKernelQuery' monad where @PLTAccount m ~ acct@. The parameter @ret@ is the type
--  of the return value expected.
data PLTKernelQueryCall acct ret where
    GetTokenState :: TokenStateKey -> PLTKernelQueryCall acct (Maybe TokenStateValue)
    GetAccount :: AccountAddress -> PLTKernelQueryCall acct (Maybe acct)
    GetAccountBalance :: acct -> PLTKernelQueryCall acct TokenRawAmount
    GetAccountIndex :: acct -> PLTKernelQueryCall acct AccountIndex
    GetAccountByIndex :: AccountIndex -> PLTKernelQueryCall acct (Maybe acct)
    GetAccountCanonicalAddress :: acct -> PLTKernelQueryCall acct AccountAddress
    GetCirculatingSupply :: PLTKernelQueryCall acct TokenRawAmount
    GetDecimals :: PLTKernelQueryCall acct Word8

deriving instance (Show acct) => Show (PLTKernelQueryCall acct ret)
deriving instance (Eq acct) => Eq (PLTKernelQueryCall acct ret)

-- | Construct a @PLTKernelQueryCall@ corresponding to getting account state from the token state.
getAccountStateCall :: AccountIndex -> TokenStateKey -> PLTKernelQueryCall acct (Maybe TokenStateValue)
getAccountStateCall accountIndex subKey = GetTokenState (accountStateKey accountIndex subKey)

-- | Construct a @PLTKernelQueryCall@ corresponding to getting module state from the token state.
getModuleStateCall :: TokenStateKey -> PLTKernelQueryCall acct (Maybe TokenStateValue)
getModuleStateCall = GetTokenState . moduleStateKey

-- | A value of type @PLTKernelUpdateCall acct ret@ represents an invocation of an operation
--  in a 'PLTKernelUpdate' monad, where @PLTAccount m ~ acct@. The parameter @ret@ is the type
--  of the return value expected.
data PLTKernelUpdateCall acct ret where
    SetTokenState :: TokenStateKey -> Maybe TokenStateValue -> PLTKernelUpdateCall acct (Maybe Bool)
    Transfer :: acct -> acct -> TokenRawAmount -> Maybe Memo -> PLTKernelUpdateCall acct Bool
    LogTokenEvent :: TokenEventType -> TokenEventDetails -> PLTKernelUpdateCall acct ()
    Touch :: acct -> PLTKernelUpdateCall acct Bool

deriving instance (Show acct) => Show (PLTKernelUpdateCall acct ret)
deriving instance (Eq acct) => Eq (PLTKernelUpdateCall acct ret)

setAccountStateCall :: AccountIndex -> TokenStateKey -> Maybe TokenStateValue -> PLTKernelUpdateCall AccountIndex (Maybe Bool)
setAccountStateCall accountIndex subKey = SetTokenState (accountStateKey accountIndex subKey)

setModuleStateCall :: TokenStateKey -> Maybe TokenStateValue -> PLTKernelUpdateCall AccountIndex (Maybe Bool)
setModuleStateCall = SetTokenState . moduleStateKey

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

-- | A value of type @PLTKernelChargeEnergy@ represents an invocation of an
--  operation in a @PLTKernelChargeEnergy@ monad.
data PLTKernelChargeEnergyCall where
    PLTChargeEnergy :: Energy -> PLTKernelChargeEnergyCall

deriving instance Show PLTKernelChargeEnergyCall
deriving instance Eq PLTKernelChargeEnergyCall

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
    | -- | A 'PLTKernelChargeEnergy' call.
      PLTE PLTKernelChargeEnergyCall

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
    getAccountIndex acct = handleEvent $ PLTQ $ GetAccountIndex acct
    getAccountByIndex accountIndex = handleEvent $ PLTQ $ GetAccountByIndex accountIndex
    getAccountBalance acct = handleEvent $ PLTQ $ GetAccountBalance acct
    getAccountCanonicalAddress acct = handleEvent $ PLTQ $ GetAccountCanonicalAddress acct
    getCirculatingSupply = handleEvent $ PLTQ GetCirculatingSupply
    getDecimals = handleEvent $ PLTQ GetDecimals

instance
    (Eq e, Eq acct, Show e, Show acct, Show ret, Typeable e, Typeable acct, Typeable ret) =>
    PLTKernelUpdate (TraceM (PLTCall e acct) res ret)
    where
    setTokenState key mValue = handleEvent $ PLTU $ SetTokenState key mValue
    transfer sender receiver amount mMemo = handleEvent $ PLTU $ Transfer sender receiver amount mMemo
    logTokenEvent eventType details = handleEvent $ PLTU $ LogTokenEvent eventType details
    touch acc = handleEvent $ PLTU $ Touch acc

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

instance
    (Eq e, Eq acct, Show e, Show acct, Show ret, Typeable e, Typeable acct, Typeable ret) =>
    PLTKernelChargeEnergy (TraceM (PLTCall e acct) res ret)
    where
    pltTickEnergy = handleEvent . PLTE . PLTChargeEnergy

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
    it "invalid parameters: decode failure" $ do
        let trace :: Trace (PLTCall InitializeTokenError AccountIndex) ()
            trace =
                abortPLTError $
                    ITEDeserializationFailure "DeserialiseFailure 0 \"end of input\""
        assertTrace
            (initializeToken (TokenParameter mempty))
            trace
    -- In this example, a parameter is missing from the required initialization parameters
    it "invalid parameters: missing parameter" $ do
        let metadata = createTokenMetadataUrl "https://plt.token"
            governanceAccount =
                CborHolderAccount
                    { chaAccount = dummyAccountAddress 1,
                      chaCoinInfo = Nothing
                    }
            params =
                TokenInitializationParameters
                    { tipName = Nothing, -- missing required parameter
                      tipMetadata = Just metadata,
                      tipGovernanceAccount = Just governanceAccount,
                      tipAllowList = Just True,
                      tipDenyList = Just False,
                      tipInitialSupply = Nothing,
                      tipMintable = Just True,
                      tipBurnable = Just True
                    }
            tokenParam = TokenParameter $ SBS.toShort $ tokenInitializationParametersToBytes params
            trace :: Trace (PLTCall InitializeTokenError AccountIndex) ()
            trace =
                abortPLTError $
                    ITEDeserializationFailure "Token name is missing"
        assertTrace
            (initializeToken tokenParam)
            trace
    -- An example with minimal parameters specified, tests default value for parameters.
    it "parameter default values" $ do
        let metadata = createTokenMetadataUrl "https://plt.token"
            governanceAccount =
                CborHolderAccount
                    { chaAccount = dummyAccountAddress 1,
                      chaCoinInfo = Nothing
                    }
            params =
                TokenInitializationParameters
                    { tipName = Just "Protocol-level token",
                      tipMetadata = Just metadata,
                      tipGovernanceAccount = Just governanceAccount,
                      tipAllowList = Nothing,
                      tipDenyList = Nothing,
                      tipInitialSupply = Nothing,
                      tipMintable = Nothing,
                      tipBurnable = Nothing
                    }
            tokenParam = TokenParameter $ SBS.toShort $ tokenInitializationParametersToBytes params
            trace :: Trace (PLTCall InitializeTokenError AccountIndex) ()
            trace =
                (PLTU (setModuleStateCall "name" $ Just "Protocol-level token") :-> Just False)
                    :>>: (PLTU (setModuleStateCall "metadata" $ Just $ tokenMetadataUrlToBytes metadata) :-> Just False)
                    :>>: (PLTQ (GetAccount $ dummyAccountAddress 1) :-> Just 1)
                    :>>: (PLTQ (GetAccountIndex 1) :-> 1)
                    :>>: (PLTU (setModuleStateCall "governanceAccount" $ Just $ encode (1 :: Word64)) :-> Just False)
                    :>>: Done ()
        assertTrace (initializeToken tokenParam) trace
    -- An example with valid parameters (no minting).
    it "valid1" $ do
        let metadata = createTokenMetadataUrl "https://plt.token"
            governanceAccount =
                CborHolderAccount
                    { chaAccount = dummyAccountAddress 1,
                      chaCoinInfo = Nothing
                    }
            params =
                TokenInitializationParameters
                    { tipName = Just "Protocol-level token",
                      tipMetadata = Just metadata,
                      tipGovernanceAccount = Just governanceAccount,
                      tipAllowList = Just True,
                      tipDenyList = Just False,
                      tipInitialSupply = Nothing,
                      tipMintable = Just True,
                      tipBurnable = Just True
                    }
            tokenParam = TokenParameter $ SBS.toShort $ tokenInitializationParametersToBytes params
            trace :: Trace (PLTCall InitializeTokenError AccountIndex) ()
            trace =
                (PLTU (setModuleStateCall "name" $ Just "Protocol-level token") :-> Just False)
                    :>>: (PLTU (setModuleStateCall "metadata" $ Just $ tokenMetadataUrlToBytes metadata) :-> Just False)
                    :>>: (PLTU (setModuleStateCall "allowList" $ Just "") :-> Just False)
                    :>>: (PLTU (setModuleStateCall "mintable" $ Just "") :-> Just False)
                    :>>: (PLTU (setModuleStateCall "burnable" $ Just "") :-> Just False)
                    :>>: (PLTQ (GetAccount $ dummyAccountAddress 1) :-> Just 1)
                    :>>: (PLTQ (GetAccountIndex 1) :-> 1)
                    :>>: (PLTU (setModuleStateCall "governanceAccount" $ Just $ encode (1 :: Word64)) :-> Just False)
                    :>>: Done ()
        assertTrace (initializeToken tokenParam) trace
    -- An example with valid parameters and minting.
    it "valid2" $ do
        let metadata = createTokenMetadataUrlWithSha256 "https://plt2.token" $ SHA256.hashShort $ SBS.pack $ replicate 32 0
            governanceAccount =
                CborHolderAccount
                    { chaAccount = dummyAccountAddress 1,
                      chaCoinInfo = Nothing
                    }
            params =
                TokenInitializationParameters
                    { tipName = Just "Protocol-level token2",
                      tipMetadata = Just metadata,
                      tipGovernanceAccount = Just governanceAccount,
                      tipAllowList = Just False,
                      tipDenyList = Just True,
                      tipInitialSupply = Just TokenAmount{taValue = 500_000, taDecimals = 2},
                      tipMintable = Just False,
                      tipBurnable = Just False
                    }
            tokenParam = TokenParameter $ SBS.toShort $ tokenInitializationParametersToBytes params
            trace :: Trace (PLTCall InitializeTokenError AccountIndex) ()
            trace =
                (PLTU (setModuleStateCall "name" $ Just "Protocol-level token2") :-> Just False)
                    :>>: (PLTU (setModuleStateCall "metadata" $ Just $ tokenMetadataUrlToBytes metadata) :-> Just False)
                    :>>: (PLTU (setModuleStateCall "denyList" $ Just "") :-> Just False)
                    :>>: (PLTQ (GetAccount $ dummyAccountAddress 1) :-> Just 1)
                    :>>: (PLTQ (GetAccountIndex 1) :-> 1)
                    :>>: (PLTU (setModuleStateCall "governanceAccount" $ Just $ encode (1 :: Word64)) :-> Just False)
                    :>>: (PLTQ GetDecimals :-> 2)
                    :>>: (PLTPU (Mint (AccountIndex 1) (TokenRawAmount 500_000)) :-> True)
                    :>>: Done ()
        assertTrace (initializeToken tokenParam) trace
    -- In this test, the Kernel responds to the minting request indicating that it failed.
    it "mint fails" $ do
        let metadata = createTokenMetadataUrl "https://plt2.token"
            governanceAccount =
                CborHolderAccount
                    { chaAccount = dummyAccountAddress 1,
                      chaCoinInfo = Nothing
                    }
            params =
                TokenInitializationParameters
                    { tipName = Just "Protocol-level token2",
                      tipMetadata = Just metadata,
                      tipGovernanceAccount = Just governanceAccount,
                      tipAllowList = Just False,
                      tipDenyList = Just False,
                      tipInitialSupply = Just TokenAmount{taValue = 500_000, taDecimals = 2},
                      tipMintable = Just False,
                      tipBurnable = Just False
                    }
            tokenParam = TokenParameter $ SBS.toShort $ tokenInitializationParametersToBytes params
            trace :: Trace (PLTCall InitializeTokenError AccountIndex) ()
            trace =
                (PLTU (setModuleStateCall "name" $ Just "Protocol-level token2") :-> Just False)
                    :>>: (PLTU (setModuleStateCall "metadata" $ Just $ tokenMetadataUrlToBytes metadata) :-> Just False)
                    :>>: (PLTQ (GetAccount $ dummyAccountAddress 1) :-> Just 1)
                    :>>: (PLTQ (GetAccountIndex 1) :-> 1)
                    :>>: (PLTU (setModuleStateCall "governanceAccount" $ Just $ encode (1 :: Word64)) :-> Just False)
                    :>>: (PLTQ GetDecimals :-> 2)
                    :>>: (PLTPU (Mint (AccountIndex 1) (TokenRawAmount 500_000)) :-> False)
                    :>>: abortPLTError (ITEInvalidMintAmount "Kernel failed to mint")
        assertTrace (initializeToken tokenParam) trace
    -- In this example, the parameters specify an initial supply with higher precision than the
    -- token allows. (decimals is 6, but GetDecimals returns 2.)
    it "too many decimals specified" $ do
        let metadata = createTokenMetadataUrl "https://plt2.token"
            governanceAccount =
                CborHolderAccount
                    { chaAccount = dummyAccountAddress 1,
                      chaCoinInfo = Nothing
                    }
            params =
                TokenInitializationParameters
                    { tipName = Just "Protocol-level token2",
                      tipMetadata = Just metadata,
                      tipGovernanceAccount = Just governanceAccount,
                      tipAllowList = Just False,
                      tipDenyList = Just False,
                      tipInitialSupply = Just TokenAmount{taValue = 500_000, taDecimals = 6},
                      tipMintable = Just False,
                      tipBurnable = Just False
                    }
            tokenParam = TokenParameter $ SBS.toShort $ tokenInitializationParametersToBytes params
            trace :: Trace (PLTCall InitializeTokenError AccountIndex) ()
            trace =
                (PLTU (setModuleStateCall "name" $ Just "Protocol-level token2") :-> Just False)
                    :>>: (PLTU (setModuleStateCall "metadata" $ Just $ tokenMetadataUrlToBytes metadata) :-> Just False)
                    :>>: (PLTQ (GetAccount $ dummyAccountAddress 1) :-> Just 1)
                    :>>: (PLTQ (GetAccountIndex 1) :-> 1)
                    :>>: (PLTU (setModuleStateCall "governanceAccount" $ Just $ encode (1 :: Word64)) :-> Just False)
                    :>>: (PLTQ GetDecimals :-> 2)
                    :>>: abortPLTError (ITEInvalidMintAmount "Token amount precision mismatch")
        assertTrace (initializeToken tokenParam) trace
    -- In this example, the parameters specify an initial supply with lower precision than the
    -- token requires. (decimals is 2, but GetDecimals returns 6.)
    it "not enough decimals specified" $ do
        let metadata = createTokenMetadataUrl "https://plt2.token"
        let governanceAccount =
                CborHolderAccount
                    { chaAccount = dummyAccountAddress 1,
                      chaCoinInfo = Nothing
                    }
        let params =
                TokenInitializationParameters
                    { tipName = Just "Protocol-level token2",
                      tipMetadata = Just metadata,
                      tipGovernanceAccount = Just governanceAccount,
                      tipAllowList = Just False,
                      tipDenyList = Just False,
                      tipInitialSupply = Just TokenAmount{taValue = 500_000, taDecimals = 2},
                      tipMintable = Just False,
                      tipBurnable = Just False
                    }
            tokenParam = TokenParameter $ SBS.toShort $ tokenInitializationParametersToBytes params
            trace :: Trace (PLTCall InitializeTokenError AccountIndex) ()
            trace =
                (PLTU (setModuleStateCall "name" $ Just "Protocol-level token2") :-> Just False)
                    :>>: (PLTU (setModuleStateCall "metadata" $ Just $ tokenMetadataUrlToBytes metadata) :-> Just False)
                    :>>: (PLTQ (GetAccount $ dummyAccountAddress 1) :-> Just 1)
                    :>>: (PLTQ (GetAccountIndex 1) :-> 1)
                    :>>: (PLTU (setModuleStateCall "governanceAccount" $ Just $ encode (1 :: Word64)) :-> Just False)
                    :>>: (PLTQ GetDecimals :-> 6)
                    :>>: abortPLTError (ITEInvalidMintAmount "Token amount precision mismatch")
        assertTrace (initializeToken tokenParam) trace

dummyAccountAddress :: Int -> AccountAddress
dummyAccountAddress seed = fst $ randomAccountAddress (mkStdGen seed)

testExecuteTokenUpdateTransactionTransfer :: Spec
testExecuteTokenUpdateTransactionTransfer = describe "executeTokenUpdateTransaction transfer" $ do
    it "invalid transaction" $ do
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                abortPLTError . encodeTokenRejectReason $
                    DeserializationFailure (Just "DeserialiseFailure 0 \"end of input\"")
        assertTrace
            (executeTokenUpdateTransaction (sender 0) (TokenParameter mempty))
            trace
    it "empty operations" $ do
        let transaction = TokenUpdateTransaction Seq.empty
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 2)
                    :>>: Done ()
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "transfer: paused operation" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [mkTransferOp amt10'000 receiver1 Nothing]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 3)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Just "")
                    :>>: ( abortPLTError . encodeTokenRejectReason $
                            OperationNotPermitted
                                { trrOperationIndex = 0,
                                  trrAddressNotPermitted = Nothing,
                                  trrReason = Just "token operation transfer is paused"
                                }
                         )
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "transfer OK" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [mkTransferOp amt10'000 receiver1 Nothing]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 3)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "denyList") :-> Nothing)
                    :>>: (PLTU (Transfer 0 4 10_000 Nothing) :-> True)
                    :>>: Done ()
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "transfer OK: long memo, max amount" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [mkTransferOp amtMax receiver2 (Just (UntaggedMemo longMemo))]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 0)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 2)) :-> Just 4)
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "denyList") :-> Nothing)
                    :>>: (PLTU (Transfer 0 4 maxBound (Just longMemo)) :-> True)
                    :>>: Done ()
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "invalid memo" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [mkTransferOp amtMax receiver2 (Just (UntaggedMemo badMemo))]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                abortPLTError . encodeTokenRejectReason $
                    DeserializationFailure (Just "DeserialiseFailure 277 \"Size of the memo (257 bytes) exceeds maximum allowed size (256 bytes).\"")
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    -- In this test, although the amount deserializes successfully, it is too large because of
    -- the number of decimals in the token representation.
    it "amount too large" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [mkTransferOp amtMax receiver2 (Just (UntaggedMemo longMemo))]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 2)
                    :>>: ( abortPLTError . encodeTokenRejectReason $
                            DeserializationFailure (Just "Token amount outside representable range: Token amount precision mismatch")
                         )
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "two transfers" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [ mkTransferOp amt10'000000 receiver1 (Just (CBORMemo cborMemo)),
                      mkTransferOp amt50'000000 receiver2 (Just (UntaggedMemo simpleMemo))
                    ]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "denyList") :-> Nothing)
                    :>>: (PLTU (Transfer 0 4 10_000_000 (Just cborMemo)) :-> True)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 2)) :-> Just 121)
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "denyList") :-> Nothing)
                    :>>: (PLTU (Transfer 0 121 50_000_000 (Just simpleMemo)) :-> True)
                    :>>: Done ()
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "two transfers - first fails (insufficient funds)" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [ mkTransferOp amt10'000000 receiver1 (Just (CBORMemo cborMemo)),
                      mkTransferOp amt50'000000 receiver2 (Just (UntaggedMemo simpleMemo))
                    ]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "denyList") :-> Nothing)
                    :>>: (PLTU (Transfer 0 4 10_000_000 (Just cborMemo)) :-> False)
                    :>>: (PLTQ (GetAccountBalance 0) :-> 0)
                    :>>: ( abortPLTError . encodeTokenRejectReason $
                            TokenBalanceInsufficient
                                { trrOperationIndex = 0,
                                  trrAvailableBalance = TokenAmount 0 6,
                                  trrRequiredBalance = amt10'000000
                                }
                         )
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "two transfers - second fails (insufficient funds)" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [ mkTransferOp amt10'000000 receiver1 (Just (CBORMemo cborMemo)),
                      mkTransferOp amt50'000000 receiver2 (Just (UntaggedMemo simpleMemo))
                    ]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "denyList") :-> Nothing)
                    :>>: (PLTU (Transfer 0 4 10_000_000 (Just cborMemo)) :-> True)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 2)) :-> Just 16)
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "denyList") :-> Nothing)
                    :>>: (PLTU (Transfer 0 16 50_000_000 (Just simpleMemo)) :-> False)
                    :>>: (PLTQ (GetAccountBalance 0) :-> 5_000_000)
                    :>>: ( abortPLTError . encodeTokenRejectReason $
                            TokenBalanceInsufficient
                                { trrOperationIndex = 1,
                                  trrAvailableBalance = TokenAmount 5_000_000 6,
                                  trrRequiredBalance = amt50'000000
                                }
                         )
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "two transfers - second fails (invalid recipient)" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [ mkTransferOp amt10'000000 receiver1 (Just (CBORMemo cborMemo)),
                      mkTransferOp amt50'000000 receiver2 (Just (UntaggedMemo simpleMemo))
                    ]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "denyList") :-> Nothing)
                    :>>: (PLTU (Transfer 0 4 10_000_000 (Just cborMemo)) :-> True)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 2)) :-> Nothing)
                    :>>: ( abortPLTError . encodeTokenRejectReason $
                            AddressNotFound
                                { trrOperationIndex = 1,
                                  trrAddress = receiver2
                                }
                         )
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "allow list: allowed" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [mkTransferOp amt10'000000 receiver1 Nothing]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Just "")
                    :>>: (PLTQ (GetAccountIndex 0) :-> 0)
                    :>>: (PLTQ (getAccountStateCall 0 "allowList") :-> Just "")
                    :>>: (PLTQ (GetAccountIndex 4) :-> 4)
                    :>>: (PLTQ (getAccountStateCall 4 "allowList") :-> Just "")
                    :>>: (PLTQ (getModuleStateCall "denyList") :-> Nothing)
                    :>>: (PLTU (Transfer 0 4 10_000_000 Nothing) :-> True)
                    :>>: Done ()
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "allow list: sender not allowed" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [mkTransferOp amt10'000000 receiver1 Nothing]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Just "")
                    :>>: (PLTQ (GetAccountIndex 0) :-> 0)
                    :>>: (PLTQ (getAccountStateCall 0 "allowList") :-> Nothing)
                    :>>: ( abortPLTError . encodeTokenRejectReason $
                            OperationNotPermitted
                                { trrOperationIndex = 0,
                                  trrAddressNotPermitted =
                                    Just (accountTokenHolder (dummyAccountAddress 0)),
                                  trrReason = Just "sender not in allow list"
                                }
                         )
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "allow list: recipient not allowed" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [mkTransferOp amt10'000000 receiver1 Nothing]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 9)
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Just "")
                    :>>: (PLTQ (GetAccountIndex 23) :-> 23)
                    :>>: (PLTQ (getAccountStateCall 23 "allowList") :-> Just "")
                    :>>: (PLTQ (GetAccountIndex 9) :-> 9)
                    :>>: (PLTQ (getAccountStateCall 9 "allowList") :-> Nothing)
                    :>>: ( abortPLTError . encodeTokenRejectReason $
                            OperationNotPermitted
                                { trrOperationIndex = 0,
                                  trrAddressNotPermitted = Just receiver1,
                                  trrReason = Just "recipient not in allow list"
                                }
                         )
        assertTrace (executeTokenUpdateTransaction (sender 23) (encodeTransaction transaction)) trace
    it "deny list: allowed" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [mkTransferOp amt10'000000 receiver1 Nothing]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "denyList") :-> Just "")
                    :>>: (PLTQ (GetAccountIndex 0) :-> 0)
                    :>>: (PLTQ (getAccountStateCall 0 "denyList") :-> Nothing)
                    :>>: (PLTQ (GetAccountIndex 4) :-> 4)
                    :>>: (PLTQ (getAccountStateCall 4 "denyList") :-> Nothing)
                    :>>: (PLTU (Transfer 0 4 10_000_000 Nothing) :-> True)
                    :>>: Done ()
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "deny list: sender denied" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [mkTransferOp amt10'000000 receiver1 Nothing]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "denyList") :-> Just "")
                    :>>: (PLTQ (GetAccountIndex 0) :-> 0)
                    :>>: (PLTQ (getAccountStateCall 0 "denyList") :-> Just "")
                    :>>: ( abortPLTError . encodeTokenRejectReason $
                            OperationNotPermitted
                                { trrOperationIndex = 0,
                                  trrAddressNotPermitted =
                                    Just (accountTokenHolder (dummyAccountAddress 0)),
                                  trrReason = Just "sender in deny list"
                                }
                         )
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "deny list: recipient denied" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [mkTransferOp amt10'000000 receiver1 Nothing]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "denyList") :-> Just "")
                    :>>: (PLTQ (GetAccountIndex 0) :-> 0)
                    :>>: (PLTQ (getAccountStateCall 0 "denyList") :-> Nothing)
                    :>>: (PLTQ (GetAccountIndex 4) :-> 4)
                    :>>: (PLTQ (getAccountStateCall 4 "denyList") :-> Just "")
                    :>>: ( abortPLTError . encodeTokenRejectReason $
                            OperationNotPermitted
                                { trrOperationIndex = 0,
                                  trrAddressNotPermitted = Just receiver1,
                                  trrReason = Just "recipient in deny list"
                                }
                         )
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "allow & deny list: allowed" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [mkTransferOp amt10'000000 receiver1 Nothing]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Just "")
                    :>>: (PLTQ (GetAccountIndex 0) :-> 0)
                    :>>: (PLTQ (getAccountStateCall 0 "allowList") :-> Just "")
                    :>>: (PLTQ (GetAccountIndex 4) :-> 4)
                    :>>: (PLTQ (getAccountStateCall 4 "allowList") :-> Just "")
                    :>>: (PLTQ (getModuleStateCall "denyList") :-> Just "")
                    :>>: (PLTQ (GetAccountIndex 0) :-> 0)
                    :>>: (PLTQ (getAccountStateCall 0 "denyList") :-> Nothing)
                    :>>: (PLTQ (GetAccountIndex 4) :-> 4)
                    :>>: (PLTQ (getAccountStateCall 4 "denyList") :-> Nothing)
                    :>>: (PLTU (Transfer 0 4 10_000_000 Nothing) :-> True)
                    :>>: Done ()
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "5000 transfers" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [ mkTransferOp
                        amt10'000
                        (CborHolderAccount (dummyAccountAddress i) Nothing)
                        Nothing
                    | i <- [1 .. 5000]
                    ]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace = (PLTQ GetDecimals :-> 3) :>>: traceLoop 1
            traceLoop n
                | n > 5000 = Done ()
                | otherwise =
                    (PLTE (PLTChargeEnergy tokenTransferCost) :-> ())
                        :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                        :>>: (PLTQ (GetAccount (dummyAccountAddress (fromIntegral n))) :-> Just n)
                        :>>: (PLTQ (getModuleStateCall "allowList") :-> Nothing)
                        :>>: (PLTQ (getModuleStateCall "denyList") :-> Nothing)
                        :>>: (PLTU (Transfer 123_456 n 10_000 Nothing) :-> True)
                        :>>: traceLoop (n + 1)
        assertTrace (executeTokenUpdateTransaction (sender 123_456) (encodeTransaction transaction)) trace
  where
    receiver1 = CborHolderAccount (dummyAccountAddress 1) Nothing
    receiver2 = CborHolderAccount (dummyAccountAddress 2) (Just CoinInfoConcordium)
    amt10'000 = TokenAmount 10_000 3
    amtMax = TokenAmount maxBound 0
    amt10'000000 = TokenAmount 10_000_000 6
    amt50'000000 = TokenAmount 50_000_000 6
    simpleMemo = Memo "Test"
    cborMemo = Memo "dTest"
    longMemo = Memo $ SBS.replicate maxMemoSize 60
    badMemo = Memo $ SBS.replicate (maxMemoSize + 1) 60
    mkTransferOp ttAmount ttRecipient ttMemo = TokenTransfer TokenTransferBody{..}
    encodeTransaction = TokenParameter . SBS.toShort . tokenUpdateTransactionToBytes
    sender ai = TransactionContext (AccountIndex ai) (dummyAccountAddress $ fromIntegral ai)

testExecuteTokenUpdateTransactionMintBurnPause :: Spec
testExecuteTokenUpdateTransactionMintBurnPause = describe "executeTokenUpdateTransaction mint, burn & pause" $ do
    it "invalid transaction" $ do
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                abortPLTError . encodeTokenRejectReason $
                    DeserializationFailure (Just "DeserialiseFailure 0 \"end of input\"")
        assertTrace
            (executeTokenUpdateTransaction (sender 0) (TokenParameter mempty))
            trace
    it "empty operations" $ do
        let transaction = TokenUpdateTransaction Seq.empty
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 2)
                    :>>: Done ()
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "unauthorized operation" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [TokenMint (TokenAmount 10_000_000 6)]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenMintCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 0)))
                    :>>: (PLTQ (GetAccountIndex 1) :-> AccountIndex 1)
                    :>>: ( abortPLTError . encodeTokenRejectReason $
                            OperationNotPermitted
                                { trrOperationIndex = 0,
                                  trrAddressNotPermitted =
                                    Just $
                                        CborHolderAccount
                                            { chaAccount = dummyAccountAddress 1,
                                              chaCoinInfo = Just CoinInfoConcordium
                                            },
                                  trrReason = Just "sender is not the token governance account"
                                }
                         )
        assertTrace (executeTokenUpdateTransaction (sender 1) (encodeTransaction transaction)) trace
    it "mint: paused operation" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [TokenMint (TokenAmount 10_000_000 6)]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenMintCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 0)))
                    :>>: (PLTQ (GetAccountIndex 0) :-> AccountIndex 0)
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Just "")
                    :>>: ( abortPLTError . encodeTokenRejectReason $
                            OperationNotPermitted
                                { trrOperationIndex = 0,
                                  trrAddressNotPermitted = Nothing,
                                  trrReason = Just "token operation mint is paused"
                                }
                         )
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "mint: OK" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [TokenMint (TokenAmount 10_000_000 6)]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenMintCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 0)))
                    :>>: (PLTQ (GetAccountIndex 0) :-> AccountIndex 0)
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "mintable") :-> Just "")
                    :>>: (PLTPU (Mint 0 10_000_000) :-> True)
                    :>>: Done ()
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "mint: not mintable" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [TokenMint (TokenAmount 10_000_000 6)]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenMintCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 0)))
                    :>>: (PLTQ (GetAccountIndex 0) :-> AccountIndex 0)
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "mintable") :-> Nothing)
                    :>>: ( abortPLTError . encodeTokenRejectReason $
                            UnsupportedOperation
                                { trrOperationIndex = 0,
                                  trrOperationType = "mint",
                                  trrReason = Just "feature not enabled"
                                }
                         )
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "mint: overflow" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [TokenMint (TokenAmount 10_000_000 6)]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenMintCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 0)))
                    :>>: (PLTQ (GetAccountIndex 0) :-> AccountIndex 0)
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "mintable") :-> Just "")
                    :>>: (PLTPU (Mint 0 10_000_000) :-> False)
                    :>>: (PLTQ GetCirculatingSupply :-> (maxBound - 1_000_000))
                    :>>: ( abortPLTError . encodeTokenRejectReason $
                            MintWouldOverflow
                                { trrOperationIndex = 0,
                                  trrRequestedAmount = TokenAmount 10_000_000 6,
                                  trrCurrentSupply = TokenAmount (maxBound - 1_000_000) 6,
                                  trrMaxRepresentableAmount = TokenAmount maxBound 6
                                }
                         )
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "burn: OK" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [TokenBurn (TokenAmount 10_000_000 6)]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenBurnCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 0)))
                    :>>: (PLTQ (GetAccountIndex 0) :-> AccountIndex 0)
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "burnable") :-> Just "")
                    :>>: (PLTPU (Burn 0 10_000_000) :-> True)
                    :>>: Done ()
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "burn: paused operation" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [TokenBurn (TokenAmount 10_000_000 6)]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenBurnCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 0)))
                    :>>: (PLTQ (GetAccountIndex 0) :-> AccountIndex 0)
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Just "")
                    :>>: ( abortPLTError . encodeTokenRejectReason $
                            OperationNotPermitted
                                { trrOperationIndex = 0,
                                  trrAddressNotPermitted = Nothing,
                                  trrReason = Just "token operation burn is paused"
                                }
                         )
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "burn: not burnable" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [TokenBurn (TokenAmount 10_000_000 6)]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenBurnCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 0)))
                    :>>: (PLTQ (GetAccountIndex 0) :-> AccountIndex 0)
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "burnable") :-> Nothing)
                    :>>: ( abortPLTError . encodeTokenRejectReason $
                            UnsupportedOperation
                                { trrOperationIndex = 0,
                                  trrOperationType = "burn",
                                  trrReason = Just "feature not enabled"
                                }
                         )
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "burn: balance insufficient" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [TokenBurn (TokenAmount 10_000_000 6)]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenBurnCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 0)))
                    :>>: (PLTQ (GetAccountIndex 0) :-> AccountIndex 0)
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "burnable") :-> Just "")
                    :>>: (PLTPU (Burn 0 10_000_000) :-> False)
                    :>>: (PLTQ (GetAccountBalance 0) :-> 100)
                    :>>: ( abortPLTError . encodeTokenRejectReason $
                            TokenBalanceInsufficient
                                { trrOperationIndex = 0,
                                  trrAvailableBalance = TokenAmount 100 6,
                                  trrRequiredBalance = TokenAmount 10_000_000 6
                                }
                         )
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "mint & burn: OK" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [ TokenMint (TokenAmount 10_000_000 6),
                      TokenBurn (TokenAmount 5_000_000 6)
                    ]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenMintCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 0)))
                    :>>: (PLTQ (GetAccountIndex 0) :-> AccountIndex 0)
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "mintable") :-> Just "")
                    :>>: (PLTPU (Mint 0 10_000_000) :-> True)
                    :>>: (PLTE (PLTChargeEnergy tokenBurnCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 0)))
                    :>>: (PLTQ (GetAccountIndex 0) :-> AccountIndex 0)
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "burnable") :-> Just "")
                    :>>: (PLTPU (Burn 0 5_000_000) :-> True)
                    :>>: Done ()
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    -- testLists
    it "pause: OK" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [TokenPause]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenPauseUnpauseCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 0)))
                    :>>: (PLTQ (GetAccountIndex 0) :-> AccountIndex 0)
                    :>>: (PLTU (setModuleStateCall "paused" $ Just "") :-> Just True)
                    :>>: (PLTU (LogTokenEvent (TokenEventType "pause") emptyEventDetails) :-> ())
                    :>>: Done ()
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
    it "unpause: OK" $ do
        let transaction =
                TokenUpdateTransaction . Seq.fromList $
                    [TokenUnpause]
        let trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
            trace =
                (PLTQ GetDecimals :-> 6)
                    :>>: (PLTE (PLTChargeEnergy tokenPauseUnpauseCost) :-> ())
                    :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 0)))
                    :>>: (PLTQ (GetAccountIndex 0) :-> AccountIndex 0)
                    :>>: (PLTU (setModuleStateCall "paused" Nothing) :-> Just False)
                    :>>: (PLTU (LogTokenEvent (TokenEventType "unpause") emptyEventDetails) :-> ())
                    :>>: Done ()
        assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
  where
    encodeTransaction = TokenParameter . SBS.toShort . tokenUpdateTransactionToBytes
    sender ai = TransactionContext (AccountIndex ai) (dummyAccountAddress $ fromIntegral ai)

data AddRemove = Add | Remove
data AllowDeny = Allow | Deny

type ListTestConf = (AddRemove, AllowDeny)

testLists :: Spec
testLists = do
    forM_ [(Add, Allow), (Add, Deny), (Remove, Allow), (Remove, Deny)] $ \listConf -> do
        describe (Text.unpack (ltcOperation listConf)) $ do
            it "OK" $ do
                let transaction =
                        TokenUpdateTransaction . Seq.fromList $
                            [ltcMakeOperation listConf receiver1]
                    trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
                    trace =
                        (PLTQ GetDecimals :-> 6)
                            :>>: (PLTE (PLTChargeEnergy tokenListOperationCost) :-> ())
                            :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 0)))
                            :>>: (PLTQ (GetAccountIndex 0) :-> AccountIndex 0)
                            :>>: (PLTQ (getModuleStateCall (ltcFeature listConf)) :-> Just "")
                            :>>: (PLTQ (GetAccount (dummyAccountAddress 1)) :-> Just 4)
                            :>>: (PLTU (Touch 4) :-> False)
                            :>>: (PLTQ (GetAccountIndex 4) :-> 4)
                            :>>: (PLTU (setAccountStateCall 4 (ltcFeature listConf) (ltcNewValue listConf)) :-> Just False)
                            :>>: ( PLTU
                                    ( LogTokenEvent
                                        (TokenEventType $ ltcOperation listConf)
                                        (encodeTargetDetails receiver1)
                                    )
                                    :-> ()
                                 )
                            :>>: Done ()
                assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
            it "feature not enabled" $ do
                let transaction =
                        TokenUpdateTransaction . Seq.fromList $
                            [ltcMakeOperation listConf receiver1]
                    trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
                    trace =
                        (PLTQ GetDecimals :-> 6)
                            :>>: (PLTE (PLTChargeEnergy tokenListOperationCost) :-> ())
                            :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 0)))
                            :>>: (PLTQ (GetAccountIndex 0) :-> AccountIndex 0)
                            :>>: (PLTQ (getModuleStateCall (ltcFeature listConf)) :-> Nothing)
                            :>>: ( abortPLTError . encodeTokenRejectReason $
                                    UnsupportedOperation
                                        { trrOperationIndex = 0,
                                          trrOperationType = ltcOperation listConf,
                                          trrReason = Just "feature not enabled"
                                        }
                                 )
                assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
            it "account not found" $ do
                let transaction =
                        TokenUpdateTransaction . Seq.fromList $
                            [ltcMakeOperation listConf receiver2]
                    trace :: Trace (PLTCall EncodedTokenRejectReason AccountIndex) ()
                    trace =
                        (PLTQ GetDecimals :-> 6)
                            :>>: (PLTE (PLTChargeEnergy tokenListOperationCost) :-> ())
                            :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 0)))
                            :>>: (PLTQ (GetAccountIndex 0) :-> AccountIndex 0)
                            :>>: (PLTQ (getModuleStateCall (ltcFeature listConf)) :-> Just "")
                            :>>: (PLTQ (GetAccount (dummyAccountAddress 2)) :-> Nothing)
                            :>>: ( abortPLTError . encodeTokenRejectReason $
                                    AddressNotFound
                                        { trrOperationIndex = 0,
                                          trrAddress = receiver2
                                        }
                                 )
                assertTrace (executeTokenUpdateTransaction (sender 0) (encodeTransaction transaction)) trace
  where
    encodeTransaction = TokenParameter . SBS.toShort . tokenUpdateTransactionToBytes
    receiver1 = CborHolderAccount (dummyAccountAddress 1) Nothing
    receiver2 = CborHolderAccount (dummyAccountAddress 2) (Just CoinInfoConcordium)
    ltcFeature :: ListTestConf -> TokenStateKey
    ltcFeature (_, Allow) = "allowList"
    ltcFeature (_, Deny) = "denyList"
    ltcOperation :: (IsString s, Semigroup s) => ListTestConf -> s
    ltcOperation c = ltcAction c <> ltcList c <> "List"
      where
        ltcAction (Add, _) = "add"
        ltcAction (Remove, _) = "remove"
        ltcList (_, Allow) = "Allow"
        ltcList (_, Deny) = "Deny"
    ltcMakeOperation :: ListTestConf -> CborHolderAccount -> TokenOperation
    ltcMakeOperation (Add, Allow) = TokenAddAllowList
    ltcMakeOperation (Remove, Allow) = TokenRemoveAllowList
    ltcMakeOperation (Add, Deny) = TokenAddDenyList
    ltcMakeOperation (Remove, Deny) = TokenRemoveDenyList
    ltcNewValue :: ListTestConf -> Maybe BS.ByteString
    ltcNewValue (Add, _) = Just ""
    ltcNewValue (Remove, _) = Nothing
    sender ai = TransactionContext (AccountIndex ai) (dummyAccountAddress $ fromIntegral ai)

testQueryTokenModuleState :: Spec
testQueryTokenModuleState = describe "queryTokenModuleState" $ do
    it "constructs the expected state key for accounts" $ do
        let key = accountStateKey (AccountIndex 456) "someSubKey"
            expected = "\x73\x9D\NUL\NUL\NUL\NUL\NUL\NUL\x01\xC8someSubKey"
         in assertEqual "Unexpected accountStateKey" expected key
        let key = accountStateKey (AccountIndex 9_999_999_999) "anotherSubKey"
            expected =
                "\x73\x9D\NUL\NUL\NUL\x02\x54\x0B\xE3\xFF\
                \anotherSubKey"
         in assertEqual "Unexpected accountStateKey" expected key
    it "constructs the expected module state key" $ do
        let key = moduleStateKey "test"
            expected = "\NUL\NULtest"
         in assertEqual "Unexpected module state key" expected key
        let key = moduleStateKey "Another"
            expected = "\NUL\NULAnother"
         in assertEqual "Unexpected module state key" expected key

    it "Example 1" $ do
        let metadata = createTokenMetadataUrl "some URL"
            governanceAccount =
                CborHolderAccount
                    { chaAccount = dummyAccountAddress 1,
                      chaCoinInfo = Nothing
                    }
            trace :: Trace (PLTCall QueryTokenError AccountIndex) BS.ByteString
            trace =
                (PLTQ (getModuleStateCall "name") :-> Just "My protocol-level token")
                    :>>: (PLTQ (getModuleStateCall "metadata") :-> Just (tokenMetadataUrlToBytes metadata))
                    :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 1)))
                    :>>: (PLTQ (GetAccountByIndex (AccountIndex 1)) :-> Just 1)
                    :>>: (PLTQ (GetAccountCanonicalAddress 1) :-> dummyAccountAddress 1)
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Just "")
                    :>>: (PLTQ (getModuleStateCall "denyList") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "mintable") :-> Just "")
                    :>>: (PLTQ (getModuleStateCall "burnable") :-> Nothing)
                    :>>: Done
                        ( tokenModuleStateToBytes
                            TokenModuleState
                                { tmsName = Just "My protocol-level token",
                                  tmsMetadata = Just metadata,
                                  tmsGovernanceAccount = Just governanceAccount,
                                  tmsPaused = Just False,
                                  tmsAllowList = Just True,
                                  tmsDenyList = Just False,
                                  tmsMintable = Just True,
                                  tmsBurnable = Just False,
                                  tmsAdditional = mempty
                                }
                        )
        assertTrace queryTokenModuleState trace
    it "Example 2" $ do
        let metadata = createTokenMetadataUrlWithSha256 "https://token.metadata" $ SHA256.hashShort $ SBS.pack $ replicate 32 0
            governanceAccount =
                CborHolderAccount
                    { chaAccount = dummyAccountAddress 1,
                      chaCoinInfo = Nothing
                    }
            trace :: Trace (PLTCall QueryTokenError AccountIndex) BS.ByteString
            trace =
                (PLTQ (getModuleStateCall "name") :-> Just "Another PLT")
                    :>>: (PLTQ (getModuleStateCall "metadata") :-> Just (tokenMetadataUrlToBytes metadata))
                    :>>: (PLTQ (getModuleStateCall "governanceAccount") :-> Just (encode (AccountIndex 1)))
                    :>>: (PLTQ (GetAccountByIndex (AccountIndex 1)) :-> Just 1)
                    :>>: (PLTQ (GetAccountCanonicalAddress 1) :-> dummyAccountAddress 1)
                    :>>: (PLTQ (getModuleStateCall "paused") :-> Just "")
                    :>>: (PLTQ (getModuleStateCall "allowList") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "denyList") :-> Just "")
                    :>>: (PLTQ (getModuleStateCall "mintable") :-> Nothing)
                    :>>: (PLTQ (getModuleStateCall "burnable") :-> Nothing)
                    :>>: Done
                        ( tokenModuleStateToBytes
                            TokenModuleState
                                { tmsName = Just "Another PLT",
                                  tmsMetadata = Just metadata,
                                  tmsGovernanceAccount = Just governanceAccount,
                                  tmsPaused = Just True,
                                  tmsAllowList = Just False,
                                  tmsDenyList = Just True,
                                  tmsMintable = Just False,
                                  tmsBurnable = Just False,
                                  tmsAdditional = mempty
                                }
                        )
        assertTrace queryTokenModuleState trace

testTokenOutOfEnergy :: Spec
testTokenOutOfEnergy = describe "tokenOutOfEnergy" $ do
    it "multiple transactions are charged correctly" $ do
        let maxBlockEnergy = 10_000
        ts <-
            Runner.processUngroupedTransactions
                (txs1 totMintCost totTransferCost totBurnCost)
        (Helpers.SchedulerResult{..}, doBlockStateAssertions) <-
            Helpers.runSchedulerTest
                (testConfig maxBlockEnergy)
                (initialBlockState @'P9)
                checkState
                ts
        let Types.FilteredTransactions{..} = srTransactions
        let rejects = [res | (_, summary) <- ftAdded, res@(Types.TxReject _) <- [summary ^. Types.summaryResult]]

        -- error $ show ftAdded ++ show ftFailed
        assertEqual "should be 3 transactions" 3 (length $ Types.perAccountTransactions ts)
        assertBool ("should not contain failed transactions: " ++ show ftFailed) (null ftFailed)
        assertBool ("should not contain rejected transactions: " ++ show rejects) (null rejects)
        assertEqual
            "transactions used the expected amount of energy"
            (totMintCost + totTransferCost + totBurnCost)
            srUsedEnergy
        doBlockStateAssertions

    it "3 transactions, block energy suffices only for the first" $ do
        let maxBlockEnergy = 600
        ts <-
            Runner.processUngroupedTransactions
                (txs1 totMintCost totTransferCost totBurnCost)
        (Helpers.SchedulerResult{..}, doBlockStateAssertions) <-
            Helpers.runSchedulerTest
                (testConfig maxBlockEnergy)
                (initialBlockState @'P9)
                checkState
                ts
        let Types.FilteredTransactions{..} = srTransactions
        let rejects = [res | (_, summary) <- ftAdded, res@(Types.TxReject _) <- [summary ^. Types.summaryResult]]
        let outOfEnergyTx = [_tx | _tx@(_, Types.ExceedsMaxBlockEnergy) <- ftFailed]

        assertEqual "should be 3 transactions" 3 (length $ Types.perAccountTransactions ts)
        assertEqual ("should contain 1 out-of-energy failed transaction" ++ ": " ++ show outOfEnergyTx) 1 (length outOfEnergyTx)
        assertBool ("should not contain rejected transactions: " ++ show rejects) (null rejects)
        assertEqual
            "transactions used the expected amount of energy"
            totMintCost
            srUsedEnergy
        doBlockStateAssertions

    it "1 transaction containing 2 operations, block energy suffices only for the first" $ do
        let maxBlockEnergy = totMintCost + totTransferCost
        ts <-
            Runner.processUngroupedTransactions
                ( txs2
                    -- enough energy for both transfer operations
                    (2 * (constA + constB * 150 + tokenUpdateBaseCost + tokenTransferCost))
                )

        (Helpers.SchedulerResult{..}, doBlockStateAssertions) <-
            Helpers.runSchedulerTest
                (testConfig maxBlockEnergy)
                (initialBlockState @'P9)
                checkState
                ts
        let Types.FilteredTransactions{..} = srTransactions
        let outOfEnergyTx = [_tx | _tx@(_, Types.ExceedsMaxBlockEnergy) <- ftFailed]

        assertEqual "should be 2 transactions" 2 (length $ Types.perAccountTransactions ts)
        assertEqual ("should contain 1 out-of-energy failed transaction" ++ ": " ++ show outOfEnergyTx) 1 (length outOfEnergyTx)
        assertEqual
            "transaction used the expected amount of energy"
            totMintCost
            srUsedEnergy
        doBlockStateAssertions

    it "3 transactions, energy suffices only for the first" $ do
        let maxBlockEnergy = 10_000
        ts <-
            Runner.processUngroupedTransactions
                ( txs1
                    totMintCost
                    -- energy to low
                    (totTransferCost - 1)
                    (totBurnCost - 1)
                )

        (Helpers.SchedulerResult{..}, doBlockStateAssertions) <-
            Helpers.runSchedulerTest
                (testConfig maxBlockEnergy)
                (initialBlockState @'P9)
                checkState
                ts
        let Types.FilteredTransactions{..} = srTransactions
        let rejects = [res | (_, summary) <- ftAdded, res@(Types.TxReject Types.OutOfEnergy) <- [summary ^. Types.summaryResult]]
        let outOfEnergyTx = [_tx | _tx@(_, Types.ExceedsMaxBlockEnergy) <- ftFailed]

        assertEqual "should be 1 transaction" 3 (length $ Types.perAccountTransactions ts)
        assertBool ("should contain no out-of-energy failed transaction" ++ ": " ++ show outOfEnergyTx) (null outOfEnergyTx)
        assertEqual ("should contain 2 out-of-energy rejected transaction: " ++ show rejects) 2 (length rejects)
        assertEqual
            "transactions used the expected amount of energy"
            (totMintCost + totTransferCost - 1 + totBurnCost - 1)
            srUsedEnergy
        doBlockStateAssertions

    it "1 transaction containing 2 operations, energy suffices only for the first" $ do
        let maxBlockEnergy = 10_000
        -- only enough energy for one transfer
        ts <- Runner.processUngroupedTransactions (txs2 totTransferCost)

        (Helpers.SchedulerResult{..}, doBlockStateAssertions) <-
            Helpers.runSchedulerTest
                (testConfig maxBlockEnergy)
                (initialBlockState @'P9)
                checkState
                ts
        let Types.FilteredTransactions{..} = srTransactions
        let rejects = [res | (_, summary) <- ftAdded, res@(Types.TxReject Types.OutOfEnergy) <- [summary ^. Types.summaryResult]]

        assertEqual "should be 2 transaction" 2 (length $ Types.perAccountTransactions ts)
        assertEqual ("should contain 1 out-of-energy rejected transaction: " ++ show rejects) (length rejects) 1
        assertEqual
            "transaction used the expected amount of energy"
            (totMintCost + totTransferCost)
            srUsedEnergy
        doBlockStateAssertions
  where
    txs1 e1 e2 e3 =
        [ Runner.TJSON
            { payload =
                Runner.TokenUpdate
                    { tuTokenId = tokenId,
                      tuOperations = encodeTxGV mintTx
                    },
              metadata =
                makeDummyHeader
                    (accountAddressFromSeed 0)
                    1
                    e1,
              keys = [(0, [(0, keyPairFromSeed 0)])]
            },
          Runner.TJSON
            { payload =
                Runner.TokenUpdate
                    { tuTokenId = tokenId,
                      tuOperations = encodeTxTH transferTx
                    },
              metadata =
                makeDummyHeader
                    (accountAddressFromSeed 0)
                    2
                    e2,
              keys = [(0, [(0, keyPairFromSeed 0)])]
            },
          Runner.TJSON
            { payload =
                Runner.TokenUpdate
                    { tuTokenId = tokenId,
                      tuOperations = encodeTxGV burnTx
                    },
              metadata =
                makeDummyHeader
                    (accountAddressFromSeed 0)
                    3
                    e3,
              keys = [(0, [(0, keyPairFromSeed 0)])]
            }
        ]
    txs2 e2 =
        [ Runner.TJSON
            { payload =
                Runner.TokenUpdate
                    { tuTokenId = tokenId,
                      tuOperations = encodeTxGV mintTx
                    },
              metadata =
                makeDummyHeader
                    (accountAddressFromSeed 0)
                    1
                    (constA + constB * 97 + tokenUpdateBaseCost + tokenMintCost),
              keys = [(0, [(0, keyPairFromSeed 0)])]
            },
          Runner.TJSON
            { payload =
                Runner.TokenUpdate
                    { tuTokenId = tokenId,
                      tuOperations = encodeTxTH transferTx2
                    },
              metadata =
                makeDummyHeader
                    (accountAddressFromSeed 0)
                    2
                    e2,
              keys = [(0, [(0, keyPairFromSeed 0)])]
            }
        ]
    checkState ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState 'P9 ->
        Helpers.PersistentBSM 'P9 Assertion
    checkState result state = do
        doAssertState <- blockStateAssertions result state
        reloadedState <- Helpers.reloadBlockState state
        doAssertReloadedState <- blockStateAssertions result reloadedState
        return $ do
            doAssertState
            doAssertReloadedState

    blockStateAssertions ::
        Helpers.SchedulerResult ->
        BS.PersistentBlockState 'P9 ->
        Helpers.PersistentBSM 'P9 Assertion
    blockStateAssertions result state = do
        hashedState <- BS.hashBlockState state
        Helpers.assertBlockStateInvariants hashedState (Helpers.srExecutionCosts result)

    tokenId = TokenId "dummyToken"
    mintTx = TokenUpdateTransaction . Seq.fromList $ [mkMintOp amt10'000]
    burnTx = TokenUpdateTransaction . Seq.fromList $ [mkBurnOp amt10'000]
    transferTx =
        TokenUpdateTransaction . Seq.fromList $
            [mkTransferOp amt10'000 receiver1 Nothing]
    transferTx2 =
        TokenUpdateTransaction . Seq.fromList $
            [mkTransferOp amt10'000 receiver1 Nothing, mkTransferOp amt10'000 receiver1 Nothing]
    encodeTxTH =
        TokenParameter . SBS.toShort . tokenUpdateTransactionToBytes
    encodeTxGV =
        TokenParameter . SBS.toShort . tokenUpdateTransactionToBytes
    receiver1 = CborHolderAccount (dummyAccountAddress 0) Nothing
    amt10'000 = TokenAmount 10_000 3
    mkMintOp toMintAmount = TokenMint{..}
    mkBurnOp toBurnAmount = TokenBurn{..}
    mkTransferOp ttAmount ttRecipient ttMemo = TokenTransfer TokenTransferBody{..}

    mintPayload =
        Types.TokenUpdate
            { tuTokenId = tokenId,
              tuOperations = encodeTxGV mintTx
            }
    burnPayload =
        Types.TokenUpdate
            { tuTokenId = tokenId,
              tuOperations = encodeTxGV burnTx
            }
    transferPayload =
        Types.TokenUpdate
            { tuTokenId = tokenId,
              tuOperations = encodeTxTH transferTx
            }
    headerSize = fromIntegral Types.transactionHeaderSize
    mintSize =
        Energy $
            fromIntegral $
                headerSize
                    + thePayloadSize
                        ( payloadSize $
                            Types.encodePayload mintPayload
                        )
    burnSize =
        Energy $
            fromIntegral $
                headerSize
                    + thePayloadSize
                        ( payloadSize $
                            Types.encodePayload burnPayload
                        )

    transferSize =
        Energy $
            fromIntegral $
                headerSize
                    + thePayloadSize
                        ( payloadSize $
                            Types.encodePayload transferPayload
                        )

    totMintCost = constA + constB * mintSize + tokenUpdateBaseCost + tokenMintCost
    totTransferCost = constA + constB * transferSize + tokenUpdateBaseCost + tokenTransferCost
    totBurnCost = constA + constB * burnSize + tokenUpdateBaseCost + tokenBurnCost

    testConfig :: Types.Energy -> Helpers.TestConfig
    testConfig maxBlockEnergy =
        Helpers.defaultTestConfig
            { Helpers.tcContextState = contextState
            }
      where
        contextState =
            Helpers.defaultContextState
                { -- Set the max energy block energy to enough for 2 successful transfers.
                  EI._maxBlockEnergy = maxBlockEnergy
                }

    initialBlockState ::
        (Types.IsProtocolVersion pv, PVSupportsPLT pv) =>
        Helpers.PersistentBSM pv (BS.HashedPersistentBlockState pv)
    initialBlockState = do
        bs0 <-
            Helpers.createTestBlockStateWithAccountsM
                [Helpers.makeTestAccountFromSeed 2_000_000 0]
        (_tIx, bs1) <-
            bsoCreateToken
                (hpbsPointers bs0)
                PLTConfiguration
                    { _pltTokenId = tokenId,
                      _pltModule = TokenModuleRef $ Hash.hash "dummyModule",
                      _pltDecimals = 3
                    }
        mutTokenState <- BlockState.getMutableTokenState bs1 0
        _ <- BlockState.updateTokenState "\NUL\NULmintable" (Just "") mutTokenState
        _ <- BlockState.updateTokenState "\NUL\NULburnable" (Just "") mutTokenState
        _ <- BlockState.updateTokenState "\NUL\NULgovernanceAccount" (Just (encode (AccountIndex 0))) mutTokenState
        _ <- BlockState.updateTokenState "\NUL\NULpaused" Nothing mutTokenState
        bs2 <- bsoSetTokenState bs1 0 mutTokenState
        hashBlockState bs2

tests :: Spec
tests = describe "TokenModule" $ do
    testInitializeToken
    testExecuteTokenUpdateTransactionMintBurnPause
    testExecuteTokenUpdateTransactionTransfer
    testQueryTokenModuleState
    testTokenOutOfEnergy
