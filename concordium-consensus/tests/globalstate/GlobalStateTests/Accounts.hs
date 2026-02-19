{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module GlobalStateTests.Accounts where

import qualified Basic.Accounts as B
import Concordium.Crypto.DummyData
import Concordium.Crypto.FFIDataTypes
import qualified Concordium.Crypto.SignatureScheme as Sig
import qualified Concordium.GlobalState.AccountMap as AccountMap
import qualified Concordium.GlobalState.AccountMap.DifferenceMap as DiffMap
import qualified Concordium.GlobalState.AccountMap.LMDB as LMDBAccountMap
import Concordium.GlobalState.Basic.BlockState.Account as BA
import Concordium.GlobalState.BlockState (AccountsHash)
import Concordium.GlobalState.DummyData
import qualified Concordium.GlobalState.Persistent.Account as PA
import qualified Concordium.GlobalState.Persistent.Accounts as P
import Concordium.GlobalState.Persistent.BlobStore
import Concordium.GlobalState.Persistent.BlockState (PersistentBlockStateContext (..))
import qualified Concordium.GlobalState.Persistent.BlockState.Modules as M
import qualified Concordium.GlobalState.Persistent.LFMBTree as L
import Concordium.ID.DummyData
import qualified Concordium.ID.Types as ID
import Concordium.Logger
import Concordium.Types
import Concordium.Types.HashableTo
import Concordium.Types.Option (Option (..))
import Control.Exception (bracket)
import Control.Monad
import Control.Monad.Reader
import Data.Either
import qualified Data.FixedByteString as FBS
import qualified Data.HashMap.Strict as HM
import Data.IORef
import Data.List (sortOn)
import qualified Data.Map.Strict as Map
import Data.Serialize as S
import qualified Data.Set as Set
import Lens.Micro.Platform
import System.FilePath
import System.IO.Temp
import Test.HUnit
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (fail)

type PV = 'P6

newtype NoLoggerT m a = NoLoggerT {runNoLoggerT :: m a}
    deriving (Functor, Applicative, Monad, MonadIO, MonadReader r, MonadFail)

instance (Monad m) => MonadLogger (NoLoggerT m) where
    logEvent _ _ _ = return ()
    logEventIO = return $ \_ _ _ -> return ()

assertRight :: Either String a -> Assertion
assertRight (Left e) = assertFailure e
assertRight _ = return ()

checkBinary :: (Show a, MonadIO m) => (a -> a -> Bool) -> a -> a -> String -> String -> String -> m ()
checkBinary bop x y sbop sx sy = liftIO $ unless (bop x y) $ assertFailure $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ sy ++ " (" ++ show y ++ ")"

checkBinaryM :: (Monad m, Show a, Show b, MonadIO m) => (a -> b -> m Bool) -> a -> b -> String -> String -> String -> m ()
checkBinaryM bop x y sbop sx sy = do
    satisfied <- bop x y
    unless satisfied $ liftIO $ assertFailure $ "Not satisfied: " ++ sx ++ " (" ++ show x ++ ") " ++ sbop ++ " " ++ show y ++ " (" ++ sy ++ ")"

-- | Check that a 'B.Accounts' and a 'P.Accounts' are equivalent.
--  That is, they have the same account map, account table, and set of
--  use registration ids.
checkEquivalent :: (P.SupportsPersistentAccount PV m, av ~ AccountVersionFor PV) => B.Accounts PV -> P.Accounts PV -> m ()
checkEquivalent ba pa = do
    addrsAndIndices <- P.allAccounts pa
    checkBinary (==) (AccountMap.toMapPure (B.accountMap ba)) (Map.fromList addrsAndIndices) "==" "Basic account map" "Persistent account map"
    let bat = B.accountList ba
    pat <- L.toAscPairList (P.accountTable pa)
    bpat <- mapM (_2 PA.toTransientAccount) pat
    checkBinary (==) bat bpat "==" "Basic account table (as list)" "Persistent account table (as list)"
    let bath = getHash ba :: AccountsHash PV
    path <- getHashM pa
    checkBinary (==) bath path "==" "Basic accounts hash" "Persistent accounts hash"
    pregids <- P.loadRegIds pa
    checkBinary (==) (B.accountRegIds ba) pregids "==" "Basic registration ids" "Persistent registration ids"

data AccountAction
    = PutAccount (Account (AccountVersionFor PV))
    | Exists AccountAddress
    | GetAccount AccountAddress
    | UpdateAccount AccountAddress (Account (AccountVersionFor PV) -> Account (AccountVersionFor PV))
    | RegIdExists ID.CredentialRegistrationID
    | RecordRegId ID.CredentialRegistrationID AccountIndex
    | FlushPersistent
    | ArchivePersistent
    | Reconstruct

randomizeAccount :: AccountAddress -> ID.CredentialPublicKeys -> Gen (Account (AccountVersionFor PV))
randomizeAccount _accountAddress _accountVerificationKeys = do
    let vfKey = snd $ Map.findMin (ID.credKeys _accountVerificationKeys)
    let cred = dummyCredential dummyCryptographicParameters _accountAddress vfKey dummyMaxValidTo dummyCreatedAt
    let a0 = newAccount dummyCryptographicParameters _accountAddress cred
    nonce <- Nonce <$> arbitrary
    amt <- Amount <$> arbitrary
    return $ a0 & accountNonce .~ nonce & accountAmount .~ amt

randomCredential :: Gen ID.CredentialRegistrationID
randomCredential = ID.RegIdCred . generateGroupElementFromSeed dummyCryptographicParameters <$> arbitrary

randomActions :: Gen [AccountAction]
randomActions = sized (ra Set.empty Map.empty)
  where
    randAccount = do
        address <- ID.AccountAddress . FBS.pack <$> vector ID.accountAddressSize
        n <- choose (1, 255)
        credKeys <- Map.fromList . zip [0 ..] . map Sig.correspondingVerifyKey <$> replicateM n genSigSchemeKeyPair
        credThreshold <- fromIntegral <$> choose (1, n)
        return (ID.CredentialPublicKeys{..}, address)
    ra _ _ 0 = return []
    ra s rids n =
        oneof $
            [ putRandAcc,
              exRandAcc,
              getRandAcc,
              (FlushPersistent :) <$> ra s rids (n - 1),
              (ArchivePersistent :) <$> ra s rids (n - 1),
              exRandReg,
              recRandReg,
              updateRandAcc,
              (Reconstruct :) <$> ra s rids (n - 1)
            ]
                ++ if null s
                    then []
                    else
                        [exExAcc, getExAcc, updateExAcc]
                            ++ if null rids then [] else [exExReg, recExReg]
      where
        fresh x
            | x `Set.member` (Set.map snd s) = fresh . snd =<< randAccount
            | otherwise = return x
        putRandAcc = do
            (vk, addr) <- randAccount
            freshAddr <- fresh addr
            acct <- randomizeAccount freshAddr vk
            (PutAccount acct :) <$> ra (Set.insert (vk, addr) s) rids (n - 1)
        exRandAcc = do
            (_, addr) <- randAccount
            (Exists addr :) <$> ra s rids (n - 1)
        exExAcc = do
            (_, addr) <- elements (Set.toList s)
            (Exists addr :) <$> ra s rids (n - 1)
        getRandAcc = do
            (_, addr) <- randAccount
            (GetAccount addr :) <$> ra s rids (n - 1)
        getExAcc = do
            (_, addr) <- elements (Set.toList s)
            (GetAccount addr :) <$> ra s rids (n - 1)
        updateExAcc = do
            (_, addr) <- elements (Set.toList s)
            newNonce <- Nonce <$> arbitrary
            newAmount <- Amount <$> arbitrary
            let upd acc =
                    if acc ^. BA.accountAddress == addr
                        then acc{_accountAmount = newAmount, _accountNonce = newNonce}
                        else error "address does not match expected value"
            (UpdateAccount addr upd :) <$> ra s rids (n - 1)
        updateRandAcc = do
            (vk, addr) <- randAccount
            let upd _ = error "account address should not exist"
            if (vk, addr) `Set.member` s
                then ra s rids n
                else (UpdateAccount addr upd :) <$> ra s rids (n - 1)
        exRandReg = do
            rid <- randomCredential
            (RegIdExists rid :) <$> ra s rids (n - 1)
        exExReg = do
            (rid, _) <- elements (Map.toList rids)
            (RegIdExists rid :) <$> ra s rids (n - 1)
        recRandReg = do
            rid <- randomCredential
            ai <- AccountIndex <$> arbitrary
            (RecordRegId rid ai :) <$> ra s (Map.insert rid ai rids) (n - 1)
        recExReg = do
            -- This is not an expected case in practice
            (rid, ai) <- elements (Map.toList rids)
            (RecordRegId rid ai :) <$> ra s rids (n - 1)

runAccountAction :: (P.SupportsPersistentAccount PV m, av ~ AccountVersionFor PV) => AccountAction -> (B.Accounts PV, P.Accounts PV) -> m (B.Accounts PV, P.Accounts PV)
runAccountAction (PutAccount acct) (ba, pa) = do
    let ba' = B.putNewAccount acct ba
    pAcct <- PA.makePersistentAccount acct
    pa' <- P.mkNewChildDifferenceMap pa
    pa'' <- P.putNewAccount pAcct pa'
    return (snd ba', snd pa'')
runAccountAction (Exists addr) (ba, pa) = do
    let be = B.exists addr ba
    pe <- P.exists addr pa
    checkBinary (==) be pe "<->" "account exists in basic" "account exists in persistent"
    return (ba, pa)
runAccountAction (GetAccount addr) (ba, pa) = do
    let bacct = B.getAccount addr ba
    pacct <- P.getAccount addr pa
    bpacct <- mapM PA.toTransientAccount pacct
    checkBinary (==) bacct bpacct "==" "account in basic" "account in persistent"
    return (ba, pa)
runAccountAction (UpdateAccount addr upd) (ba, pa) = do
    let ba' = ba & ix addr %~ upd
        -- Transform a function that updates in-memory accounts into a function that updates persistent accounts
        liftP :: (MonadBlobStore m) => (Account (AccountVersionFor PV) -> Account (AccountVersionFor PV)) -> PA.PersistentAccount (AccountVersionFor PV) -> m (PA.PersistentAccount (AccountVersionFor PV))
        liftP f pAcc = do
            bAcc <- PA.toTransientAccount pAcc
            PA.makePersistentAccount $ f bAcc
    (_, pa') <- P.updateAccounts (fmap ((),) . liftP upd) addr pa
    return (ba', pa')
runAccountAction FlushPersistent (ba, pa) = do
    (_, pa') <- storeUpdate pa
    void $ P.writeAccountsCreated pa'
    return (ba, pa')
runAccountAction ArchivePersistent (ba, pa) = do
    ppa <- fst <$> storeUpdate pa
    void $ P.writeAccountsCreated pa
    pa' <- fromRight (error "couldn't deserialize archived persistent") $ S.runGet load (S.runPut ppa)
    return (ba, pa')
runAccountAction (RegIdExists rid) (ba, pa) = do
    let be = B.regIdExists rid ba
    pe <- P.regIdExists rid pa
    checkBinary (==) be pe "<->" "regid exists in basic" "regid exists in persistent"
    return (ba, pa)
runAccountAction (RecordRegId rid ai) (ba, pa) = do
    let ba' = B.recordRegId (ID.toRawCredRegId rid) ai ba
    pa' <- P.recordRegId rid ai pa
    return (ba', pa')
runAccountAction Reconstruct (ba, pa) = do
    oPaDiffMap <- liftIO $ readIORef $ P.accountDiffMapRef pa
    -- Get the parent difference map reference and a list of accounts of the current difference map.
    (parentDiffMapRef, diffMapAccs) <- case oPaDiffMap of
        Absent -> do
            ref <- liftIO DiffMap.newEmptyReference
            return (ref, [])
        Present paDiffMap -> do
            let ref = DiffMap.dmParentMapRef paDiffMap
                -- Note that we sort them by ascending account index such that the order
                -- matches the insertion order.
                accs = map snd $ sortOn fst $ HM.elems $ DiffMap.dmMap paDiffMap
            return (ref, accs)
    -- create pa' which is the same as pa, but with an empty difference map.
    emptyRef <- liftIO DiffMap.newEmptyReference
    let pa' = pa{P.accountDiffMapRef = emptyRef}
    -- reconstruct pa into pa'.
    void $ P.reconstructDifferenceMap parentDiffMapRef diffMapAccs pa'
    return (ba, pa')

emptyTest :: SpecWith (PersistentBlockStateContext PV)
emptyTest =
    it "empty" $ \bs ->
        runNoLoggerT $
            flip runBlobStoreT bs $ do
                emptyPersistentAccs <- P.emptyAccounts
                (checkEquivalent B.emptyAccounts emptyPersistentAccs :: BlobStoreT (PersistentBlockStateContext PV) (NoLoggerT IO) ())

actionTest :: Word -> SpecWith (PersistentBlockStateContext PV)
actionTest lvl = it "account actions" $ \bs -> withMaxSuccess (100 * fromIntegral lvl) $ property $ do
    acts <- randomActions
    return $ ioProperty $ runNoLoggerT $ flip runBlobStoreT bs $ do
        emptyPersistentAccs <- P.emptyAccounts
        (ba, pa) <- foldM (flip runAccountAction) (B.emptyAccounts, emptyPersistentAccs) acts
        checkEquivalent ba pa

tests :: Word -> Spec
tests lvl = describe "GlobalStateTests.Accounts"
    $ around
        ( \kont ->
            withTempDirectory "." "blockstate" $ \dir ->
                bracket
                    ( do
                        pbscBlobStore <- createBlobStore (dir </> "blockstate.dat")
                        pbscAccountCache <- PA.newAccountCache 100
                        pbscModuleCache <- M.newModuleCache 100
                        pbscAccountMap <- LMDBAccountMap.openDatabase (dir </> "accountmap")
                        return PersistentBlockStateContext{..}
                    )
                    ( \PersistentBlockStateContext{..} -> do
                        closeBlobStore pbscBlobStore
                        LMDBAccountMap.closeDatabase pbscAccountMap
                    )
                    kont
        )
    $ do
        emptyTest
        actionTest lvl
