{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module GlobalStateTests.Accounts where

import Concordium.Crypto.DummyData
import qualified Concordium.Crypto.SHA256 as H
import qualified Concordium.Crypto.SignatureScheme as Sig
import Concordium.Crypto.FFIDataTypes
import Concordium.GlobalState.Basic.BlockState.Account as BA
import qualified Concordium.GlobalState.Basic.BlockState.AccountTable as BAT
import qualified Concordium.GlobalState.Basic.BlockState.Accounts as B
import qualified Concordium.GlobalState.Persistent.Account as PA
import qualified Concordium.GlobalState.Persistent.BlockState.AccountReleaseSchedule as PA
import qualified Concordium.GlobalState.Persistent.Accounts as P
import qualified Concordium.GlobalState.Persistent.LFMBTree as L
import Concordium.GlobalState.DummyData
import Concordium.GlobalState.Persistent.BlobStore
import qualified Concordium.GlobalState.Persistent.Trie as Trie
import Concordium.ID.DummyData
import qualified Concordium.ID.Types as ID
import Concordium.Types
import Concordium.Types.HashableTo
import Control.Exception (bracket)
import Control.Monad hiding (fail)
import Control.Monad.Trans.Reader
import Data.Either
import qualified Data.FixedByteString as FBS
import qualified Data.Map.Strict as OrdMap
import Data.Serialize as S
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Lens.Micro.Platform
import System.FilePath
import System.IO.Temp
import Test.HUnit
import Test.Hspec
import Test.QuickCheck
import Prelude hiding (fail)

import Control.Monad.IO.Class

type PV = 'P1

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
checkEquivalent :: (MonadBlobStore m, MonadFail m) => B.Accounts PV -> P.Accounts PV -> m ()
checkEquivalent ba pa = do
  pam <- Trie.toMap (P.accountMap pa)
  checkBinary (==) (B.accountMap ba) pam "==" "Basic account map" "Persistent account map"
  let bat = BAT.toList (B.accountTable ba)
  pat <- L.toAscPairList (P.accountTable pa)
  checkBinaryM sameAccList bat pat "==" "Basic account table (as list)" "Persistent account table (as list)"
  let bath = getHash (B.accountTable ba) :: H.Hash
  path <- getHashM (P.accountTable pa)
  checkBinary (==) bath path "==" "Basic account table hash" "Persistent account table hash"
  (pregids, _) <- P.loadRegIds pa
  checkBinary (==) (B.accountRegIds ba) pregids "==" "Basic registration ids" "Persistent registration ids"
  where
    -- Check whether an in-memory account-index and account pair is equivalent to a persistent account-index and account pair
    sameAccPair ::
      (MonadBlobStore m) =>
      Bool -> -- accumulator for the fold in 'sameAccList'
      ((AccountIndex, BA.Account PV), (AccountIndex, PA.PersistentAccount PV)) -> -- the pairs to be compared
      m Bool
    sameAccPair b ((bInd, bAcc), (pInd, pAcc)) = do
      sameAcc <- PA.sameAccount bAcc pAcc
      return $ b && bInd == pInd && sameAcc
    -- Check whether a list of in-memory account-index and account pairs is equivalent to a persistent list of account-index and account pairs
    sameAccList l1 l2 = foldM sameAccPair True $ zip l1 l2

data AccountAction
  = PutAccount (Account PV)
  | Exists AccountAddress
  | GetAccount AccountAddress
  | UpdateAccount AccountAddress (Account PV -> Account PV)
  | UnsafeGetAccount AccountAddress
  | RegIdExists ID.CredentialRegistrationID
  | RecordRegId ID.CredentialRegistrationID AccountIndex
  | FlushPersistent
  | ArchivePersistent

randomizeAccount :: AccountAddress -> ID.CredentialPublicKeys -> Gen (Account PV)
randomizeAccount _accountAddress _accountVerificationKeys = do
  let vfKey = snd . head $ (OrdMap.toAscList (ID.credKeys _accountVerificationKeys))
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
      credKeys <- OrdMap.fromList . zip [0 ..] . map Sig.correspondingVerifyKey <$> replicateM n genSigSchemeKeyPair
      credThreshold <- fromIntegral <$> choose (1, n)
      return (ID.CredentialPublicKeys {..}, address)
    ra _ _ 0 = return []
    ra s rids n =
      oneof $
        [ putRandAcc,
          exRandAcc,
          getRandAcc,
          (FlushPersistent :) <$> ra s rids (n -1),
          (ArchivePersistent :) <$> ra s rids (n -1),
          exRandReg,
          recRandReg,
          updateRandAcc
        ]
          ++ if null s
            then []
            else
              [putExAcc, exExAcc, getExAcc, unsafeGetExAcc, updateExAcc]
                ++ if null rids then [] else [exExReg, recExReg]
      where
        putRandAcc = do
          (vk, addr) <- randAccount
          acct <- randomizeAccount addr vk
          (PutAccount acct :) <$> ra (Set.insert (vk, addr) s) rids (n -1)
        putExAcc = do
          (vk, addr) <- elements (Set.toList s)
          acct <- randomizeAccount addr vk
          (PutAccount acct :) <$> ra s rids (n -1)
        exRandAcc = do
          (_, addr) <- randAccount
          (Exists addr :) <$> ra s rids (n -1)
        exExAcc = do
          (_, addr) <- elements (Set.toList s)
          (Exists addr :) <$> ra s rids (n -1)
        getRandAcc = do
          (_, addr) <- randAccount
          (GetAccount addr :) <$> ra s rids (n -1)
        getExAcc = do
          (_, addr) <- elements (Set.toList s)
          (GetAccount addr :) <$> ra s rids (n -1)
        updateExAcc = do
          (_, addr) <- elements (Set.toList s)
          newNonce <- Nonce <$> arbitrary
          newAmount <- Amount <$> arbitrary
          let upd acc =
                if acc ^. BA.accountAddress == addr
                  then acc {_accountAmount = newAmount, _accountNonce = newNonce}
                  else error "address does not match expected value"
          (UpdateAccount addr upd :) <$> ra s rids (n -1)
        updateRandAcc = do
          (vk, addr) <- randAccount
          let upd _ = error "account address should not exist"
          if (vk, addr) `Set.member` s
            then ra s rids n
            else (UpdateAccount addr upd :) <$> ra s rids (n -1)
        unsafeGetExAcc = do
          (_, addr) <- elements (Set.toList s)
          (UnsafeGetAccount addr :) <$> ra s rids (n -1)
        exRandReg = do
          rid <- randomCredential
          (RegIdExists rid :) <$> ra s rids (n -1)
        exExReg = do
          (rid, _) <- elements (Map.toList rids)
          (RegIdExists rid :) <$> ra s rids (n -1)
        recRandReg = do
          rid <- randomCredential
          ai <- AccountIndex <$> arbitrary
          (RecordRegId rid ai :) <$> ra s (Map.insert rid ai rids) (n -1)
        recExReg = do
          -- This is not an expected case in practice
          (rid, ai) <- elements (Map.toList rids)
          (RecordRegId rid ai :) <$> ra s rids (n -1)

makePureAccount :: forall m pv. (MonadBlobStore m, IsProtocolVersion pv) => PA.PersistentAccount pv -> m (Account pv)
makePureAccount PA.PersistentAccount {..} = do
  (_accountPersisting :: AccountPersisting pv) <- makeHashed <$> refLoad _persistingData
  _accountEncryptedAmount <- PA.loadPersistentAccountEncryptedAmount =<< loadBufferedRef _accountEncryptedAmount
  _accountReleaseSchedule <- PA.loadPersistentAccountReleaseSchedule =<< loadBufferedRef _accountReleaseSchedule
  ab <- case _accountBaker of
    Null -> return Nothing
    Some pabRef -> do
      pab <- refLoad pabRef
      abi <- refLoad (PA._accountBakerInfo pab)
      return $ Just AccountBaker {
        _stakedAmount = PA._stakedAmount pab,
        _stakeEarnings = PA._stakeEarnings pab,
        _accountBakerInfo = abi,
        _bakerPendingChange = PA._bakerPendingChange pab
      }
  return Account {_accountBaker = ab, ..}

runAccountAction :: (MonadBlobStore m, MonadIO m) => AccountAction -> (B.Accounts PV, P.Accounts PV) -> m (B.Accounts PV, P.Accounts PV)
runAccountAction (PutAccount acct) (ba, pa) = do
  let ba' = B.putAccount acct ba
  pAcct <- PA.makePersistentAccount acct
  pa' <- P.putAccount pAcct pa
  return (ba', pa')
runAccountAction (Exists addr) (ba, pa) = do
  let be = B.exists addr ba
  pe <- P.exists addr pa
  checkBinary (==) be pe "<->" "account exists in basic" "account exists in persistent"
  return (ba, pa)
runAccountAction (GetAccount addr) (ba, pa) = do
  let bacct = B.getAccount addr ba
  pacct <- P.getAccount addr pa
  let sameAcc (Just bac) (Just pac) = PA.sameAccount bac pac
      sameAcc Nothing Nothing = return True
      sameAcc _ _ = return False
  checkBinaryM sameAcc bacct pacct "==" "account in basic" "account in persistent"
  return (ba, pa)
runAccountAction (UpdateAccount addr upd) (ba, pa) = do
  let ba' = ba & ix addr %~ upd
      -- Transform a function that updates in-memory accounts into a function that updates persistent accounts
      liftP :: (MonadBlobStore m) => (Account PV -> Account PV) -> PA.PersistentAccount PV -> m (PA.PersistentAccount PV)
      liftP f pAcc = do
        bAcc <- makePureAccount pAcc
        PA.makePersistentAccount $ f bAcc
  (_, pa') <- P.updateAccounts (fmap ((),) . liftP upd) addr pa
  return (ba', pa')
runAccountAction (UnsafeGetAccount addr) (ba, pa) = do
  let bacct = B.unsafeGetAccount addr ba
  pacct <- P.unsafeGetAccount addr pa
  checkBinaryM PA.sameAccount bacct pacct "==" "account in basic" "account in persistent"
  return (ba, pa)
runAccountAction FlushPersistent (ba, pa) = do
  (_, pa') <- storeUpdate pa
  return (ba, pa')
runAccountAction ArchivePersistent (ba, pa) = do
  ppa <- store pa
  pa' <- fromRight (error "couldn't deserialize archived persistent") $ S.runGet load (S.runPut ppa)
  return (ba, pa')
runAccountAction (RegIdExists rid) (ba, pa) = do
  let be = B.regIdExists rid ba
  (pe, pa') <- P.regIdExists rid pa
  checkBinary (==) be pe "<->" "regid exists in basic" "regid exists in persistent"
  return (ba, pa')
runAccountAction (RecordRegId rid ai) (ba, pa) = do
  let ba' = B.recordRegId rid ai ba
  pa' <- P.recordRegId rid ai pa
  return (ba', pa')

emptyTest :: SpecWith BlobStore
emptyTest =
  it "empty" $
    runReaderT
      (checkEquivalent B.emptyAccounts P.emptyAccounts :: ReaderT BlobStore IO ())

actionTest :: Word -> SpecWith BlobStore
actionTest lvl = it "account actions" $ \bs -> withMaxSuccess (100 * fromIntegral lvl) $ property $ do
  acts <- randomActions
  return $ ioProperty $ flip runReaderT bs $ do
    (ba, pa) <- foldM (flip runAccountAction) (B.emptyAccounts, P.emptyAccounts) acts
    checkEquivalent ba pa

tests :: Word -> Spec
tests lvl = describe "GlobalStateTests.Accounts" $
            around (\kont ->
                      withTempDirectory "." "blockstate" $ \dir -> bracket
                        (createBlobStore (dir </> "blockstate.dat"))
                        closeBlobStore
                        kont
                   ) $ do emptyTest
                          actionTest lvl
