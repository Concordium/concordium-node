{-# LANGUAGE RecordWildCards, OverloadedStrings, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
module GlobalStateTests.DummyData where

import qualified Data.Map.Strict as OrdMap
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.Ed25519Signature as Ed25519
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.SeedState
import Concordium.GlobalState.IdentityProviders
import Concordium.Types
import Concordium.ID.Types(randomAccountAddress, makeSingletonAC)
import qualified Data.PQueue.Prio.Max as Queue
import Test.Hspec
import Data.FixedByteString as FBS
import System.Random
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.Ed25519Signature as Ed25519
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.Crypto.SHA256 as Hash
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.SeedState
import qualified Data.Hashable as IntHash
import Concordium.GlobalState.IdentityProviders
import Concordium.Types
import Data.Time.Clock.POSIX
import System.IO.Unsafe
import qualified Concordium.ID.Types as ID
import Concordium.Types.Transactions
import qualified Data.ByteString.Short as BSS
import qualified Concordium.GlobalState.Basic.BlockState as Basic

dummyRegId :: AccountAddress -> ID.CredentialRegistrationID
dummyRegId addr = ID.RegIdCred . FBS.pack $ bytes
  where bytes = take (FBS.fixedLength (undefined :: ID.RegIdSize)) . randoms . mkStdGen $ IntHash.hash addr

{-# WARNING dummyCredential "Invalid credential, only for testing." #-}
dummyCredential :: ID.AccountAddress -> ID.CredentialExpiryTime -> ID.CredentialDeploymentValues
dummyCredential address pExpiry  = ID.CredentialDeploymentValues
    {
      cdvAccount = ID.ExistingAccount address,
      cdvRegId = dummyRegId address,
      cdvIpId = ID.IP_ID 0,
      cdvThreshold = ID.Threshold 2,
      cdvArData = [],
      cdvPolicy = ID.Policy {
        pAttributeListVariant = 0,
        pItems = OrdMap.empty,
        ..
        },
      ..
    }

{-# WARNING dummyExpiryTime "Invalid expiry time, only for testing." #-}
dummyExpiryTime :: ID.CredentialExpiryTime
dummyExpiryTime = maxBound

genesisState :: GenesisData -> Basic.BlockState
genesisState genData = Basic.initialState
                       (genesisBirkParameters genData)
                       (genesisCryptographicParameters genData)
                       (genesisAccounts genData ++ genesisSpecialBetaAccounts genData)
                       (genesisIdentityProviders genData)
                       (genesisMintPerSlot genData)

mateuszKP :: SigScheme.KeyPair
mateuszKP = (\((Sig.KeyPair a b),_) -> SigScheme.KeyPairEd25519 a b) $ Sig.randomKeyPair (mkStdGen 0)

kp :: Int -> Sig.KeyPair
kp n = fst (Sig.randomKeyPair (mkStdGen n))

proofKP :: Int -> VRF.KeyPair
proofKP n = fst (VRF.randomKeyPair (mkStdGen n))

dummyCryptographicParameters :: CryptographicParameters
dummyCryptographicParameters =
  case unsafePerformIO (readCryptographicParameters <$> BSL.readFile "testdata/global.json") of
    Nothing -> error "Could not read cryptographic parameters."
    Just params -> params

dummyIdentityProviders :: [IpInfo]
dummyIdentityProviders = []

makeBakerAccount :: BakerId -> Account
makeBakerAccount bid =
    acct {_accountAmount = 1000000000000,
          _accountStakeDelegate = Just bid,
          _accountCredentials = credentialList}
  where
    vfKey = SigScheme.correspondingVerifyKey kp
    credentialList = Queue.singleton dummyExpiryTime (dummyCredential address dummyExpiryTime)
    acct = newAccount (makeSingletonAC vfKey) address
    -- NB the negation makes it not conflict with other fake accounts we create elsewhere.
    seed = - (fromIntegral bid) - 1
    (address, seed') = randomAccountAddress (mkStdGen seed)
    kp = uncurry SigScheme.KeyPairEd25519 $ fst (Ed25519.randomKeyPair seed')

makeBakers :: Word -> [(BakerInfo, Account)]
makeBakers nBakers = take (fromIntegral nBakers) $ mbs (mkStdGen 17) 0
    where
        mbs gen bid = (BakerInfo epk spk blspk stake accAddress, account):mbs gen''' (bid+1)
            where
                (ek@(VRF.KeyPair _ epk), gen') = VRF.randomKeyPair gen
                (sk, gen'') = Sig.randomKeyPair gen'
                spk = Sig.verifyKey sk
                (blssk, gen''') = Bls.randomSecretKey gen''
                blspk = Bls.derivePublicKey blssk
                accAddress = _accountAddress account
                stake = _accountAmount account
                account = makeBakerAccount bid


makeGenesisData ::
    Timestamp -- ^Genesis time
    -> Word  -- ^Initial number of bakers.
    -> Duration  -- ^Slot duration in seconds.
    -> ElectionDifficulty  -- ^Initial election difficulty.
    -> BlockHeight -- ^Minimum finalization interval - 1
    -> CryptographicParameters -- ^Initial cryptographic parameters.
    -> [IpInfo]   -- ^List of initial identity providers.
    -> [Account]  -- ^List of starting genesis special accounts (in addition to baker accounts).
    -> GenesisData
makeGenesisData genesisTime nBakers genesisSlotDuration elecDiff finMinSkip genesisCryptographicParameters genesisIdentityProviders genesisSpecialBetaAccounts
    = GenesisData{..}
    where
        genesisMintPerSlot = 10 -- default value, OK for testing.
        genesisBakers = fst (bakersFromList bakers)
        genesisBirkParameters =
            BirkParameters elecDiff -- voting power
                          genesisBakers
                          genesisBakers
                          genesisBakers
                          (genesisSeedState (Hash.hash "LeadershipElectionNonce") 10) -- todo hardcoded epoch length (and initial seed)
        genesisFinalizationParameters = FinalizationParameters [VoterInfo vvk vrfk 1 vblsk | (BakerInfo vrfk vvk vblsk _ _) <- bakers] finMinSkip
        (bakers, genesisAccounts) = unzip (makeBakers nBakers)

successTest = True `shouldBe` True

failTest = False `shouldBe` True
