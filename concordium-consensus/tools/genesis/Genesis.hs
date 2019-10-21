{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE DeriveDataTypeable, RecordWildCards, LambdaCase, OverloadedStrings #-}
module Main where

import System.Exit
import System.Console.CmdArgs
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import qualified Data.Serialize as S
import Control.Monad
import System.FilePath

import Data.Text
import qualified Data.HashMap.Strict as Map
import Concordium.GlobalState.Parameters
import Concordium.Birk.Bake
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.ID.Account as ID

data Genesis
    = GenerateGenesisData {gdSource :: FilePath,
                           gdOutput :: FilePath,
                           gdIdentity :: Maybe FilePath,
                           gdCryptoParams :: Maybe FilePath,
                           gdBetaAccounts :: Maybe FilePath,
                           gdBakers :: Maybe FilePath}
    | GenerateBakers {number :: Int,
                      numFinalizers :: Maybe Int,
                      gdOutput :: FilePath}
    | GenerateBetaAccounts {number :: Int,
                            gdOutput :: FilePath}
    deriving (Typeable, Data)

generateGenesisData :: Genesis
generateGenesisData = GenerateGenesisData {
    gdSource = def &= typ "INFILE" &= argPos 0,
    gdOutput = def &= typ "OUTFILE" &= argPos 1,
    gdIdentity = def &=
                 explicit &=
                 name "identity-providers" &=
                 opt (Nothing :: Maybe FilePath) &=
                 typFile &=
                 help "JSON file with identity providers.",
    gdCryptoParams = def &=
                     explicit &=
                     name "crypto-params" &=
                     opt (Nothing :: Maybe FilePath) &=
                     typFile &=
                     help "JSON file with cryptographic parameters for the chain.",
    gdBetaAccounts = def &=
                     explicit &=
                     name "beta-accounts" &=
                     opt (Nothing :: Maybe FilePath) &=
                     typFile &=
                     help "JSON file with special genesis accounts.",
    gdBakers = def &=
               explicit &=
               name "bakers" &=
               opt (Nothing :: Maybe FilePath) &=
               typFile &=
               help "JSON file with baker information."
 } &= help "Parse JSON genesis parameters from INFILE and write serialized genesis data to OUTFILE"
  &= explicit &= name "make-genesis"

generateBakerData :: Genesis
generateBakerData = GenerateBakers {
    number = def &= typ "NUM" &= argPos 0,
    numFinalizers = def &=
                    explicit &=
                    name "num-finalizers" &=
                    opt (Nothing :: Maybe Int) &=
                    typ "NUM" &=
                    help "Number of bakers which are finalizers.",
    gdOutput = def &= typDir &= opt ("." :: FilePath) &= argPos 1
} &= help "Generate baker data"
    &= details ["This generates the following files:",
        " bakers.json: JSON encoding of the public identities of the generated bakers",
        " baker-0.dat .. : baker credentials for running consensus",
        " baker-0-acct.json .. : baker account keys"]
    &= explicit &= name "make-bakers"


generateBetaAccounts :: Genesis
generateBetaAccounts = GenerateBetaAccounts {
    number = def &= typ "NUM" &= argPos 0,
    gdOutput = def &= typDir &= opt ("." :: FilePath) &= argPos 1
} &= help "Generate beta accounts"
    &= details ["This generates the following files:",
        " beta-accounts.json: JSON encoding of the public identities of the generated accounts.",
        " beta-account-0-acct.json .. : beta account private account keys"]
    &= explicit &= name "make-beta-accounts"

mode :: Mode (CmdArgs Genesis)
mode = cmdArgsMode $ modes [generateGenesisData, generateBakerData, generateBetaAccounts]
    &= summary "Concordium genesis v1"
    &= help "Generate genesis data"

modifyValueWith :: Text -> Value -> Value -> Maybe Value
modifyValueWith key val (Object obj) = Just (Object (Map.insert key val obj))
modifyValueWith _ _ _ = Nothing

maybeModifyValue :: Maybe FilePath -> Text -> Value -> IO Value
maybeModifyValue Nothing _ obj = return obj
maybeModifyValue (Just source) key obj = do
  inBS <- LBS.readFile source
  case eitherDecode inBS of
    Left e -> do
      putStrLn e
      exitFailure
    Right v' ->
      case modifyValueWith key v' obj of
        Nothing -> do
          putStrLn "Base value not an object."
          exitFailure
        Just v -> return v


main :: IO ()
main = cmdArgsRun mode >>=
    \case
        GenerateGenesisData{..} -> do
            inBS <- LBS.readFile gdSource
            case eitherDecode inBS of
                Left e -> do
                    putStrLn e
                    exitFailure
                Right v -> do
                  vId <- maybeModifyValue gdIdentity "identityProviders" v
                  vCP <- maybeModifyValue gdCryptoParams "cryptographicParameters" vId
                  vAcc <- maybeModifyValue gdBetaAccounts "betaAccounts" vCP
                  value <- maybeModifyValue gdBakers "bakers" vAcc
                  case fromJSON value of
                    Error err -> do
                      putStrLn err
                      exitFailure
                    Success params -> do
                      LBS.writeFile gdOutput (S.encodeLazy $ parametersToGenesisData params)
                      putStrLn $ "Wrote genesis data to file " ++ show gdOutput
                      exitSuccess

        GenerateBakers{..} ->
            if number <= 0 || number > 1000000 then do
                putStrLn "Error: NUM must be between 1 and 1000000"
                exitFailure
            else do
                let finalizerP n =
                      case numFinalizers of
                        Nothing -> True
                        Just num -> n < num
                bakers <- forM [0..number - 1] $ \n -> do
                    skp <- Sig.newKeyPair
                    vrfkp <- VRF.newKeyPair
                    acctkp <- Sig.newKeyPair
                    blssk <- Bls.generateSecretKey
                    -- blspk <- Bls.derivePublicKey blssk
                    LBS.writeFile (gdOutput </> "baker-" ++ show n ++ ".dat") $ S.encodeLazy $
                        BakerIdentity skp vrfkp blssk
                    encodeFile (gdOutput </> "baker-" ++ show n ++ "-account.json") $
                        object [
                            "address" .= ID.accountAddress (Sig.verifyKey acctkp) SigScheme.Ed25519,
                            "signatureScheme" .= SigScheme.Ed25519,
                            "signKey" .= Sig.signKey acctkp,
                            "verifyKey" .= Sig.verifyKey acctkp
                        ]
                    encodeFile (gdOutput </> "baker-" ++ show n ++ "-credentials.json") $
                        object [
                          "electionPrivateKey" .= VRF.privateKey vrfkp,
                          "electionVerifyKey" .= VRF.publicKey vrfkp,
                          "signatureSignKey" .= Sig.signKey skp,
                          "signatureVerifyKey" .= Sig.verifyKey skp
                          ]
                    return $ object [
                        "electionVerifyKey" .= VRF.publicKey vrfkp,
                        "signatureVerifyKey" .= Sig.verifyKey skp,
                        "finalizer" .= finalizerP n,
                        "account" .= object [
                            "signatureScheme" .= SigScheme.Ed25519,
                            "verifyKey" .= Sig.verifyKey acctkp,
                            "balance" .= (1000000000000 :: Integer)
                            ]
                        ]
                encodeFile (gdOutput </> "bakers.json") bakers
        GenerateBetaAccounts{..} -> do
          accounts <- forM [0..number-1] $ \n -> do
            acctkp <- Sig.newKeyPair
            encodeFile (gdOutput </> "beta-account-" ++ show n ++ ".json") $
                object ["address" .= ID.accountAddress (Sig.verifyKey acctkp) SigScheme.Ed25519,
                        "signatureScheme" .= SigScheme.Ed25519,
                        "signKey" .= Sig.signKey acctkp,
                        "verifyKey" .= Sig.verifyKey acctkp
                       ]
            return $ object [
              "signatureScheme" .= SigScheme.Ed25519,
              "verifyKey" .= Sig.verifyKey acctkp,
              "balance" .= (1000000000000 :: Integer)
              ]
          encodeFile (gdOutput </> "beta-accounts.json") accounts
