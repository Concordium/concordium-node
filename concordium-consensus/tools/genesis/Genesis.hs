{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, ScopedTypeVariables #-}
module Main where

import System.Exit
import System.Console.CmdArgs
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map.Strict as OrdMap
import Data.Aeson
import qualified Data.Serialize as S
import Control.Monad
import Text.Printf
import Data.Time.Format
import Lens.Micro.Platform

import Data.Text(Text)
import qualified Data.HashMap.Strict as Map
import Concordium.Common.Version
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.AnonymityRevokers
import Concordium.GlobalState.Basic.BlockState.Bakers
import qualified Concordium.GlobalState.SeedState as SS
import Concordium.ID.Types
import Concordium.Types

data Genesis
    = GenerateGenesisData {gdSource :: FilePath,
                           gdOutput :: FilePath,
                           gdIdentity :: Maybe FilePath,
                           gdArs :: Maybe FilePath,
                           gdCryptoParams :: Maybe FilePath,
                           gdAdditionalAccounts :: Maybe FilePath,
                           gdControlAccounts :: Maybe FilePath,
                           gdBakers :: Maybe FilePath}
      | PrintGenesisData { gdSource :: FilePath }
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
    gdArs = def &=
            explicit &=
            name "anonymity-revokers" &=
            opt (Nothing :: Maybe FilePath) &=
            typFile &=
            help "JSON file with anonymity revokers.",
    gdCryptoParams = def &=
                     explicit &=
                     name "crypto-params" &=
                     opt (Nothing :: Maybe FilePath) &=
                     typFile &=
                     help "JSON file with cryptographic parameters for the chain.",
    gdAdditionalAccounts = def &=
                           explicit &=
                           name "additional-accounts" &=
                           opt (Nothing :: Maybe FilePath) &=
                           typFile &=
                           help "JSON file with additional accounts (not baker, not control)",
    gdControlAccounts = def &=
                        explicit &=
                        name "control-accounts" &=
                        opt (Nothing :: Maybe FilePath) &=
                        typFile &=
                        help "JSON file with special control accounts.",
    gdBakers = def &=
               explicit &=
               name "bakers" &=
               opt (Nothing :: Maybe FilePath) &=
               typFile &=
               help "JSON file with baker information."
 } &= help "Parse JSON genesis parameters from INFILE and write serialized genesis data to OUTFILE"
  &= explicit &= name "make-genesis"

printGenesisBlock :: Genesis
printGenesisBlock = PrintGenesisData {
    gdSource = def &= typ "INFILE" &= argPos 0
 } &= help "Parse genesis data from INFILE and print it to stdout."
  &= explicit &= name "print-genesis"

mode :: Mode (CmdArgs Genesis)
mode = cmdArgsMode $ modes [generateGenesisData, printGenesisBlock]
    &= summary "Concordium genesis v1"
    &= help "Generate genesis data or display the genesis block."

modifyValueWith :: Text -> Value -> Value -> Maybe Value
modifyValueWith key val (Object obj) = Just (Object (Map.insert key val obj))
modifyValueWith _ _ _ = Nothing

maybeModifyValue :: Maybe FilePath -> Text -> Value -> IO Value
maybeModifyValue Nothing _ obj = return obj
maybeModifyValue (Just source) key obj = do
  inBS <- LBS.readFile source
  case eitherDecode inBS of
    Left e -> do
      die $ "Could not decode JSON: " ++ e
    Right v' ->
      case modifyValueWith key v' obj of
        Nothing -> do
          putStrLn "Base value not an object."
          exitFailure
        Just v -> return v


maybeModifyValueVersioned :: Version -> Maybe FilePath -> Text -> Value -> IO Value
maybeModifyValueVersioned _ Nothing _ obj = return obj
maybeModifyValueVersioned ver (Just source) key obj = do
  inBS <- LBS.readFile source
  case eitherDecode inBS of
    Left e -> do
      die $ "Could not decode JSON: " ++ e
    Right v' -> do
      if vVersion v' /= ver then do
        die $ "Invalid version in JSON file, expected " ++ (show ver) ++ ", got " ++ (show (vVersion v'))
      else do
        let value = vValue v'
        case modifyValueWith key value obj of
          Nothing -> do
            die $ "Base value '" ++ show key ++ "' not an object."
          Just v -> return v

unwrapVersionedGenesisParameters :: Version -> (Versioned Value) -> IO Value
unwrapVersionedGenesisParameters ver v = 
  if vVersion v /= ver then die $ "Unsupported genesis parameters version " ++ show (vVersion v)
  else return (vValue v)

expectedIpInfosVersion, expectedArInfosVersion, expectedGenesisParametersVersion, expectedCryptoParamsVersion :: Version
expectedArInfosVersion = 0
expectedIpInfosVersion = 0
expectedGenesisParametersVersion = 0
expectedCryptoParamsVersion = 0

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
                  g <- unwrapVersionedGenesisParameters expectedGenesisParametersVersion v
                  vId <- maybeModifyValueVersioned expectedIpInfosVersion gdIdentity "identityProviders" g
                  vAr <- maybeModifyValueVersioned expectedArInfosVersion gdArs "anonymityRevokers" vId
                  vCP <- maybeModifyValueVersioned expectedCryptoParamsVersion gdCryptoParams "cryptographicParameters" vAr
                  vAdditionalAccs <- maybeModifyValue gdAdditionalAccounts "initialAccounts" vCP
                  vAcc <- maybeModifyValue gdControlAccounts "controlAccounts" vAdditionalAccs
                  value <- maybeModifyValue gdBakers "bakers" vAcc
                  case fromJSON value of
                    Error err -> do
                      die $ "Could not decode genesis parameters: " ++ show err
                    Success params -> do
                      let genesisData = parametersToGenesisData params
                      let totalGTU = genesisTotalGTU genesisData
                      putStrLn "Successfully generated genesis data."
                      putStrLn $ "Genesis time is set to: " ++ showTime (genesisTime genesisData)
                      putStrLn $ "There are the following " ++ show (Prelude.length (genesisAccounts genesisData)) ++ " initial accounts in genesis:"
                      forM_ (genesisAccounts genesisData) $ \account ->
                        putStrLn $ "\tAccount: " ++ show (account ^. accountAddress) ++ ", balance = " ++ showBalance totalGTU (_accountAmount account)

                      putStrLn $ "\nIn addition there are the following " ++ show (Prelude.length (genesisControlAccounts genesisData)) ++ " control accounts:"
                      forM_ (genesisControlAccounts genesisData) $ \account ->
                        putStrLn $ "\tAccount: " ++ show (account ^. accountAddress) ++ ", balance = " ++ showBalance totalGTU (_accountAmount account)

                      LBS.writeFile gdOutput (S.runPutLazy $ putVersionedGenesisDataV0 genesisData)
                      putStrLn $ "Wrote genesis data to file " ++ show gdOutput
                      exitSuccess
        PrintGenesisData{..} -> do
          source <- LBS.readFile gdSource
          case S.runGetLazy getExactVersionedGenesisData source of
            Left err -> putStrLn $ "Cannot parse genesis data:" ++ err
            Right genData@GenesisData{..} -> do
              putStrLn "Genesis data."
              putStrLn $ "Genesis time is set to: " ++ showTime genesisTime
              putStrLn $ "Slot duration: " ++ show (durationToNominalDiffTime genesisSlotDuration)
              putStrLn $ "Genesis nonce: " ++ show (SS.currentSeed genesisSeedState)
              putStrLn $ "Epoch length in slots: " ++ show (SS.epochLength genesisSeedState)
              putStrLn $ "Election difficulty: " ++ show genesisElectionDifficulty

              let totalGTU = genesisTotalGTU genData

              putStrLn ""
              putStrLn $ "Mint per slot amount: " ++ show genesisMintPerSlot
              putStrLn $ "Genesis total GTU: " ++ show totalGTU
              putStrLn $ "Maximum block energy: " ++ show genesisMaxBlockEnergy

              putStrLn ""
              putStrLn "Finalization parameters: "
              let FinalizationParameters{..} = genesisFinalizationParameters
              putStrLn $ "  - minimum skip: " ++ show finalizationMinimumSkip
              putStrLn $ "  - committee max size: " ++ show finalizationCommitteeMaxSize
              putStrLn $ "  - waiting time: " ++ show (durationToNominalDiffTime finalizationWaitingTime)
              putStrLn $ "  - ignore first wait: " ++ show finalizationIgnoreFirstWait
              putStrLn $ "  - old style skip: " ++ show finalizationOldStyleSkip
              putStrLn $ "  - skip shrink factor: " ++ show finalizationSkipShrinkFactor
              putStrLn $ "  - skip grow factor: " ++ show finalizationSkipGrowFactor
              putStrLn $ "  - delay shrink factor: " ++ show finalizationDelayShrinkFactor
              putStrLn $ "  - delay grow factor: " ++ show finalizationDelayGrowFactor
              putStrLn $ "  - allow zero delay: " ++ show finalizationAllowZeroDelay

              putStrLn ""
              putStrLn $ "Cryptographic parameters: " ++ show genesisCryptographicParameters

              putStrLn ""
              putStrLn $ "There are " ++ show (Map.size (idProviders genesisIdentityProviders)) ++ " identity providers in genesis."

              putStrLn ""
              putStrLn $ "There are " ++ show (Map.size (arRevokers genesisAnonymityRevokers)) ++ " anonymity revokers in genesis."

              putStrLn $ "Genesis bakers:"
              putStrLn $ "  - bakers total stake: " ++ show (genesisBakers ^. bakerTotalStake)
              forM_ (OrdMap.toAscList (genesisBakers ^. bakerMap)) $ \(bid, FullBakerInfo{_bakerInfo = BakerInfo{..}, ..}) -> do
                putStrLn $ "  - baker: " ++ show bid
                putStrLn $ "    * stake: " ++ showBalance (genesisBakers ^. bakerTotalStake) _bakerStake
                putStrLn $ "    * account: " ++ show _bakerAccount
                putStrLn $ "    * election key: " ++ show _bakerElectionVerifyKey
                putStrLn $ "    * signature key: " ++ show _bakerSignatureVerifyKey
                putStrLn $ "    * aggregation key: " ++ show _bakerAggregationVerifyKey

              putStrLn ""
              putStrLn "Genesis normal accounts:"
              forM_ genesisAccounts (showAccount totalGTU)

              putStrLn ""
              putStrLn "Genesis control accounts:"
              forM_ genesisControlAccounts (showAccount totalGTU)

  where showTime t = formatTime defaultTimeLocale rfc822DateFormat (timestampToUTCTime t)
        showBalance totalGTU balance =
            printf "%d (= %.4f%%)" (toInteger balance) (100 * (fromIntegral balance / fromIntegral totalGTU) :: Double)
        showAccount totalGTU Account{_accountPersisting=PersistingAccountData{..}, ..} = do
          putStrLn $ "  - " ++ show _accountAddress
          putStrLn $ "     * balance: " ++ showBalance totalGTU _accountAmount
          putStrLn $ "     * threshold: " ++ show (akThreshold _accountVerificationKeys)
          putStrLn $ "     * keys: "
          forM (OrdMap.toList (akKeys _accountVerificationKeys)) $ \(idx, k) ->
            putStrLn $ "       - " ++ show idx ++ ": " ++ show k
