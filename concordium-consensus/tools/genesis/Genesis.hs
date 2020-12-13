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
import qualified Data.Vector as Vec
import Data.Foldable
import Data.Maybe

import Data.Text(Text)
import qualified Data.HashMap.Strict as Map
import Concordium.Common.Version
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Basic.BlockState.Account
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.AnonymityRevokers
import qualified Concordium.GlobalState.SeedState as SS
import Concordium.ID.Types
import Concordium.Types
import Concordium.Types.Updates

data Genesis
    = GenerateGenesisData {gdSource :: FilePath,
                           gdOutput :: FilePath,
                           gdIdentity :: Maybe FilePath,
                           gdArs :: Maybe FilePath,
                           gdCryptoParams :: Maybe FilePath,
                           gdAccounts :: Maybe FilePath,
                           gdUpdateAuthorizations :: Maybe FilePath
                           }
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
    gdAccounts = def &=
                           explicit &=
                           name "accounts" &=
                           opt (Nothing :: Maybe FilePath) &=
                           typFile &=
                           help "JSON file with initial accounts, whether they are bakers or not.",
    gdUpdateAuthorizations = def &=
                        explicit &=
                        name "update-authorizations" &=
                        opt (Nothing :: Maybe FilePath) &=
                        typFile &=
                        help "JSON file with update authorizations."
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
        die $ "Invalid version in JSON file, expected " ++ show ver ++ ", got " ++ show (vVersion v')
      else do
        let value = vValue v'
        case modifyValueWith key value obj of
          Nothing -> do
            die $ "Base value '" ++ show key ++ "' not an object."
          Just v -> return v

unwrapVersionedGenesisParameters :: Version -> Versioned Value -> IO Value
unwrapVersionedGenesisParameters ver v =
  if vVersion v /= ver then die $ "Unsupported genesis parameters version " ++ show (vVersion v)
  else return (vValue v)

expectedIpInfosVersion, expectedArInfosVersion, expectedGenesisParametersVersion, expectedCryptoParamsVersion :: Version
expectedArInfosVersion = 0
expectedIpInfosVersion = 0
expectedGenesisParametersVersion = genesisParametersVersion
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
                  vAdditionalAccs <- maybeModifyValue gdAccounts "initialAccounts" vCP
                  value <- maybeModifyValue gdUpdateAuthorizations "updateAuthorizations" vAdditionalAccs
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
                      LBS.writeFile gdOutput (S.runPutLazy $ putVersionedGenesisDataV2 genesisData)
                      putStrLn $ "Wrote genesis data to file " ++ show gdOutput
                      exitSuccess
        PrintGenesisData{..} -> do
          source <- LBS.readFile gdSource
          case S.runGetLazy getExactVersionedGenesisData source of
            Left err -> putStrLn $ "Cannot parse genesis data:" ++ err
            Right genData@GenesisDataV2{..} -> do
              putStrLn "Genesis data."
              putStrLn $ "Genesis time is set to: " ++ showTime genesisTime
              putStrLn $ "Slot duration: " ++ show (durationToNominalDiffTime genesisSlotDuration)
              putStrLn $ "Genesis nonce: " ++ show (SS.currentLeadershipElectionNonce genesisSeedState)
              putStrLn $ "Epoch length in slots: " ++ show (SS.epochLength genesisSeedState)

              let totalGTU = genesisTotalGTU genData

              putStrLn ""
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

              let ChainParameters{..} = genesisChainParameters
              putStrLn ""
              putStrLn "Chain parameters: "
              putStrLn $ "  - election difficulty: " ++ show _cpElectionDifficulty
              putStrLn $ "  - Euro per Energy rate: " ++ show _cpEuroPerEnergy
              putStrLn $ "  - microGTU per Euro rate: " ++ show _cpMicroGTUPerEuro
              putStrLn $ "  - baker extra cooldown epochs: " ++ show _cpBakerExtraCooldownEpochs
              putStrLn $ "  - maximum credential deployments per block: " ++ show _cpAccountCreationLimit
              putStrLn "  - reward parameters:"
              putStrLn "    + mint distribution:"
              putStrLn $ "      * mint rate per slot: " ++ show (_cpRewardParameters ^. mdMintPerSlot)
              putStrLn $ "      * baking reward: " ++ show (_cpRewardParameters ^. mdBakingReward)
              putStrLn $ "      * finalization reward: " ++ show (_cpRewardParameters ^. mdFinalizationReward)
              putStrLn "    + transaction fee distribution:"
              putStrLn $ "      * baker: " ++ show (_cpRewardParameters ^. tfdBaker)
              putStrLn $ "      * GAS account: " ++ show (_cpRewardParameters ^. tfdGASAccount)
              putStrLn "    + GAS rewards:"
              putStrLn $ "      * baking a block: " ++ show (_cpRewardParameters ^. gasBaker)
              putStrLn $ "      * adding a finalization proof: " ++ show (_cpRewardParameters ^. gasFinalizationProof)
              putStrLn $ "      * adding a credential deployment: " ++ show (_cpRewardParameters ^. gasAccountCreation)
              putStrLn $ "      * adding a chain update: " ++ show (_cpRewardParameters ^. gasChainUpdate)

              let foundAcc = case genesisAccounts ^? ix (fromIntegral _cpFoundationAccount) of
                    Nothing -> "INVALID (" ++ show _cpFoundationAccount ++ ")"
                    Just acc -> show (_accountAddress $ _accountPersisting acc) ++ " (" ++ show _cpFoundationAccount ++ ")"
              putStrLn $ "  - foundation account: " ++ foundAcc

              putStrLn ""
              putStrLn $ "Cryptographic parameters: " ++ show genesisCryptographicParameters

              putStrLn ""
              putStrLn $ "There are " ++ show (OrdMap.size (idProviders genesisIdentityProviders)) ++ " identity providers in genesis."

              putStrLn ""
              putStrLn $ "There are " ++ show (OrdMap.size (arRevokers genesisAnonymityRevokers)) ++ " anonymity revokers in genesis."

              let bkrs = catMaybes (_accountBaker <$> genesisAccounts)
              let bkrTotalStake = foldl' (+) 0 (_stakedAmount <$> bkrs)
              putStrLn $ "\nThere are " ++ show (length bkrs) ++ " bakers with total stake: " ++ showBalance totalGTU bkrTotalStake

              putStrLn ""
              putStrLn "Genesis accounts:"
              forM_ genesisAccounts (showAccount totalGTU)

              let Authorizations{..} = genesisAuthorizations
              putStrLn ""
              putStrLn "Genesis update authorizations:"
              putStrLn "  - public keys:"
              Vec.imapM_ (\i k -> putStrLn $ "    " ++ show i ++ ": " ++ show k) asKeys
              printAccessStructure "emergency" asEmergency
              printAccessStructure "authorization" asAuthorization
              printAccessStructure "protocol" asProtocol
              printAccessStructure "election difficulty" asParamElectionDifficulty
              printAccessStructure "euro per energy" asParamEuroPerEnergy
              printAccessStructure "microGTU per euro" asParamMicroGTUPerEuro
              printAccessStructure "foundation account" asParamFoundationAccount
              printAccessStructure "mint distribution" asParamMintDistribution
              printAccessStructure "transaction fee distribution" asParamTransactionFeeDistribution
              printAccessStructure "gas reward parameters" asParamGASRewards

  where showTime t = formatTime defaultTimeLocale rfc822DateFormat (timestampToUTCTime t)
        showBalance totalGTU balance =
            printf "%s (= %.4f%%)" (amountToString balance) (100 * (fromIntegral balance / fromIntegral totalGTU) :: Double)
        showAccount totalGTU Account{_accountPersisting=PersistingAccountData{..}, ..} = do
          putStrLn $ "  - " ++ show _accountAddress
          putStrLn $ "     * balance: " ++ showBalance totalGTU _accountAmount
          putStrLn $ "     * threshold: " ++ show (akThreshold _accountVerificationKeys)
          putStrLn $ "     * keys: "
          forM_ (OrdMap.toList (akKeys _accountVerificationKeys)) $ \(idx, k) ->
            putStrLn $ "       - " ++ show idx ++ ": " ++ show k
          forM_ _accountBaker $ \AccountBaker{_accountBakerInfo = BakerInfo{..}, ..} -> do
            putStrLn $ "     * baker:"
            putStrLn $ "       + id: " ++ show _bakerIdentity
            putStrLn $ "       + stake: " ++ showBalance totalGTU _stakedAmount
            putStrLn $ "       + election key: " ++ show _bakerElectionVerifyKey
            putStrLn $ "       + signature key: " ++ show _bakerSignatureVerifyKey
            putStrLn $ "       + aggregation key: " ++ show _bakerAggregationVerifyKey
            putStrLn $ "       + earnings are " ++ (if _stakeEarnings then "" else "not ") ++ "restaked"
        printAccessStructure n AccessStructure{..} = putStrLn $ "  - " ++ n ++ " update: " ++ show accessThreshold ++ " of " ++ show (toList accessPublicKeys)
