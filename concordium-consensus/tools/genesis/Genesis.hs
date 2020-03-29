{-# OPTIONS_GHC -fno-cse #-}
{-# LANGUAGE
    DeriveDataTypeable,
    OverloadedStrings #-}
module Main where

import System.Exit
import System.Console.CmdArgs
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import qualified Data.Serialize as S
import Control.Monad
import Text.Printf

import Data.Text
import qualified Data.HashMap.Strict as Map
import Concordium.GlobalState.Parameters
import Concordium.Types

data Genesis
    = GenerateGenesisData {gdSource :: FilePath,
                           gdOutput :: FilePath,
                           gdIdentity :: Maybe FilePath,
                           gdCryptoParams :: Maybe FilePath,
                           gdAdditionalAccounts :: Maybe FilePath,
                           gdControlAccounts :: Maybe FilePath,
                           gdBakers :: Maybe FilePath}
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

mode :: Mode (CmdArgs Genesis)
mode = cmdArgsMode $ modes [generateGenesisData]
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
                  vAdditionalAccs <- maybeModifyValue gdAdditionalAccounts "initialAccounts" vCP
                  vAcc <- maybeModifyValue gdControlAccounts "controlAccounts" vAdditionalAccs
                  value <- maybeModifyValue gdBakers "bakers" vAcc
                  case fromJSON value of
                    Error err -> do
                      putStrLn err
                      exitFailure
                    Success params -> do
                      let genesisData = parametersToGenesisData params
                      let totalGTU = genesisTotalGTU genesisData
                      let showBalance account =
                            let balance = _accountAmount account
                            in printf "%d (= %.4f%%)" (toInteger balance) (100 * (fromIntegral balance / fromIntegral totalGTU) :: Double)
                      putStrLn "Successfully generated genesis data."
                      putStrLn $ "There are the following " ++ show (Prelude.length (genesisAccounts genesisData)) ++ " initial accounts in genesis:"
                      forM_ (genesisAccounts genesisData) $ \account ->
                        putStrLn $ "\tAccount: " ++ show (_accountAddress account) ++ ", balance = " ++ showBalance account

                      putStrLn $ "\nIn addition there are the following " ++ show (Prelude.length (genesisControlAccounts genesisData)) ++ " control accounts:"
                      forM_ (genesisControlAccounts genesisData) $ \account ->
                        putStrLn $ "\tAccount: " ++ show (_accountAddress account) ++ ", balance = " ++ showBalance account

                      LBS.writeFile gdOutput (S.encodeLazy $ genesisData)
                      putStrLn $ "Wrote genesis data to file " ++ show gdOutput
                      exitSuccess
