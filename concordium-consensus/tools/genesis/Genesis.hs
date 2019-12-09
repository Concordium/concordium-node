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

import Data.Text
import qualified Data.HashMap.Strict as Map
import Concordium.GlobalState.Parameters

data Genesis
    = GenerateGenesisData {gdSource :: FilePath,
                           gdOutput :: FilePath,
                           gdIdentity :: Maybe FilePath,
                           gdCryptoParams :: Maybe FilePath,
                           gdBetaAccounts :: Maybe FilePath,
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
