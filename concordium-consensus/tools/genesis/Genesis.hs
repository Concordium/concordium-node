{-# LANGUAGE DeriveDataTypeable, RecordWildCards, LambdaCase, OverloadedStrings #-}
module Main where

import System.Exit
import System.Console.CmdArgs
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson
import qualified Data.Serialize as S
import Control.Monad
import System.FilePath

import Concordium.Types
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.Bakers
import Concordium.GlobalState.IdentityProviders
import Concordium.Birk.Bake
import qualified Concordium.Crypto.SignatureScheme as SigScheme
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.VRF as VRF
import qualified Concordium.ID.Account as ID

data Genesis
    = GenerateGenesisData {gdSource :: FilePath, gdOutput :: FilePath}
    | GenerateBakers {number :: Int, gdOutput :: FilePath}
    deriving (Typeable, Data)

generateGenesisData :: Genesis
generateGenesisData = GenerateGenesisData {
    gdSource = def &= typ "INFILE" &= argPos 0,
    gdOutput = def &= typ "OUTFILE" &= argPos 1
} &= help "Parse JSON genesis parameters from INFILE and write serialized genesis data to OUTFILE"
  &= explicit &= name "make-genesis"

generateBakerData :: Genesis
generateBakerData = GenerateBakers {
    number = def &= typ "NUM" &= argPos 0,
    gdOutput = def &= typDir &= opt ("." :: FilePath) &= argPos 1
} &= help "Generate baker data"
    &= details ["This generates the following files:", 
        " bakers.json: JSON encoding of the public identities of the generated bakers",
        " baker-0.dat .. : baker credentials for running consensus",
        " baker-0-acct.json .. : baker account keys"]
    &= explicit &= name "make-bakers"

mode :: Mode (CmdArgs Genesis)
mode = cmdArgsMode $ modes [generateGenesisData, generateBakerData]
    &= summary "Concordium genesis v0"
    &= help "Generate genesis data"

main :: IO ()
main = cmdArgsRun mode >>=
    \case
        GenerateGenesisData{..} -> do
            inBS <- LBS.readFile gdSource
            case eitherDecode inBS of
                Left e -> do
                    putStrLn e
                    exitFailure
                Right v ->
                    LBS.writeFile gdOutput (S.encodeLazy $ parametersToGenesisData v)
        GenerateBakers{..} ->
            if number <= 0 || number > 1000000 then do
                putStrLn "Error: NUM must be between 1 and 1000000"
                exitFailure
            else do
                bakers <- forM [0..number - 1] $ \n -> do
                    skp <- Sig.newKeyPair
                    vrfkp <- VRF.newKeyPair
                    acctkp <- Sig.newKeyPair
                    LBS.writeFile (gdOutput </> "baker-" ++ show n ++ ".dat") $ S.encodeLazy $
                        BakerIdentity (fromIntegral n) skp (Sig.verifyKey skp) vrfkp (VRF.publicKey vrfkp)
                    encodeFile (gdOutput </> "baker-" ++ show n ++ "-account.json") $
                        object [
                            "address" .= show (ID.accountAddress (Sig.verifyKey acctkp) SigScheme.Ed25519),
                            "signatureScheme" .= fromEnum SigScheme.Ed25519,
                            "signKey" .= serializeBase16 (Sig.signKey acctkp),
                            "verifyKey" .= serializeBase16 (Sig.verifyKey acctkp)
                        ]
                    return $ object [
                        "electionVerifyKey" .= serializeBase16 (VRF.publicKey vrfkp),
                        "signatureVerifyKey" .= serializeBase16 (Sig.verifyKey skp),
                        "finalizer" .= True,
                        "account" .= object [
                            "signatureScheme" .= fromEnum SigScheme.Ed25519,
                            "signatureKey" .= serializeBase16 (Sig.signKey acctkp),
                            "balance" .= (1000000 :: Integer)
                            ]
                        ]
                encodeFile (gdOutput </> "bakers.json") bakers