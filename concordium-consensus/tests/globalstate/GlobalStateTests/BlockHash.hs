{-# OPTIONS_GHC -Wno-deprecations #-}
module GlobalStateTests.BlockHash where

import Test.Hspec

import Concordium.Crypto.VRF as VRF

import Concordium.GlobalState.Block as Block
import Concordium.GlobalState.Finalization
import Concordium.Types

import Concordium.GlobalState.BakerInfo
import qualified Concordium.Crypto.BlockSignature as Sig
import qualified Concordium.Crypto.BlsSignature as Bls
import Concordium.Types.Transactions

import Lens.Micro.Platform

import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy          as L
import           Data.ByteString               as BS
import Data.Serialize

import Data.FixedByteString as FBS
import Concordium.Crypto.SHA256 as Hash

import           System.Random
import Concordium.GlobalState.DummyData
import Concordium.Types.DummyData
import Concordium.Crypto.DummyData

-- This file contains tests relating to the construction of the blockhash.
-- Current tests verify that every element of the block which is intended to form part of the blockhash does so.
-- This is done by testing the generateHash() function and verifying that every field in the input has an influence on the output.
-- In the future tests can be added to ensure that all supported blockhash versions are correctly parsed.

-- Helper functions for stub proof generation
giveKeyPair :: VRF.KeyPair
giveKeyPair = fst (VRF.randomKeyPair (mkStdGen 1))

stringToByteString :: String -> BS.ByteString
stringToByteString input =  L.toStrict $ toLazyByteString $ stringUtf8 input

generateProofFromString :: String -> VRF.Proof
generateProofFromString  input = VRF.prove giveKeyPair (stringToByteString input)

-- Helper to generate block finalization records
genFinData :: FinalizationIndex -> BlockHash -> FinalizationProof -> BlockHeight -> BlockFinalizationData
genFinData finalizationIndex finalizationBlockPointer finalizationProof finalizationDelay
        = BlockFinalizationData FinalizationRecord{..}

-- Values for default inputs to generate hash
baker1 :: (FullBakerInfo, VRF.SecretKey, Sig.SignKey, Bls.SecretKey)
baker1 = mkFullBaker 1 1

baker2 :: (FullBakerInfo, VRF.SecretKey, Sig.SignKey, Bls.SecretKey)
baker2 = mkFullBaker 2 2

slot :: Slot
slot = 5

parent :: BlockHash
parent = dummyblockPointer

bakerid :: BakerId
bakerid = BakerId 1

bakerSVK :: BakerSignVerifyKey
bakerSVK = baker1 ^. _1 . bakerInfo . bakerSignatureVerifyKey

blockP :: BlockProof
blockP = generateProofFromString "blocProof"

nonce :: BlockNonce
nonce = generateProofFromString "BlockNonce"

blockFinData :: BlockFinalizationData
blockFinData = genFinData 1 dummyblockPointer emptyFinalizationProof 1

payload :: [BlockItem]
payload = [makeTransferTransaction (alesKP, alesAccount)  thomasAccount 20 2]

stateHash :: StateHash
stateHash = StateHashV0 (Hash (FBS.pack (Prelude.replicate 32 (fromIntegral (0 :: Word)))))

transactionH :: TransactionOutcomesHash
transactionH = TransactionOutcomesHashV0 (Hash (FBS.pack (Prelude.replicate 32 (fromIntegral (0 :: Word)))))

defaultHash :: BlockHash
defaultHash = Block.generateBlockHash slot parent bakerid bakerSVK blockP nonce blockFinData payload stateHash transactionH


tests :: Spec
tests = do
  describe "Testing all block fields modify BlockHash: " $ do 
    parallel $ do
      specify ("Slot modifies BlockHash") $ do
        let slot' = 8
            hash' = Block.generateBlockHash slot' parent bakerid bakerSVK blockP nonce blockFinData payload stateHash transactionH
        defaultHash `shouldNotBe` hash'
      
      specify ("Parent modifies BlockHash") $ do
        let parent' = defaultHash
            hash' = Block.generateBlockHash slot parent' bakerid bakerSVK blockP nonce blockFinData payload stateHash transactionH
        defaultHash `shouldNotBe` hash'

      specify ("BakerID modifies BlockHash") $ do
        let bakerid' = 8
            hash' = Block.generateBlockHash slot parent bakerid' bakerSVK blockP nonce blockFinData payload stateHash transactionH
        defaultHash `shouldNotBe` hash'

      specify ("BakerSigVerifyKey modifies BlockHash") $ do
        let bakerSVK' = baker2 ^. _1 . bakerInfo . bakerSignatureVerifyKey
            hash' = Block.generateBlockHash slot parent bakerid bakerSVK' blockP nonce blockFinData payload stateHash transactionH
        defaultHash `shouldNotBe` hash'

      specify ("blockProof modifies BlockHash") $ do
        let proof' = generateProofFromString "fakeProof"
            hash' = Block.generateBlockHash slot parent bakerid bakerSVK proof' nonce blockFinData payload stateHash transactionH
        defaultHash `shouldNotBe` hash'

      specify ("BlockNonce modifies BlockHash") $ do
        let nonce' = generateProofFromString "fakeNonce"
            hash' = Block.generateBlockHash slot parent bakerid bakerSVK blockP nonce' blockFinData payload stateHash transactionH
        defaultHash `shouldNotBe` hash'

      specify ("Verifying that all fields of BlockFinalizationData contribute to hash") $ do
        let 
          hfindata = Hash.hashLazy . runPutLazy $ put blockFinData
          blockFinData' = genFinData 2 dummyblockPointer emptyFinalizationProof 1
          blockFinData'' = genFinData 1 defaultHash emptyFinalizationProof 1
          blockFinData''' = genFinData 1 dummyblockPointer (FinalizationProof [2] Bls.emptySignature) 1
          blockFinData'''' = genFinData 1 dummyblockPointer emptyFinalizationProof 2
        hfindata `shouldNotBe` (Hash.hashLazy . runPutLazy $ put blockFinData')
        hfindata `shouldNotBe` (Hash.hashLazy . runPutLazy $ put blockFinData'')
        hfindata `shouldNotBe` (Hash.hashLazy . runPutLazy $ put blockFinData''')
        hfindata `shouldNotBe` (Hash.hashLazy . runPutLazy $ put blockFinData'''')

      -- only need to change one argument. Other tests cover that each field of findata influences hash
      specify ("BlockFinalizationData modifies BlockHash") $ do
        let blockFinData' = genFinData 2 dummyblockPointer emptyFinalizationProof 1
            hash' = Block.generateBlockHash slot parent bakerid bakerSVK blockP nonce blockFinData' payload stateHash transactionH
        defaultHash `shouldNotBe` hash'

      specify ("Payload different transaction modifies BlockHash") $ do
        let payload' = [makeTransferTransaction (alesKP, alesAccount)  thomasAccount 30 3]
            hash' = Block.generateBlockHash slot parent bakerid bakerSVK blockP nonce blockFinData payload' stateHash transactionH
        defaultHash `shouldNotBe` hash'

      specify ("Payload more transactions modifies BlockHash") $ do
        let payload' = [makeTransferTransaction (alesKP, alesAccount)  thomasAccount 20 2, makeTransferTransaction (alesKP, alesAccount)  thomasAccount 20 2]
            hash' = Block.generateBlockHash slot parent bakerid bakerSVK blockP nonce blockFinData payload' stateHash transactionH
        defaultHash `shouldNotBe` hash'

      specify ("StateHash modifies BlockHash") $ do
        let stateHash' = StateHashV0 (Hash (FBS.pack (Prelude.replicate 32 (fromIntegral (1 :: Word)))))
            hash' = Block.generateBlockHash slot parent bakerid bakerSVK blockP nonce blockFinData payload stateHash' transactionH
        defaultHash `shouldNotBe` hash'

      specify ("TransactionOutcomesHash modifies BlockHash") $ do
        let transactionH' = TransactionOutcomesHashV0 (Hash (FBS.pack (Prelude.replicate 32 (fromIntegral (1 :: Word)))))
            hash' = Block.generateBlockHash slot parent bakerid bakerSVK blockP nonce blockFinData payload stateHash transactionH'      
        defaultHash `shouldNotBe` hash'

