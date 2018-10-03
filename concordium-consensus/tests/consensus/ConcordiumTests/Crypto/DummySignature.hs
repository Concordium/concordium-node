module ConcordiumTests.Crypto.DummySignature where

import Data.ByteString
import Data.Word
import Test.QuickCheck
import Test.Hspec

import Concordium.Crypto.DummySignature

checkSignature :: (SignKey, VerifyKey) -> Property
checkSignature (ks, kv) = property $ ck
    where
        ck :: [Word8] -> Bool
        ck doc = let doc' = pack doc in verify kv doc' (sign ks doc')

checkNoDocCollision :: (SignKey, VerifyKey) -> Property
checkNoDocCollision (ks, kv) = property $ ck
    where
        ck :: [Word8] -> [Word8] -> Property
        ck ia ib = ia /= ib ==> sign ks (pack ia) /= sign ks (pack ib)

checkNoKeyCollision :: (SignKey, VerifyKey) -> Property
checkNoKeyCollision (ks1, kv2) = property $ ck
    where
        ck :: [Word8] -> Bool
        ck doc = let doc' = pack doc in not $ verify kv2 doc' (sign ks1 doc')

tests = describe "Concordium.Crypto.DummySignature" $ do
    before (newKeypair) $ do
        it "verify agrees with sign" $ checkSignature
        it "signatures don't collide for different docs" $ checkNoDocCollision
    before (do
        (ks1, _) <- newKeypair
        (_, kv2) <- newKeypair
        return (ks1, kv2)) $
        it "verification fails if keys don't match" $ checkNoKeyCollision