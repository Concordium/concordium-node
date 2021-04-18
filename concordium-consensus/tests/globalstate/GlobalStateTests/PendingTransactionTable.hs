{-# OPTIONS_GHC -Wno-deprecations #-}

module GlobalStateTests.PendingTransactionTable where

import Concordium.Types
import Data.Foldable
import Test.Hspec
import Test.QuickCheck
import qualified Data.Sequence as Seq
import qualified Data.Set as Set
import Concordium.GlobalState.TransactionTable

genNonce :: Gen Nonce
genNonce = max minNonce . Nonce <$> arbitrary

-- |Generate a strictly increasing sequence of elements.
genOrderedSeq :: Ord a => Gen a -> Gen (Seq.Seq a)
genOrderedSeq gena = sized (go Set.empty)
  where go xs n
            | n <= 0 = return $ Seq.fromList (Set.toAscList xs)
            | otherwise = do
                x <- gena
                go (Set.insert x xs) (n `div` 2)

nextFreeNonceSpec :: Nonce -> Seq.Seq Nonce -> Nonce
nextFreeNonceSpec def Seq.Empty = def
nextFreeNonceSpec def (n Seq.:<| rest)
    | n == def = nextFreeNonceSpec (n + 1) rest
    | otherwise = def

insertInOrderSpec :: Nonce -> Seq.Seq Nonce -> Maybe (Seq.Seq Nonce)
insertInOrderSpec n xs =
  let xsSet = Set.fromList (toList xs)
  in if n `Set.member` xsSet then Nothing
     else Just (Seq.fromList (Set.toAscList (n `Set.insert` xsSet)))

testNextFree :: Property
testNextFree = forAll (genOrderedSeq genNonce) $ \xs -> forAll genNonce $ \n ->
  case xs of
    Seq.Empty -> nextFreeNonceSpec n Seq.empty === nextFreeNonce n Seq.empty
    h Seq.:<| _ -> 
      let nnce = min h n
      in nextFreeNonceSpec nnce xs === nextFreeNonce nnce xs

testInsertInOrder :: Property
testInsertInOrder = forAll (genOrderedSeq genNonce) $ \xs -> forAll genNonce $ \n ->
  insertInOrderSpec n xs === insertInOrder n xs

tests :: Spec
tests = describe "Transaction Table" $ do
    it "next free nonce" $ withMaxSuccess 10000 testNextFree
    it "insert in order" $ withMaxSuccess 10000 testInsertInOrder
