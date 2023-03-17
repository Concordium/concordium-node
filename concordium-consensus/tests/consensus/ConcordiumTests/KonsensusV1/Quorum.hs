-- |Module testing functions from the 'Concordium.KonsensusV1.Quorum' module.
module ConcordiumTests.KonsensusV1.Quorum where

import Data.Kind (Type)
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec
import Test.QuickCheck

import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.Quorum

-- |A type that is an instance of 'MonadMulticast' required for receiving a
-- 'QuorumMessage'.
-- Note. We do not test relaying of quorum messages in this test.
newtype NoMultiCastT (m :: Type -> Type) (a :: Type) = NoMultiCastT {runMyMultiCast :: m a}
    deriving (Functor, Applicative, Monad)

instance Monad m => MonadMulticast (NoMultiCastT m) where
    sendMessage _ = return ()

testReceiveQuorumMessage :: Spec
testReceiveQuorumMessage = describe "receiveQuorumMessage" $ do
    it "obsolete round" $ foo `shouldBe` Rejected
  where
    foo = Rejected

tests :: Spec
tests = describe "KonsensusV1.Quorum" $ do
    testReceiveQuorumMessage
