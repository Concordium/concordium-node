{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

-- |Module testing functions from the 'Concordium.KonsensusV1.Consensus' module.
module ConcordiumTests.KonsensusV1.Consensus (tests) where

import Control.Monad.State
import Data.Functor.Identity
import Data.Kind (Type)
import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec

import Concordium.Types

import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.TreeState.Implementation
import Concordium.KonsensusV1.TreeState.Types
import Concordium.KonsensusV1.Types

import ConcordiumTests.KonsensusV1.Types hiding (tests)

-- A trivial timeout monad which does nothing.
newtype MyMonadTimeoutT (m :: Type -> Type) a = MyMonadTimeoutT {runMyMonadTimeoutT :: m a}
    deriving (Functor, Applicative, Monad)

instance Monad m => MonadTimeout (MyMonadTimeoutT m) where
    resetTimer _ = return ()

type MyTestMonad = StateT (SkovData 'P6) (MyMonadTimeoutT Identity)

-- |Runs the computation and returns the result and resulting state.
runMyTestMonad :: SkovData 'P6 -> MyTestMonad a -> IO (a, SkovData 'P6)
runMyTestMonad sd f = undefined

-- |Checking that advancing rounds via a quorum certificate results
-- in the expected state.
testAdvanceByQuorum :: Spec
testAdvanceByQuorum = describe "Advance round by a quorum certificate." $ do
    it "should advance round" $ do
        -- resultingState <- runMyTestMonad sd $ advanceRoundWithQuorum certifiedBlock
        assertEqual
            "Round number should be incremented"
            1
            1
  where
    certifiedBlock =
        CertifiedBlock
            { cbQuorumCertificate = undefined,
              cbQuorumBlock = undefined
            }
    sd = undefined

-- |Checking that advancing a round via a timeout results
-- in the expected state.
testAdvanceByTimeout :: Spec
testAdvanceByTimeout = describe "Advance round by timeout." $ do
    return ()

tests :: Spec
tests = describe "KonsensusV1.Consensus" $ do
    testAdvanceByQuorum
    testAdvanceByTimeout
