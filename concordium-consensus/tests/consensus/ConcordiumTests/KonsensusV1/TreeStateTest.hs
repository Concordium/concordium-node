module ConcordiumTests.KonsensusV1.TreeStateTest where

-- quickcheck for our property based testing.
import Test.QuickCheck

-- base types.
import Concordium.Types

-- konsensus v1 related imports.
import Concordium.KonsensusV1.Types
import Concordium.KonsensusV1.TreeState.Implementation
import ConcordiumTests.KonsensusV1.Types

-- |Create an initial 'SkovData' with a set of default parameters.
-- Note that the parameters are not particularly imporant for the tests in this module.
initialSkovData :: SkovData pv
initialSkovData = mkInitialSkovData runtimeParams genesisConfig genesisState baseTimeout leadershipElec
    runtimeParams :: RuntimeParameters
    runtimeParams = undefined
    genesisConfig :: GenesisConfiguration
    genesisConfig = undefined
    genesisState :: HashedPersistentBlockState
    genesisState = undefined
    baseTimeout :: Duration
    baseTimeout = 1_000
    leadershipElec :: LeadershipElectionNonce
    leadershipElec = undefined
    

-- |Generate a pending block signed with an arbitrary key pair.
-- and adds it to the 'SkovData'.
genPendingBlock :: SkovData -> Gen (PendingBlock, SkovData pv)
genPendingBlock = undefined

-- |Check that the provided 'PendingBlock' has been
-- added to the 'SkovData'.
checkBlockIsAdded :: (PendingBlock, SkovData pv) -> Property
checkBlockIsAdded pb sd = undefined

-- |Check that pending blocks can
-- be added to the 'TreeState'.
propAddPendingBlock :: Property
propAddPendingBlock = forAll (genPendingBlock initialSkovData) checkBlockIsAdded

tests :: Spec
tests = describe "KonsensusV1.TreeStateTest" $ do
    it "Add pending block" propAddPendingBlock

  
