-- |Module testing functions from the 'Concordium.KonsensusV1.Consensus.Timeout' module.
module ConcordiumTests.KonsensusV1.Timeout(tests) where

-- import Lens.Micro.Platform
import Test.HUnit
import Test.Hspec
import Data.Ratio
import Data.Word

import Concordium.Types
-- import qualified Concordium.Crypto.SHA256 as Hash
-- import Concordium.KonsensusV1.Consensus
import Concordium.KonsensusV1.Consensus.Timeout

-- |Test that 'updateCurrentTimeout' correctly calculates a new timeout given the current timeout and the
-- `timeoutIncrease` parameter.
testUpdateCurrentTimeout :: Duration -> Ratio Word64 -> Duration -> Assertion
testUpdateCurrentTimeout baseTimeout timeoutIncrease expectedTimeout  = do
    let actualTimeout = updateCurrentTimeout timeoutIncrease baseTimeout 
    assertEqual "Timeout duration should be correct" expectedTimeout actualTimeout 


tests :: Spec
tests = describe "KonsensusV1.Timeout" $ do
    it "Test updateCurrentTimeout" $ do
        testUpdateCurrentTimeout 10000 (3 % 2) 15000
        testUpdateCurrentTimeout 10000 (4 % 3) 13333
        testUpdateCurrentTimeout 10000 (5 % 3) 16666
        testUpdateCurrentTimeout 3000 (4 % 3) 4000
        testUpdateCurrentTimeout 80000 (10 % 9) 88888
        testUpdateCurrentTimeout 8000 (8 % 7) 9142