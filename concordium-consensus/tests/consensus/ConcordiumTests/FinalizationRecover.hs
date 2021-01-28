{-# OPTIONS_GHC -Wno-deprecations #-}
module ConcordiumTests.FinalizationRecover where

import Control.Monad
import Data.Proxy
import Control.Monad.IO.Class
import qualified Data.Vector as Vec

import Concordium.Afgjort.Finalize
import Concordium.Birk.Bake
import Concordium.Logger
import Concordium.Skov.MonadImplementations
import Concordium.Startup

import Concordium.Types.HashableTo
import Concordium.GlobalState
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Block
import Concordium.GlobalState.Parameters
import Concordium.Types.IdentityProviders
import Concordium.Types.AnonymityRevokers
import qualified Concordium.GlobalState.DummyData as Dummy

import Test.Hspec

-- Test that 'recoverFinalizationState' recovers the initial finalization state
-- when initialized from genesis state.

dummyArs :: AnonymityRevokers
dummyArs = emptyAnonymityRevokers

-- type TreeConfig = DiskTreeDiskBlockConfig
type TreeConfig = MemoryTreeMemoryBlockConfig
makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> IO TreeConfig
makeGlobalStateConfig rt genData = return $ MTMBConfig rt genData

genesis :: Word -> (GenesisData, [(BakerIdentity, FullBakerInfo)])
genesis nBakers =
    makeGenesisData
    0
    nBakers
    1000
    defaultFinalizationParameters
    Dummy.dummyCryptographicParameters
    emptyIdentityProviders
    dummyArs
    []
    1234
    Dummy.dummyAuthorizations
    Dummy.dummyChainParameters

makeFinalizationInstance :: BakerIdentity -> FinalizationInstance
makeFinalizationInstance bid = FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid) (bakerAggregationKey bid)

setup :: Word -> IO [(FinalizationState (), FinalizationState ())]
setup nBakers = do
  let (genData@GenesisDataV2{..}, bakers) = genesis nBakers
  let fbi = Vec.fromList (snd <$> bakers)
  let fullBakers = FullBakers {
          fullBakerInfos = fbi,
          bakerTotalStake = sum (_bakerStake <$> fbi)
        }
  let initialState inst =
        initialFinalizationState
        inst
        (getHash (GenesisBlock genData))
        genesisFinalizationParameters
        fullBakers
        (genesisTotalGTU genData)
  let initialPassiveState =
        initialPassiveFinalizationState
        (getHash (GenesisBlock genData))
        genesisFinalizationParameters
        fullBakers
        (genesisTotalGTU genData)
  let finInstances = map (makeFinalizationInstance . fst) bakers
  (gsc, gss, _) <- runSilentLogger( initialiseGlobalState =<< (liftIO $ makeGlobalStateConfig defaultRuntimeParameters genData))
  active <- forM finInstances (\inst -> (initialState inst,) <$> runSilentLogger (getFinalizationState (Proxy :: Proxy TreeConfig) (gsc, gss) (Just inst)))
  passive <- (initialPassiveState,) <$> runSilentLogger (getFinalizationState (Proxy :: Proxy TreeConfig) (gsc, gss) Nothing)
  return $ passive:active

test :: Spec
test = describe "Concordium.FinalizationRecover" $ do
  pairs <- runIO (setup 10)
  forM_ (zip [(0::Int)..] pairs) $ \(n, (pureState, goalState)) ->
    -- instance 0 is passive, others are active
    specify ("Instance = " ++ show n) $ pureState `shouldBe` goalState
