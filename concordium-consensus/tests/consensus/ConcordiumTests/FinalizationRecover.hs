{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module ConcordiumTests.FinalizationRecover where

import Control.Monad
import qualified Data.Vector as Vec
import System.FilePath
import System.IO.Temp

import Concordium.Afgjort.Finalize
import Concordium.Birk.Bake
import Concordium.Logger
import Concordium.Skov.MonadImplementations
import Concordium.Startup
import Concordium.Types.ProtocolVersion

import Concordium.GlobalState
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Block
import qualified Concordium.GlobalState.DummyData as Dummy
import Concordium.GlobalState.Parameters
import Concordium.Types
import Concordium.Types.AnonymityRevokers
import Concordium.Types.HashableTo
import Concordium.Types.IdentityProviders

import Test.Hspec

-- Test that 'recoverFinalizationState' recovers the initial finalization state
-- when initialized from genesis state.

-- |Protocol version
type PV = 'P1

dummyArs :: AnonymityRevokers
dummyArs = emptyAnonymityRevokers

type TreeConfig = DiskTreeDiskBlockConfig

makeGlobalStateConfig :: FilePath -> RuntimeParameters -> TreeConfig
makeGlobalStateConfig tempDir rt = DTDBConfig rt tempDir (tempDir </> "data" <.> "blob")

genesis :: Word -> (GenesisData PV, [(BakerIdentity, FullBakerInfo)], Amount)
genesis nBakers =
    makeGenesisData
        0
        nBakers
        1000
        defaultFinalizationParameters{finalizationAllowZeroDelay = False}
        Dummy.dummyCryptographicParameters
        emptyIdentityProviders
        dummyArs
        []
        1234
        Dummy.dummyKeyCollection
        Dummy.dummyChainParameters

makeFinalizationInstance :: BakerIdentity -> FinalizationInstance
makeFinalizationInstance bid = FinalizationInstance (bakerSignKey bid) (bakerElectionKey bid) (bakerAggregationKey bid)

setup :: Word -> IO [(FinalizationState (), FinalizationState ())]
setup nBakers = withTempDirectory "." "tmp-consensus-data" $ \tempDir -> do
    let (genData, bakers, genTotal) = genesis nBakers
    let fbi = Vec.fromList (snd <$> bakers)
    let fullBakers =
            FullBakers
                { fullBakerInfos = fbi,
                  bakerTotalStake = sum (_bakerStake <$> fbi)
                }
    let initialState inst =
            initialFinalizationState
                inst
                (getHash (GenesisBlock (genesisConfiguration genData)))
                (gdFinalizationParameters genData)
                fullBakers
                genTotal
    let initialPassiveState =
            initialPassiveFinalizationState
                (getHash (GenesisBlock (genesisConfiguration genData)))
                (gdFinalizationParameters genData)
                fullBakers
                genTotal
    let finInstances = map (makeFinalizationInstance . fst) bakers
    (gsc, gss) <- runSilentLogger (initialiseGlobalState genData $ makeGlobalStateConfig tempDir defaultRuntimeParameters)
    active <- forM finInstances (\inst -> (initialState inst,) <$> runSilentLogger (getFinalizationState (Just inst) gsc gss))
    passive <- (initialPassiveState,) <$> runSilentLogger (getFinalizationState Nothing gsc gss)
    return $ passive : active
  where
    getFinalizationState :: Maybe FinalizationInstance -> GSContext PV -> GSState PV -> LogIO (FinalizationState timer)
    getFinalizationState mInst = evalGlobalState (recoverFinalizationState mInst)

test :: Spec
test = describe "Concordium.FinalizationRecover" $ do
    pairs <- runIO (setup 10)
    forM_ (zip [(0 :: Int) ..] pairs) $ \(n, (pureState, goalState)) ->
        -- instance 0 is passive, others are active
        specify ("Instance = " ++ show n) $ pureState `shouldBe` goalState
