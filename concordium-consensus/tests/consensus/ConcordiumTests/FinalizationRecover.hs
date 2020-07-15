{-# OPTIONS_GHC -Wno-deprecations #-}
module ConcordiumTests.FinalizationRecover where

import Control.Monad
import Data.Proxy
import qualified Data.ByteString.Lazy as BSL
import System.IO.Unsafe
import Control.Monad.IO.Class

import Concordium.Afgjort.Finalize
import Concordium.Birk.Bake
import Concordium.Logger
import Concordium.Skov.MonadImplementations
import Concordium.Startup hiding (dummyCryptographicParameters)

import Concordium.Types.HashableTo
import Concordium.GlobalState
import Concordium.GlobalState.BakerInfo
import Concordium.GlobalState.Basic.BlockState.Bakers
import qualified Concordium.GlobalState.Basic.BlockState as BS
import Concordium.GlobalState.Block
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.AnonymityRevokers

import Test.Hspec

-- Test that 'recoverFinalizationState' recovers the initial finalization state
-- when initialized from genesis state.

{-# NOINLINE dummyCryptographicParameters #-}
dummyCryptographicParameters :: CryptographicParameters
dummyCryptographicParameters =
  case unsafePerformIO (readCryptographicParameters <$> BSL.readFile "../scheduler/testdata/global.json") of
    Nothing -> error "Could not read cryptographic parameters."
    Just params -> params

dummyArs :: AnonymityRevokers
dummyArs = emptyAnonymityRevokers

-- type TreeConfig = DiskTreeDiskBlockConfig
type TreeConfig = MemoryTreeMemoryBlockConfig
makeGlobalStateConfig :: RuntimeParameters -> GenesisData -> IO TreeConfig
makeGlobalStateConfig rt genData@GenesisData{..} = return $ MTMBConfig rt genData blockS
  where blockS = BS.emptyBlockState birkParams dummyCryptographicParameters
        birkParams = BS.BasicBirkParameters genesisElectionDifficulty genesisBakers genesisBakers genesisBakers genesisSeedState

genesis :: Word -> (GenesisData, [(BakerIdentity, FullBakerInfo)])
genesis nBakers =
    makeGenesisData
    0
    nBakers
    1000
    0.5
    defaultFinalizationParameters
    dummyCryptographicParameters
    emptyIdentityProviders
    dummyArs
    []
    1234

makeFinalizationInstance :: BakerIdentity -> FinalizationInstance
makeFinalizationInstance (BakerIdentity k1 k2 k3) = FinalizationInstance k1 k2 k3

setup :: Word -> IO [(FinalizationState (), FinalizationState ())]
setup nBakers = do
  let (genData@GenesisData{..}, bakers) = genesis nBakers
  let initialState inst =
        initialFinalizationState
        inst
        (getHash (GenesisBlock genData))
        genesisFinalizationParameters
        (_bakerMap genesisBakers)
        (genesisTotalGTU genData)
  let initialPassiveState =
        initialPassiveFinalizationState
        (getHash (GenesisBlock genData))
        genesisFinalizationParameters
        (_bakerMap genesisBakers)
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
