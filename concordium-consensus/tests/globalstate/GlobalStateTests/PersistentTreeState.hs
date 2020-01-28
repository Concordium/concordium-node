{-# LANGUAGE DerivingVia, StandaloneDeriving, MultiParamTypeClasses, GeneralizedNewtypeDeriving, UndecidableInstances, FlexibleInstances #-}
module GlobalStateTests.PersistentTreeState where

import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Persistent.TreeState
import Concordium.GlobalState.Classes
import Concordium.GlobalState.Parameters
import Concordium.GlobalState.TreeState
import Concordium.GlobalState.BlockPointer
import Concordium.GlobalState.Finalization
import Concordium.GlobalState.Persistent.LMDB
import Control.Monad.State
import Concordium.GlobalState.BlockState
import Concordium.GlobalState
import GlobalStateTests.DummyData
import Control.Monad.RWS.Strict hiding (state)
import Control.Monad.IO.Class
import Data.Time.Clock.POSIX
import Control.Monad
import Control.Monad.Identity
import Data.Proxy
import Test.QuickCheck
import Test.Hspec
import Control.Exception
import Lens.Micro.Platform
import Data.Maybe


newtype MyTreeStateMonad c g s a = MyTreeStateMonad { runMTSM :: RWST c () s IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState s)

deriving via (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))
  instance BlockStateTypes (MyTreeStateMonad c g s)

deriving via (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))
  instance (BlockStateQuery (BlockStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))) =>
    BlockStateQuery (MyTreeStateMonad c g s)

deriving via (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))
  instance (BlockStateOperations (BlockStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))) =>
    BlockStateOperations (MyTreeStateMonad c g s)

deriving via (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))
  instance (BlockStateStorage (BlockStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))) =>
    BlockStateStorage (MyTreeStateMonad c g s)

deriving via (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))
  instance (GlobalStateTypes (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))) =>
    GlobalStateTypes (MyTreeStateMonad c g s)

deriving via (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))
  instance (BlockPointerMonad (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))) =>
    BlockPointerMonad (MyTreeStateMonad c g s)

deriving via (PersistentTreeStateMonad
              PBS.PersistentBlockState
              (MyTreeStateMonad
               PBS.PersistentBlockStateContext
               (SkovPersistentData PBS.PersistentBlockState)
               (SkovPersistentData PBS.PersistentBlockState)))
  instance (MonadState
            (SkovPersistentData PBS.PersistentBlockState)
            (MyTreeStateMonad
             PBS.PersistentBlockStateContext
             (SkovPersistentData PBS.PersistentBlockState)
             (SkovPersistentData PBS.PersistentBlockState))) =>
    LMDBStoreMonad (MyTreeStateMonad
                    PBS.PersistentBlockStateContext
                    (SkovPersistentData PBS.PersistentBlockState)
                    (SkovPersistentData PBS.PersistentBlockState))

deriving via (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))
  instance (TreeStateMonad (GlobalStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO)),
            BlockStateStorage (BlockStateM c (Identity c) g (Identity g) (RWST (Identity c) () (Identity s) IO))) =>
    TreeStateMonad (MyTreeStateMonad c g s)

type Test = MyTreeStateMonad PBS.PersistentBlockStateContext (SkovPersistentData PBS.PersistentBlockState) (SkovPersistentData PBS.PersistentBlockState) ()

createGlobalState :: IO (PBS.PersistentBlockStateContext, SkovPersistentData PBS.PersistentBlockState)
createGlobalState = do
  now <- truncate <$> getPOSIXTime
  let
    n = 3
    genesis = makeGenesisData now n 1 0.5 1 dummyCryptographicParameters dummyIdentityProviders []
    state = genesisState genesis
    config = DTDBConfig defaultRuntimeParameters genesis state
  initialiseGlobalState config

destroyGlobalState :: (PBS.PersistentBlockStateContext, SkovPersistentData PBS.PersistentBlockState) -> IO ()
destroyGlobalState (c, s) = do
  shutdownGlobalState (Proxy :: Proxy DiskTreeDiskBlockConfig) c s

specifyWithGS :: String -> Test -> SpecWith (Arg (IO ()))
specifyWithGS s f = specify s $
                    bracket
                      createGlobalState
                      destroyGlobalState
                      (\(c, d) -> do
                          (ret, _, _) <- runRWST (runMTSM f) c d
                          return ret)

testEmptyGS :: Test
testEmptyGS = do
  gb <- use genesisBlockPointer
  let gbh = bpHash gb
  b <- readBlock gbh
  case b of
    Just b ->
      assert (bpHash b == gbh) $ liftIO $ successTest
    _ -> liftIO $ failTest
  fr <- readFinalizationRecord 0
  case fr of
    Just fr ->
      assert (finalizationBlockPointer fr == gbh) $ liftIO $ successTest
    _ -> liftIO $ failTest

tests :: Spec
tests = describe "GlobalState:PersistentTreeState" $ do
  specifyWithGS "empty gs wrote the genesis to disk" testEmptyGS
