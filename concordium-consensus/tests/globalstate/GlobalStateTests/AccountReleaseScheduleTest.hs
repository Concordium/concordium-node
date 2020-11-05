{-# LANGUAGE DerivingVia, UndecidableInstances, DeriveGeneric, OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module GlobalStateTests.AccountReleaseScheduleTest where

import Test.Hspec
import Concordium.GlobalState.Persistent.BlobStore
import Control.Monad.Trans.Reader
import System.IO.Temp
import Control.Exception
import System.FilePath
import Concordium.GlobalState.BlockState
import qualified Data.Map.Strict as OrdMap
import Test.QuickCheck
import Control.Monad
import Concordium.Types
import Concordium.GlobalState
import Concordium.GlobalState.AccountTransactionIndex
import Concordium.GlobalState.AnonymityRevokers
import Concordium.GlobalState.BlockPointer hiding (BlockPointer)
import Concordium.GlobalState.DummyData
import Concordium.GlobalState.IdentityProviders
import Concordium.GlobalState.Parameters
import qualified Concordium.GlobalState.Persistent.BlockState as PBS
import Concordium.GlobalState.Persistent.TreeState
import Concordium.Logger
import Control.Monad.Identity
import Control.Monad.RWS.Strict as RWS hiding (state)
import Data.Proxy
import Data.Time.Clock.POSIX
import Lens.Micro.Platform
import Concordium.GlobalState.Paired
import Concordium.GlobalState.Basic.TreeState
import Concordium.GlobalState.Basic.BlockState as BS
import Concordium.GlobalState.Basic.BlockState.Accounts
import Concordium.GlobalState.Basic.BlockState.Account
import Data.List
import Data.Word
import Data.IORef
import Data.Maybe
import qualified  Control.Monad.Reader.Class as R

import qualified Concordium.GlobalState.Basic.BlockState.AccountTable as AT
import Data.Foldable
import Concordium.GlobalState.Basic.BlockState.AccountReleaseSchedule
import Data.Aeson (eitherDecode)

type ThisMonadConcrete = BlockStateM
                (PairGSContext () PBS.PersistentBlockStateContext)
                (Identity (PairGSContext () PBS.PersistentBlockStateContext))
                (PairGState
                   (SkovData HashedBlockState)
                   (SkovPersistentData () PBS.HashedPersistentBlockState))
                (Identity
                   (PairGState
                      (SkovData HashedBlockState)
                      (SkovPersistentData () PBS.HashedPersistentBlockState)))
                (RWST
                   (Identity (PairGSContext () PBS.PersistentBlockStateContext))
                   ()
                   (Identity
                      (PairGState
                         (SkovData HashedBlockState)
                         (SkovPersistentData () PBS.HashedPersistentBlockState)))
                   LogIO)

dummyTransactionHash :: TransactionHash
dummyTransactionHash = case eitherDecode "\"f26a45adbb7d5cbefd9430d1eac665bd225fb3d8e04efb288d99a0347f0b8868\"" of
  Left e -> error e
  Right v -> v

tests :: Spec
tests = describe "GlobalState.AccountReleaseScheduleTest"
  $ specify "correct releases"
  $ withTempDirectory "." "test-directory"
  $ \dbDir ->
      bracket (do
                  now <- utcTimeToTimestamp <$> getCurrentTime
                  let
                    n = 3
                    genesis = makeTestingGenesisData now n 1 1 dummyFinalizationCommitteeMaxSize dummyCryptographicParameters emptyIdentityProviders emptyAnonymityRevokers maxBound dummyAuthorizations dummyChainParameters
                    state = basicGenesisState genesis
                    rp = defaultRuntimeParameters { rpTreeStateDir = dbDir, rpBlockStateFile = dbDir </> "blockstate" }
                    config = PairGSConfig (MTMBConfig rp genesis state, DTDBConfig rp genesis state)
                  (x, y, (NoLogContext, NoLogContext)) <- runSilentLogger $ initialiseGlobalState config
                  return (Identity x, Identity y)) (\(c, s) -> shutdownGlobalState (Proxy :: Proxy (PairGSConfig MemoryTreeMemoryBlockConfig DiskTreeDiskBlockConfig)) (runIdentity c) (runIdentity s) (NoLogContext, NoLogContext)) $
        runSilentLogger . void . uncurry (runRWST (runBlockStateM testing))
  where testing :: ThisMonadConcrete ()
        testing = do
          let numTxs = (fmap (map getPositive) . generate . vectorOf 2 $ arbitrary) :: IO [Int]
              numTimestamps = mapM (\x -> fmap (map getPositive) . generate . vectorOf x $ arbitrary) :: [Int] -> IO [[Int]]
              txs = mapM (mapM (\x -> sortOn fst . map (\(a, b) -> (Timestamp a, b `mod` 200)) <$> (generate . vectorOf x $ arbitrary))) :: [[Int]] -> IO [[[(Timestamp, Amount)]]]
          -- generate [[(t1, a1), (t2, a2)..], [(s1, b1), (s2, b2), ...]] the two lists with the random releases to be added with t{i+1} > ti, and ai < 200.
          ~[txsAB, txsBA] <- liftIO $ txs =<< numTimestamps =<< numTxs
          bs <- get
          let -- get the blockstates
              bs1 = _unhashedBlockState $ _bpState (bs ^. pairStateLeft . Concordium.GlobalState.Basic.TreeState.focusBlock)
              bs2 = PBS.hpbsPointers $ _bpState (bs ^. pairStateRight . Concordium.GlobalState.Persistent.TreeState.focusBlock)
              -- find two accounts, just the first two
              ~a@[(accA, _), (accB, _)] = take 2 $ map (\(_, ac) -> (_accountAddress $ _accountPersisting ac, _accountAmount ac)) $ AT.toList $ accountTable (bs1 ^. blockAccounts)
              -- create the list of release schedules for each account
              rsA = map (\e -> [(accA, e)]) txsBA :: [[(AccountAddress, [(Timestamp, Amount)])]]
              rsB = map (\e -> [(accB, e)]) txsAB :: [[(AccountAddress, [(Timestamp, Amount)])]]
              -- create a list with all the moments when an amount should be released annotated with the account and sorted by timestamp
              sortedA = map (\(t, b) -> (accA, t, b)) $ sortOn fst $ concat txsBA
              sortedB = map (\(t, b) -> (accB, t, b)) $ sortOn fst $ concat txsAB
              allSorted = sortOn (\(_, x, _) -> x) (sortedA ++ sortedB)

          -- add all the releases
          bs' <- foldlM (\b e -> do
                           b' <- bsoAddReleaseSchedule b [(fst $ head e, minimum $ map fst $ snd $ head e)]
                           b'' <- bsoModifyAccount b' ((emptyAccountUpdate (fst $ head e)) { _auReleaseSchedule = Just [(snd $ head e, dummyTransactionHash)] })
                           checkB b'' b e
                           return b') (bs1, bs2) (rsA ++ rsB)

          times <- sort . map Timestamp <$> liftIO (generate $ listOf1 $ choose (tsMillis (snd' $ head allSorted) `div` 2, tsMillis (snd' $ last allSorted) * 2) :: IO [Word64])
          let times' = zip (0 : init times) times

          -- remove releases at random times

          _ <- foldlM (\b (prevTime, thisTime) -> do
                            b' <- bsoProcessReleaseSchedule b thisTime
                            checkP b' b (takeWhile ((< thisTime) . snd') $ dropWhile ((< prevTime) . snd') allSorted) thisTime
                            return b') bs' times'
          return ()

fst' (x, _, _) = x
snd' (_, x, _) = x
thd' (_, _, x) = x

-- | Check the blockstate when removing items
checkP :: (BS.BlockState, PBS.PersistentBlockState) -> (BS.BlockState, PBS.PersistentBlockState) -> [(AccountAddress, Timestamp, Amount)] -> Timestamp -> ThisMonadConcrete ()
checkP (newB, newP) (oldB, _) removedNow thisTime = do
  let brs = _blockReleaseSchedule newB
  -- we only have keys that are past this time
  assert (all (> thisTime) $ OrdMap.elems brs) $ return ()
  -- check that the persistent version is the same as the basic one
  ctx <- PBS.pbscBlobStore . _pairContextRight <$> R.ask
  brsP <- liftIO $ runReaderT (do
                                 newP' <- loadBufferedRef =<< liftIO (readIORef newP)
                                 loadBufferedRef $ PBS.bspReleaseSchedule newP') ctx
  assert (brs == brsP) $ return ()

  -- the items removed are the ones that should have been removed
  let items = map (\l -> (fst' $ head l, concatMap (\(a, b, c) -> [(b, c)]) l)) $ groupBy (\a b -> fst' a == fst' b) $ sortOn fst' removedNow -- convert from [(Addr, Timestamp, Amount)] to [(Addr, [(Timestamp, Amount)])]

  assert (all (\(addr, l) -> case (Concordium.GlobalState.Basic.BlockState.Accounts.getAccount addr (newB ^. blockAccounts), Concordium.GlobalState.Basic.BlockState.Accounts.getAccount addr (oldB ^. blockAccounts)) of
                              (Just acc', Just acc) -> let (rels', rels) = (_accountReleaseSchedule acc', _accountReleaseSchedule acc)
                                                      in
                                                        -- the sum of stakes is equal to the total
                                                        (sum (OrdMap.map fst (rels' ^. pendingReleases)) == rels' ^. totalLockedUpBalance) &&
                                                        -- this chunk of amounts is the difference in amounts between blockstates
                                                        sum (map snd l) == rels' ^. totalLockedUpBalance + rels ^. totalLockedUpBalance &&
                                                        all (> thisTime) (OrdMap.keys (rels' ^. pendingReleases)) &&
                                                        -- the items have been removed
                                                        rels ^. pendingReleases == foldl' (\m (t, v) -> OrdMap.insert t (v, [dummyTransactionHash]) m) (rels' ^. pendingReleases) l &&
                                                        all (\(t, _) -> isNothing (OrdMap.lookup t (rels' ^. pendingReleases))) l
                              _ -> False) items) $ return ()


-- | Check the blockstate when adding items
checkB :: (BS.BlockState, PBS.PersistentBlockState) -> (BS.BlockState, PBS.PersistentBlockState) -> [(AccountAddress, [(Timestamp, Amount)])] -> ThisMonadConcrete ()
checkB (newB, newP) (oldB, _) e = do
  let brs = _blockReleaseSchedule newB
      oldbrs = _blockReleaseSchedule oldB
  -- check that the blockstate map has the items
  assert (all (\(a, l) -> case OrdMap.lookup a brs of
                           Just v -> minimum (map fst l) == v
                           Nothing -> False) e) $ return ()
  -- check that the persistent version is the same as the basic one
  ctx <- PBS.pbscBlobStore . _pairContextRight <$> R.ask
  brsP <- liftIO $ runReaderT (do
                                 newP' <- loadBufferedRef =<< liftIO (readIORef newP)
                                 loadBufferedRef $ PBS.bspReleaseSchedule newP') ctx
  assert (brs == brsP) $ return ()
  -- check that the account has the items
  assert (all (\(addr, l) -> case (Concordium.GlobalState.Basic.BlockState.Accounts.getAccount addr (newB ^. blockAccounts), Concordium.GlobalState.Basic.BlockState.Accounts.getAccount addr (oldB ^. blockAccounts)) of
                              (Just acc', Just acc) -> let (rels', rels) = (_accountReleaseSchedule acc', _accountReleaseSchedule acc)
                                                      in
                                                        -- the sum of stakes is equal to the total
                                                        (sum (OrdMap.map fst (rels' ^. pendingReleases)) == rels' ^. totalLockedUpBalance) &&
                                                        -- this chunk of amounts is the difference in amounts between blockstates
                                                        sum (map snd l) == rels' ^. totalLockedUpBalance - rels ^. totalLockedUpBalance &&
                                                        -- the amount on the account has been added as expected
                                                        acc' ^. accountAmount - rels' ^. totalLockedUpBalance == acc ^. accountAmount &&
                                                        -- the items are present or added up
                                                        all (\(t, v) -> case (OrdMap.lookup t (rels' ^. pendingReleases), OrdMap.lookup t (rels ^. pendingReleases)) of
                                                                         (Just v', Nothing) -> v' == (v, [dummyTransactionHash])
                                                                         (Just (v', txh'), Just (v'', txh'')) -> v == v' - v'' && dummyTransactionHash:txh'' == txh'
                                                                         _ -> False) l
                              _ -> False) e) $ return ()
