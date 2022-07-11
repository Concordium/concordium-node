{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | A simple benchmark for the Trie implementation.
-- This benchmark tests the performance of lookups in the AccountMap, which uses the Trie.
-- It constructs an AccountMap with many addresses and benchmarks the time to look up specific
-- addresses in the map.
module Main where

import Concordium.ID.Types
import Concordium.Types

import qualified Concordium.GlobalState.AccountMap as AM

import Control.DeepSeq
import Criterion
import Criterion.Main
import Data.Maybe
import System.Random

testAccountAddress :: Int -> AccountAddress
testAccountAddress = fst . randomAccountAddress . mkStdGen

accounts :: [(AccountAddress, AccountIndex)]
accounts = [(testAccountAddress i, fromIntegral i) | i <- [0 .. 100000]]

testAccountMap :: AM.PureAccountMap 'P4
testAccountMap = foldr (uncurry AM.insertPure) AM.empty accounts

instance NFData (AM.PureAccountMap pv) where
    rnf a = seq a ()

instance NFData AccountAddress where
    rnf a = seq a ()

main :: IO ()
main =
    defaultMain
        [ env (pure testAccountMap) $ \am0 ->
            bgroup
                "lookup"
                [ env (pure (testAccountAddress 0)) $ \addr0 ->
                    bench "Account0" $
                        whnf (\(am, addr) -> AM.lookupPure addr am == Just 0) (am0, addr0),
                  env (pure (testAccountAddress 1234)) $ \addr0 ->
                    bench "Account1234" $
                        whnf (\(am, addr) -> AM.lookupPure addr am == Just 1234) (am0, addr0),
                  env (pure (testAccountAddress 7)) $ \addr0 ->
                    bench "Account7" $
                        whnf (\(am, addr) -> AM.lookupPure addr am == Just 7) (am0, addr0),
                  env (pure (testAccountAddress 8)) $ \addr0 ->
                    bench "Account8" $
                        whnf (\(am, addr) -> AM.lookupPure addr am == Just 8) (am0, addr0),
                  env (pure (testAccountAddress 9)) $ \addr0 ->
                    bench "Account9" $
                        whnf (\(am, addr) -> AM.lookupPure addr am == Just 9) (am0, addr0),
                  env (pure (testAccountAddress 10)) $ \addr0 ->
                    bench "Account10" $
                        whnf (\(am, addr) -> AM.lookupPure addr am == Just 10) (am0, addr0),
                  env (pure (testAccountAddress (-2))) $ \addr0 ->
                    bench "Account-2" $
                        whnf (\(am, addr) -> isNothing (AM.lookupPure addr am)) (am0, addr0)
                ]
        ]
