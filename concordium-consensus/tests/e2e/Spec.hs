module Main where

import Data.List (stripPrefix)
import Data.Semigroup
import qualified EndToEndTests.CredentialDeploymentTests (tests)
import System.Environment
import Test.Hspec

atLevel :: (Word -> IO ()) -> IO ()
atLevel a = do
    args0 <- getArgs
    let (args1, mlevel) = mconcat $ map lvlArg args0
    withArgs args1 $ a $! maybe 1 getLast mlevel
  where
    lvlArg s = case stripPrefix "--level=" s of
        Nothing -> ([s], Nothing)
        Just r -> ([], Just $! Last $! (read r :: Word))

main :: IO ()
main = atLevel $ \lvl -> hspec $ do
    EndToEndTests.CredentialDeploymentTests.tests lvl
