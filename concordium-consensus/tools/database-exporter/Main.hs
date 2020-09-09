module Main where

import DatabaseExporter.CommandLineParser
import DatabaseExporter
import Options.Applicative
import Control.Monad.Reader
import Data.Functor.Identity

main :: IO ()
main = do
  Identity conf <- execParser opts
  case conf of
    Check file -> readExportedDatabaseV1 =<< initialReadingHandle file
    Export db filepath -> do
      database <- initialDatabase db
      file <- initialHandle filepath
      runReaderT exportDatabaseV1 (ReadEnv database file)
 where opts = info (config <**> helper)
        (fullDesc
          <> progDesc "Export the database of a consensus node")
