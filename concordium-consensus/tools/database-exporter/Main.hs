module Main where

import DatabaseExporter.CommandLineParser
import DatabaseExporter
import Options.Applicative
import Control.Monad.Reader

main :: IO ()
main = do
  conf <- execParser opts
  if readingMode conf then readExportedDatabaseV1 =<< initialReadingHandle (exportPath conf)
  else do
      database <- initialDatabase (dbPath conf)
      file <- initialHandle (exportPath conf)
      runReaderT exportDatabaseV1 (ReadEnv database file)
 where opts = info (config <**> helper)
        (fullDesc
          <> progDesc "Export the database of a consensus node")
