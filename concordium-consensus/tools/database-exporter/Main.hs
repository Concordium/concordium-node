module Main where

import DatabaseExporter.CommandLineParser
import DatabaseExporter hiding (exportPath)
import Options.Applicative
import Control.Monad.Reader

main :: IO ()
main = do
  conf <- execParser opts
  if readingMode conf then do
      file <- initialReadingHandle (exportPath conf)
      runReaderT readExportedDatabase (ReadEnv (dbPath conf) (exportPath conf) undefined file)
  else do
      database <- initialDatabase (dbPath conf)
      file <- initialHandle (exportPath conf)
      runReaderT exportDatabaseV0 (ReadEnv (dbPath conf) (exportPath conf) database file)
 where opts = info (config <**> helper)
        (fullDesc
          <> progDesc "Export the database of a consensus node")
