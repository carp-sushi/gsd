module Database
  ( createPool
  , runMigrations
  ) where

import Models (migrateAll)

import Control.Monad.Logger (runStderrLoggingT)
import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Persist.Sqlite (ConnectionPool, createSqlitePool, runMigration, runSqlPool)

-- Create a database connection pool.
createPool :: Text -> Int -> IO ConnectionPool
createPool dbFile poolSize =
  runStderrLoggingT $ do
    createSqlitePool (cs dbFile) poolSize

-- Run SQL migrations on a sqlite database.
runMigrations :: ConnectionPool -> IO ()
runMigrations =
  runSqlPool (runMigration migrateAll)
