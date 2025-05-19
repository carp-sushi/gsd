module Database.Migrator where

import Database.Models (migrateAll)

import Control.Monad.Logger (runNoLoggingT)
import Database.Persist.Sql (ConnectionPool, runMigrationSilent, runSqlPool)

-- Run SQL migrations on a database.
runMigrations :: ConnectionPool -> IO ()
runMigrations pool =
  runNoLoggingT $ do
    _ <- runSqlPool (runMigrationSilent migrateAll) pool
    return ()
