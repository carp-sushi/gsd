module Main (main) where

import Config (Config (..), loadConfig)
import Ctx (Ctx (..))
import Data.Maybe (listToMaybe)
import Database (createPool, runMigrations)
import Server (app)

import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (getArgs)

-- GSD server entry point.
main :: IO ()
main = do
  args <- getArgs
  case listToMaybe args of
    Nothing -> putStrLn "Usage: gsd-server <config-file>"
    Just configFile -> startServer configFile

-- Start the server with the given configuration file.
startServer :: FilePath -> IO ()
startServer configFile = do
  config <- loadConfig configFile
  pool <- createPool (dbFile config) (poolSize config)
  runMigrations pool
  let port = webPort config
  putStrLn $ "Running gsd-server on port " <> show port
  Warp.run port $ app (Ctx pool)
