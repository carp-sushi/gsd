module Main (main) where

import Config (Config (..), loadConfig)
import Data.Maybe (listToMaybe)
import qualified Database as DB
import Env (Env (..))

import App (app)
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
  pool <- DB.createPool (dbUrl config) (poolSize config)
  DB.runMigrations pool
  let port = webPort config
  putStrLn $ "Running gsd-server on port " <> show port
  Warp.run port $ app $ Env config pool
