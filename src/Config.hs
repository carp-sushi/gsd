module Config
  ( Config (..)
  , loadConfig
  ) where

import Data.Configurator
import Data.Text

-- Service config type
data Config = Config
  { dbFile :: Text
  , webPort :: Int
  , poolSize :: Int
  }
  deriving (Eq, Ord, Show)

-- Read service config from file path.
loadConfig :: FilePath -> IO Config
loadConfig file = do
  cfg <- load [Required file]
  dbFile' <- require cfg "dbFile"
  webPort' <- require cfg "webPort"
  poolSize' <- require cfg "poolSize"
  return $ Config dbFile' webPort' poolSize'
