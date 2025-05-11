module Config
  ( Config (..)
  , loadConfig
  ) where

import Data.Configurator
import Data.Text

-- | App config
data Config = Config
  { pgConn :: Text
  , webPort :: Int
  , poolSize :: Int
  }
  deriving (Eq, Ord, Show)

-- | Read app config from file.
loadConfig :: FilePath -> IO Config
loadConfig file = do
  cfg <- load [Required file]
  pgConn' <- require cfg "pgConn"
  webPort' <- require cfg "webPort"
  poolSize' <- require cfg "poolSize"
  return $ Config pgConn' webPort' poolSize'
