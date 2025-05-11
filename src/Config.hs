module Config
  ( Config (..)
  , loadConfig
  ) where

import Data.Configurator
import Data.Text

-- | App config
data Config = Config
  { dbUrl :: Text
  , webPort :: Int
  , poolSize :: Int
  }
  deriving (Eq, Ord, Show)

-- | Read app config from file.
loadConfig :: FilePath -> IO Config
loadConfig file = do
  cfg <- load [Required file]
  dbUrl' <- require cfg "dbUrl"
  webPort' <- require cfg "webPort"
  poolSize' <- require cfg "poolSize"
  return $ Config dbUrl' webPort' poolSize'
