{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Types
  ( TaskStatus (..)
  ) where

import Data.Aeson
import Database.Persist.TH
import GHC.Generics

-- Custom field type for task status.
data TaskStatus
  = Todo
  | Done
  deriving (Eq, Generic, Read, Show)

derivePersistField "TaskStatus"
instance ToJSON TaskStatus
instance FromJSON TaskStatus
