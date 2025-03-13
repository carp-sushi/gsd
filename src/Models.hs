{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Models where

import Data.Aeson
import Data.Text (Text)
import Database.Persist.Sql
import Database.Persist.TH
import GHC.Generics

-- Custom field type for task status.
data TaskStatus = Todo | Done
  deriving (Eq, Generic, Ord, Read, Show)

derivePersistField "TaskStatus"
instance ToJSON TaskStatus
instance FromJSON TaskStatus

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Story json sql=stories
  address String
  name String
  deriving Eq Read Show
Task json sql=tasks
  storyId StoryId
  name String
  status TaskStatus default=Todo
  deriving Eq Read Show
|]

-- Story request type.
newtype StoryReq = StoryReq
  { storyReqName :: Text
  }
  deriving (Eq, Ord, Show)

instance FromJSON StoryReq where
  parseJSON = withObject "StoryReq" $ \o -> do
    name <- o .: "name"
    return $ StoryReq name

-- Story reply type.
data StoryRep = StoryRep
  { storyId_ :: StoryId
  , storyAddress_ :: String
  , storyName_ :: String
  }
  deriving (Eq, Ord, Show)

-- Render data transfer object as JSON.
instance ToJSON StoryRep where
  toJSON (StoryRep storyId address name) =
    object
      [ "id" .= toJSON storyId
      , "address" .= toJSON address
      , "name" .= toJSON name
      ]

-- Create a story data transfer object from a database entity.
mkStoryRep :: Entity Story -> StoryRep
mkStoryRep (Entity storyId (Story address name)) =
  StoryRep storyId address name

-- Task data transfer object.
data TaskDto = TaskDto
  { taskId_ :: TaskId
  , taskName_ :: String
  , taskStatus_ :: TaskStatus
  }
  deriving (Eq, Ord, Show)

-- Render data transfer object as JSON.
instance ToJSON TaskDto where
  toJSON (TaskDto taskId name status) =
    object
      [ "id" .= toJSON taskId
      , "name" .= toJSON name
      , "status" .= toJSON status
      ]

-- Create a task data transfer object from a database entity.
mkTaskDto :: Entity Task -> TaskDto
mkTaskDto (Entity taskId (Task _ name status)) =
  TaskDto taskId name status
