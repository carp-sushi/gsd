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

module Database.Models where

import Data.Aeson
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
  name String sqltype="TEXT"
  deriving Eq Read Show
Task json sql=tasks
  storyId StoryId
  name String sqltype="TEXT"
  status TaskStatus default='Todo' sqltype="TEXT"
  deriving Eq Read Show
|]

-- Alias story request type.
type StoryReq = Story

-- Alias task request type.
type TaskReq = Task

-- Story data transfer object.
-- We use this so we can pair the primary key with the story name.
data StoryRep = StoryRep
  { storyId_ :: StoryId
  , storyName_ :: String
  }
  deriving (Eq, Ord, Show)

-- Render data transfer object as JSON.
instance ToJSON StoryRep where
  toJSON (StoryRep storyId name) =
    object
      [ "id" .= toJSON storyId
      , "name" .= toJSON name
      ]

-- Create a story data transfer object from a database entity.
mkStoryRep :: Entity Story -> StoryRep
mkStoryRep (Entity storyId (Story name)) =
  StoryRep storyId name

-- Task data transfer object.
data TaskRep = TaskRep
  { taskId_ :: TaskId
  , taskName_ :: String
  , taskStatus_ :: TaskStatus
  }
  deriving (Eq, Ord, Show)

-- Render data transfer object as JSON.
instance ToJSON TaskRep where
  toJSON (TaskRep taskId name status) =
    object
      [ "id" .= toJSON taskId
      , "name" .= toJSON name
      , "status" .= toJSON status
      ]

-- Create a task data transfer object from a database entity.
mkTaskRep :: Entity Task -> TaskRep
mkTaskRep (Entity taskId (Task _ name status)) =
  TaskRep taskId name status
