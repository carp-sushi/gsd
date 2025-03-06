{-# LANGUAGE DataKinds #-}
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
{-# LANGUAGE UndecidableInstances #-}

module Models where

import Types (TaskStatus)

import Data.Aeson
import Data.Text (Text)
import Database.Persist.Sql
import Database.Persist.TH

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
Story json sql=stories
  name String
  deriving Eq Read Show
Task json sql=tasks
  storyId StoryId
  name String
  status TaskStatus default=Todo
  deriving Eq Read Show
|]

-- Story data transfer object.
-- We use this so we can pair the primary key with the story name.
data StoryDto = StoryDto
  { storyId_ :: StoryId
  , storyName_ :: String
  }
  deriving (Eq, Ord, Show)

-- Render data transfer object as JSON.
instance ToJSON StoryDto where
  toJSON (StoryDto storyId name) =
    object
      [ "id" .= toJSON storyId
      , "name" .= toJSON name
      ]

-- Create a story data transfer object from a database entity.
mkStoryDto :: Entity Story -> StoryDto
mkStoryDto (Entity storyId (Story name)) =
  StoryDto storyId name
