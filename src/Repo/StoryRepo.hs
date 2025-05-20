{-# LANGUAGE TypeApplications #-}

module Repo.StoryRepo
  ( getStory
  , insertStory
  , listStories
  , deleteStory
  , updateStory
  ) where

import Database.Esqueleto.Experimental
import Database.Models
import qualified Database.Persist.Sql as S

import Data.String.Conversions (cs)

-- Get a story by primary key.
getStory :: ConnectionPool -> StoryId -> IO (Maybe Story)
getStory pool storyId =
  runSqlPersistMPool (S.get storyId) pool

-- Insert a new story in the database.
insertStory :: ConnectionPool -> Story -> IO StoryId
insertStory pool story =
  runSqlPersistMPool (S.insert story) pool

-- List a page of stories
listStories :: ConnectionPool -> Int -> Int -> IO [Entity Story]
listStories pool page size =
  flip runSqlPersistMPool pool $
    select $ do
      s <- from $ table @Story
      limit $ fromIntegral size
      offset $ fromIntegral $ (page - 1) * size
      return s

-- Delete a story by primary key.
deleteStory :: ConnectionPool -> StoryId -> IO ()
deleteStory pool storyId =
  runSqlPersistMPool (S.delete storyId) pool

-- Update a story name in the database.
updateStory :: ConnectionPool -> StoryId -> Story -> IO ()
updateStory pool storyId (Story name) =
  flip runSqlPersistMPool pool $ do
    S.update storyId [StoryName S.=. cs name]
