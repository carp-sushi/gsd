{-# LANGUAGE TypeApplications #-}

module Repo.StoryRepo
  ( getStory
  , insertStory
  , listStories
  , deleteStory
  , updateStory
  ) where

import Data.String.Conversions (cs)
import Database.Esqueleto.Experimental
import Database.Models
import qualified Database.Persist.Sql as S

-- Get a story by primary key.
getStory :: ConnectionPool -> StoryId -> IO (Maybe Story)
getStory pool storyId =
  flip S.runSqlPersistMPool pool $ do
    S.get storyId

-- Insert a new story in the database.
insertStory :: ConnectionPool -> StoryReq -> IO StoryId
insertStory pool story =
  flip S.runSqlPersistMPool pool $ do
    S.insert story

-- List a page of stories
listStories :: ConnectionPool -> Int -> Int -> IO [Entity Story]
listStories pool page size =
  flip S.runSqlPersistMPool pool $
    select $ do
      s <- from $ table @Story
      limit $ fromIntegral size
      offset $ fromIntegral $ (page - 1) * size
      return s

-- Delete a story by primary key.
deleteStory :: ConnectionPool -> StoryId -> IO ()
deleteStory pool storyId =
  S.runSqlPersistMPool (S.delete storyId) pool

-- Update a story name in the database.
updateStory :: ConnectionPool -> StoryId -> StoryReq -> IO ()
updateStory pool storyId (Story name) =
  flip S.runSqlPersistMPool pool $ do
    S.update storyId [StoryName S.=. cs name]
