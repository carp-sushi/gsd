{-# LANGUAGE FlexibleContexts #-}

module Handlers
  ( deleteStoryHandler
  , getStoryHandler
  , insertStoryHandler
  , listStoriesHandler
  , updateStoryHandler
  ) where

import Models
import Repo
  ( deleteStory
  , getStory
  , insertStory
  , listStories
  , updateStory
  )

import Control.Monad.IO.Class (liftIO)
import Database.Persist.Sql (ConnectionPool)
import Servant

-- Get a story from the database.
getStoryHandler :: ConnectionPool -> StoryId -> Handler (Maybe StoryDto)
getStoryHandler pool storyId =
  liftIO $
    getStory pool storyId

-- Get a page of stories from the database.
listStoriesHandler :: ConnectionPool -> Maybe Int -> Maybe Int -> Handler [StoryDto]
listStoriesHandler pool maybePage maybeSize =
  liftIO $
    listStories pool (getPage maybePage) (getSize maybeSize)
  where
    getPage Nothing = 1
    getPage (Just page) = max page 1
    getSize Nothing = 10
    getSize (Just size) = max 1 (min size 100)

-- Delete a story from the database.
deleteStoryHandler :: ConnectionPool -> StoryId -> Handler NoContent
deleteStoryHandler pool storyId =
  liftIO $ do
    deleteStory pool storyId
    return NoContent

-- Validate then insert a story in the database.
insertStoryHandler :: ConnectionPool -> Story -> Handler (Maybe StoryDto)
insertStoryHandler pool story@(Story name) =
  if name == ""
    then throwError err400 {errBody = "Invalid story name"}
    else liftIO $ insertStory pool story

-- Update a story name in the database.
updateStoryHandler :: ConnectionPool -> StoryId -> Story -> Handler (Maybe StoryDto)
updateStoryHandler pool storyId story@(Story name) =
  if name == ""
    then throwError err400 {errBody = "Invalid story name"}
    else liftIO $ updateStory pool storyId story
