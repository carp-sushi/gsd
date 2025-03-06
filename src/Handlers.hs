{-# LANGUAGE FlexibleContexts #-}

module Handlers
  ( deleteStoryHandler
  , getStoryHandler
  , insertStoryHandler
  , listStoriesHandler
  , updateStoryHandler
  ) where

import Models
import Repo (deleteStory, getStory, insertStory, listStories, updateStory)

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Database.Persist.Sql (ConnectionPool)
import Servant

-- Get a story from the database.
getStoryHandler :: ConnectionPool -> StoryId -> Handler (Maybe Story)
getStoryHandler pool storyId =
  liftIO $
    getStory pool storyId

-- Get a page of stories from the database.
listStoriesHandler :: ConnectionPool -> Maybe Int -> Handler [Story]
listStoriesHandler pool maybePage =
  liftIO $
    listStories pool (getPage maybePage)
  where
    getPage Nothing = 1
    getPage (Just page) =
      if page < 1 then 1 else page

-- Delete a story from the database.
deleteStoryHandler :: ConnectionPool -> StoryId -> Handler NoContent
deleteStoryHandler pool storyId =
  liftIO $ do
    deleteStory pool storyId
    return NoContent

-- Validate then insert a story in the database.
insertStoryHandler :: ConnectionPool -> Story -> Handler (Maybe Story)
insertStoryHandler pool story@(Story name) =
  if name == ""
    then throwError err400 {errBody = "Invalid story name"}
    else liftIO $ insertStory pool story

-- Update a story name in the database.
updateStoryHandler :: ConnectionPool -> StoryId -> Text -> Handler (Maybe Story)
updateStoryHandler pool storyId name =
  if name == ""
    then throwError err400 {errBody = "Invalid story name"}
    else liftIO $ updateStory pool storyId name
