{-# LANGUAGE FlexibleContexts #-}

module Handlers
  ( deleteStoryHandler
  , getStoryHandler
  , insertStoryHandler
  , listStoriesHandler
  , updateStoryHandler
  , listTasksHandler
  , getTaskHandler
  , insertTaskHandler
  , deleteTaskHandler
  , updateTaskHandler
  ) where

import Auth (Account (..))
import Errors
import Models
import Repo

import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import Database.Persist.Sql
import Servant

-- Get a story from the database.
getStoryHandler :: ConnectionPool -> Account -> StoryId -> Handler StoryRep
getStoryHandler pool _ storyId = do
  maybeStory <- liftIO $ getStory pool storyId
  case maybeStory of
    Just story -> return story
    Nothing -> throwError $ notFound "Story not found"

-- Get a page of stories from the database.
listStoriesHandler :: ConnectionPool -> Account -> Maybe Int -> Maybe Int -> Handler [StoryRep]
listStoriesHandler pool account maybePage maybeSize = do
  liftIO $
    listStories pool (cs $ accountAddress account) (getPage maybePage) (getSize maybeSize)
  where
    getPage Nothing = 1
    getPage (Just p) = max p 1
    getSize Nothing = 10
    getSize (Just s) = max 1 (min s 100)

-- Delete a story from the database.
deleteStoryHandler :: ConnectionPool -> Account -> StoryId -> Handler NoContent
deleteStoryHandler pool _ storyId = do
  liftIO $ deleteStory pool storyId
  return NoContent

-- Validate then insert a story in the database.
insertStoryHandler :: ConnectionPool -> Account -> StoryReq -> Handler StoryRep
insertStoryHandler pool account req =
  if name == ""
    then throwError $ badRequest "Invalid story name"
    else liftIO $ insertStory pool story
  where
    name = storyReqName req
    address = accountAddress account
    story = Story (cs address) (cs name)

-- Update a story name in the database.
updateStoryHandler :: ConnectionPool -> Account -> StoryId -> StoryReq -> Handler StoryRep
updateStoryHandler pool account storyId req =
  if name == ""
    then throwError $ badRequest "Invalid story name"
    else updateStory' pool storyId story
  where
    name = storyReqName req
    address = accountAddress account
    story = Story (cs address) (cs name)

-- Update story helper.
updateStory' :: ConnectionPool -> StoryId -> Story -> Handler StoryRep
updateStory' pool storyId story = do
  liftIO $ updateStory pool storyId story

-- Get tasks for a story from the database.
listTasksHandler :: ConnectionPool -> Maybe StoryId -> Handler [TaskDto]
listTasksHandler _ Nothing = throwError $ badRequest "Missing storyId query parameter"
listTasksHandler pool (Just storyId) = do
  liftIO $ listTasks pool storyId

-- Get a task from the database.
getTaskHandler :: ConnectionPool -> TaskId -> Handler TaskDto
getTaskHandler pool taskId = do
  maybeTask <- liftIO $ getTask pool taskId
  case maybeTask of
    Just task -> return task
    Nothing -> throwError $ notFound "Task not found"

-- Insert a task in the database.
insertTaskHandler :: ConnectionPool -> Task -> Handler TaskDto
insertTaskHandler pool task =
  if taskName task == ""
    then throwError $ badRequest "Invalid task name"
    else insertTask' pool task

-- Insert task helper
insertTask' :: ConnectionPool -> Task -> Handler TaskDto
insertTask' pool task = do
  maybeStory <- liftIO $ getStory pool (taskStoryId task)
  case maybeStory of
    Nothing -> throwError $ badRequest "Invalid storyId"
    Just _ -> liftIO $ insertTask pool task

-- Delete a task from the database.
deleteTaskHandler :: ConnectionPool -> TaskId -> Handler NoContent
deleteTaskHandler pool taskId = do
  liftIO $ deleteTask pool taskId
  return NoContent

-- Update a task name and status in the database.
updateTaskHandler :: ConnectionPool -> TaskId -> Task -> Handler TaskDto
updateTaskHandler pool taskId task = do
  if taskName task == ""
    then throwError $ badRequest "Invalid task name"
    else updateTask' pool taskId task

-- Update task helper
updateTask' :: ConnectionPool -> TaskId -> Task -> Handler TaskDto
updateTask' pool taskId task = do
  liftIO $ updateTask pool taskId task
