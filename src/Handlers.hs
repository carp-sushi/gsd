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

import Ctx
import Models
import Repo

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Servant

-- Get a story from the database.
getStoryHandler :: StoryId -> HandlerM StoryDto
getStoryHandler storyId = do
  Ctx {pool_ = pool} <- ask
  maybeStory <- liftIO $ getStory pool storyId
  case maybeStory of
    Just story -> return story
    Nothing -> throwError err404 {errBody = "Story not found"}

-- Get a page of stories from the database.
listStoriesHandler :: Maybe Int -> Maybe Int -> HandlerM [StoryDto]
listStoriesHandler maybePage maybeSize = do
  Ctx {pool_ = pool} <- ask
  liftIO $ listStories pool (getPage maybePage) (getSize maybeSize)
  where
    getPage Nothing = 1
    getPage (Just page) = max page 1
    getSize Nothing = 10
    getSize (Just size) = max 1 (min size 100)

-- Delete a story from the database.
deleteStoryHandler :: StoryId -> HandlerM NoContent
deleteStoryHandler storyId = do
  Ctx {pool_ = pool} <- ask
  liftIO $ deleteStory pool storyId
  return NoContent

-- Validate then insert a story in the database.
insertStoryHandler :: Story -> HandlerM StoryDto
insertStoryHandler story@(Story name) =
  if name == ""
    then throwError err400 {errBody = "Invalid story name"}
    else do
      Ctx {pool_ = pool} <- ask
      liftIO $ insertStory pool story

-- Update a story name in the database.
updateStoryHandler :: StoryId -> Story -> HandlerM StoryDto
updateStoryHandler storyId story@(Story name) =
  if name == ""
    then throwError err400 {errBody = "Invalid story name"}
    else do
      Ctx {pool_ = pool} <- ask
      liftIO $ updateStory pool storyId story

-- Get tasks for a story from the database.
listTasksHandler :: Maybe StoryId -> HandlerM [TaskDto]
listTasksHandler Nothing = throwError err400 {errBody = "Missing storyId query parameter"}
listTasksHandler (Just storyId) = do
  Ctx {pool_ = pool} <- ask
  liftIO $ listTasks pool storyId

-- Get a task from the database.
getTaskHandler :: TaskId -> HandlerM TaskDto
getTaskHandler taskId = do
  Ctx {pool_ = pool} <- ask
  maybeTask <- liftIO $ getTask pool taskId
  case maybeTask of
    Just task -> return task
    Nothing -> throwError err404 {errBody = "Task not found"}

-- Insert a task in the database.
insertTaskHandler :: Task -> HandlerM TaskDto
insertTaskHandler task =
  if (taskName task) == ""
    then throwError err400 {errBody = "Invalid task name"}
    else insertTask' task

-- Insert task helper
insertTask' :: Task -> HandlerM TaskDto
insertTask' task = do
  Ctx {pool_ = pool} <- ask
  maybeStory <- liftIO $ getStory pool (taskStoryId task)
  case maybeStory of
    Nothing -> throwError err400 {errBody = "Invalid storyId"}
    Just _ -> liftIO $ insertTask pool task

-- Delete a task from the database.
deleteTaskHandler :: TaskId -> HandlerM NoContent
deleteTaskHandler taskId = do
  Ctx {pool_ = pool} <- ask
  liftIO $ deleteTask pool taskId
  return NoContent

-- Update a task name and status in the database.
updateTaskHandler :: TaskId -> Task -> HandlerM TaskDto
updateTaskHandler taskId task = do
  if (taskName task) == ""
    then throwError err400 {errBody = "Invalid task name"}
    else updateTask' taskId task

-- Update task helper
updateTask' :: TaskId -> Task -> HandlerM TaskDto
updateTask' taskId task = do
  Ctx {pool_ = pool} <- ask
  liftIO $ updateTask pool taskId task
