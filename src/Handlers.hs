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
  )
where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (isNothing)
import Env
import qualified Errors
import Models
import qualified Repo
import Servant

-- Get a story from the database.
getStoryHandler :: StoryId -> HandlerM StoryRep
getStoryHandler storyId = do
  (Env _ pool) <- ask
  maybeStory <- liftIO $ Repo.getStory pool storyId
  maybe (throwError $ Errors.notFound "Story not found") return maybeStory

-- Get a page of stories from the database.
listStoriesHandler :: Maybe Int -> Maybe Int -> HandlerM [StoryRep]
listStoriesHandler maybePage maybeSize = do
  (Env _ pool) <- ask
  liftIO $ Repo.listStories pool (getPage maybePage) (getSize maybeSize)

-- Get page number or return a default.
getPage :: Maybe Int -> Int
getPage Nothing = 1
getPage (Just page) = max page 1

-- Get page size or return a default.
getSize :: Maybe Int -> Int
getSize Nothing = 10
getSize (Just size) = max 1 (min size 100)

-- Delete a story from the database.
deleteStoryHandler :: StoryId -> HandlerM NoContent
deleteStoryHandler storyId = do
  (Env _ pool) <- ask
  liftIO $ Repo.deleteStory pool storyId
  return NoContent

-- Validate then insert a story in the database.
insertStoryHandler :: StoryReq -> HandlerM StoryRep
insertStoryHandler story@(Story name) =
  if name == ""
    then throwError $ Errors.badRequest "Invalid story name"
    else insertStory' story

-- Insert story helper
insertStory' :: StoryReq -> HandlerM StoryRep
insertStory' story = do
  (Env _ pool) <- ask
  liftIO $ Repo.insertStory pool story

-- Update a story name in the database.
updateStoryHandler :: StoryId -> StoryReq -> HandlerM StoryRep
updateStoryHandler storyId story@(Story name) =
  if name == ""
    then throwError $ Errors.badRequest "Invalid story name"
    else updateStory' storyId story

-- Update story helper.
updateStory' :: StoryId -> StoryReq -> HandlerM StoryRep
updateStory' storyId story = do
  (Env _ pool) <- ask
  liftIO $ Repo.updateStory pool storyId story

-- Get tasks for a story from the database.
listTasksHandler :: Maybe StoryId -> HandlerM [TaskRep]
listTasksHandler Nothing = throwError $ Errors.badRequest "Missing storyId query parameter"
listTasksHandler (Just storyId) = do
  (Env _ pool) <- ask
  liftIO $ Repo.listTasks pool storyId

-- Get a task from the database.
getTaskHandler :: TaskId -> HandlerM TaskRep
getTaskHandler taskId = do
  (Env _ pool) <- ask
  maybeTask <- liftIO $ Repo.getTask pool taskId
  maybe (throwError $ Errors.notFound "Task not found") return maybeTask

-- Insert a task in the database.
insertTaskHandler :: TaskReq -> HandlerM TaskRep
insertTaskHandler task@(Task _ name _) =
  if name == ""
    then throwError $ Errors.badRequest "Invalid task name"
    else insertTask' task

-- Insert task helper
insertTask' :: TaskReq -> HandlerM TaskRep
insertTask' task = do
  (Env _ pool) <- ask
  maybeStory <- liftIO $ Repo.getStory pool (taskStoryId task)
  if isNothing maybeStory
    then throwError $ Errors.badRequest "Invalid storyId"
    else liftIO $ Repo.insertTask pool task

-- Delete a task from the database.
deleteTaskHandler :: TaskId -> HandlerM NoContent
deleteTaskHandler taskId = do
  (Env _ pool) <- ask
  liftIO $ Repo.deleteTask pool taskId
  return NoContent

-- Update a task name and status in the database.
updateTaskHandler :: TaskId -> TaskReq -> HandlerM TaskRep
updateTaskHandler taskId task@(Task _ name _) = do
  if name == ""
    then throwError $ Errors.badRequest "Invalid task name"
    else updateTask' taskId task

-- Update task helper
updateTask' :: TaskId -> TaskReq -> HandlerM TaskRep
updateTask' taskId task = do
  (Env _ pool) <- ask
  maybeTask <- liftIO $ Repo.getTask pool taskId
  if isNothing maybeTask
    then throwError $ Errors.notFound "Task not found"
    else liftIO $ Repo.updateTask pool taskId task
