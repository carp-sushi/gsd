{-# LANGUAGE FlexibleContexts #-}

module Handlers
  ( deleteStoryHandler
  , getStoryHandler
  , createStoryHandler
  , listStoriesHandler
  , updateStoryHandler
  , listTasksHandler
  , getTaskHandler
  , createTaskHandler
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
import Servant
import Service
  ( StoryReader (..)
  , StoryWriter (..)
  , TaskReader (..)
  , TaskWriter (..)
  )

-- Get a story from the database.
getStoryHandler :: StoryId -> HandlerM StoryRep
getStoryHandler storyId = do
  env <- ask
  maybeStory <- liftIO $ getStory env storyId
  maybe (throwError $ Errors.notFound "Story not found") return maybeStory

-- Get a page of stories from the database.
listStoriesHandler :: Maybe Int -> Maybe Int -> HandlerM [StoryRep]
listStoriesHandler maybePage maybeSize = do
  env <- ask
  liftIO $ listStories env (getPage maybePage) (getSize maybeSize)

-- Get page number or return the first page.
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
  env <- ask
  liftIO $ deleteStory env storyId
  return NoContent

-- Validate then insert a story in the database.
createStoryHandler :: StoryReq -> HandlerM StoryRep
createStoryHandler story@(Story name) =
  if name == ""
    then throwError $ Errors.badRequest "Invalid story name"
    else createStoryHandler' story

-- Internal create story handler
createStoryHandler' :: StoryReq -> HandlerM StoryRep
createStoryHandler' story = do
  env <- ask
  liftIO $ createStory env story

-- Update a story name in the database.
updateStoryHandler :: StoryId -> StoryReq -> HandlerM StoryRep
updateStoryHandler storyId story@(Story name) =
  if name == ""
    then throwError $ Errors.badRequest "Invalid story name"
    else updateStoryHandler' storyId story

-- Internal update story handler.
updateStoryHandler' :: StoryId -> StoryReq -> HandlerM StoryRep
updateStoryHandler' storyId story = do
  env <- ask
  liftIO $ updateStory env storyId story

-- Get tasks for a story from the database.
listTasksHandler :: Maybe StoryId -> HandlerM [TaskRep]
listTasksHandler Nothing = throwError $ Errors.badRequest "Missing storyId query parameter"
listTasksHandler (Just storyId) = do
  env <- ask
  liftIO $ listTasks env storyId

-- Get a task from the database.
getTaskHandler :: TaskId -> HandlerM TaskRep
getTaskHandler taskId = do
  env <- ask
  maybeTask <- liftIO $ getTask env taskId
  maybe (throwError $ Errors.notFound "Task not found") return maybeTask

-- Insert a task in the database.
createTaskHandler :: TaskReq -> HandlerM TaskRep
createTaskHandler task@(Task _ name _) =
  if name == ""
    then throwError $ Errors.badRequest "Invalid task name"
    else createTaskHandler' task

-- Internal create task handler
createTaskHandler' :: TaskReq -> HandlerM TaskRep
createTaskHandler' task = do
  env <- ask
  maybeStory <- liftIO $ getStory env (taskStoryId task)
  if isNothing maybeStory
    then throwError $ Errors.badRequest "Invalid storyId"
    else liftIO $ createTask env task

-- Delete a task from the database.
deleteTaskHandler :: TaskId -> HandlerM NoContent
deleteTaskHandler taskId = do
  env <- ask
  liftIO $ deleteTask env taskId
  return NoContent

-- Update a task name and status in the database.
updateTaskHandler :: TaskId -> TaskReq -> HandlerM TaskRep
updateTaskHandler taskId task@(Task _ name _) = do
  if name == ""
    then throwError $ Errors.badRequest "Invalid task name"
    else updateTaskHandler' taskId task

-- Internal update task handler
updateTaskHandler' :: TaskId -> TaskReq -> HandlerM TaskRep
updateTaskHandler' taskId task = do
  env <- ask
  maybeTask <- liftIO $ getTask env taskId
  if isNothing maybeTask
    then throwError $ Errors.notFound "Task not found"
    else liftIO $ updateTask env taskId task
