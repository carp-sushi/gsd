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
import Errors (badRequest, notFound)
import Models
import qualified Repo

import Control.Monad.IO.Class (liftIO)
import Data.String.Conversions (cs)
import Database.Persist.Sql
import Servant

-- Get a story from the database.
getStoryHandler ::
  ConnectionPool ->
  Account ->
  StoryId ->
  Handler StoryRep
getStoryHandler pool account storyId = do
  maybeStory <- liftIO $ Repo.getStory pool storyId
  case verify account maybeStory of
    Just story -> return story
    Nothing -> throwError $ notFound "Story not found"

-- Verify story ownership.
verify ::
  Account ->
  Maybe StoryRep ->
  Maybe StoryRep
verify _ Nothing = Nothing
verify account (Just storyRep) =
  if address == accountAddress account
    then Just storyRep
    else Nothing
  where
    address = cs $ storyAddress_ storyRep

-- Get a page of stories from the database.
listStoriesHandler ::
  ConnectionPool ->
  Account ->
  Maybe Int ->
  Maybe Int ->
  Handler [StoryRep]
listStoriesHandler pool account maybePage maybeSize = do
  liftIO $
    Repo.listStories pool address page size
  where
    address = cs $ accountAddress account
    page = getPage maybePage
    size = getSize maybeSize

-- Get a page number or return a default.
getPage :: Maybe Int -> Int
getPage Nothing = 1
getPage (Just p) = max p 1

-- Get a page size or return a default.
getSize :: Maybe Int -> Int
getSize Nothing = 10
getSize (Just s) = max 1 (min s 100)

-- Delete a story from the database.
deleteStoryHandler ::
  ConnectionPool ->
  Account ->
  StoryId ->
  Handler NoContent
deleteStoryHandler pool account storyId = do
  maybeStory <- liftIO $ Repo.getStory pool storyId
  case verify account maybeStory of
    Just _ -> do
      liftIO $ Repo.deleteStory pool storyId
      return NoContent
    Nothing ->
      throwError $ notFound "Story not found"

-- Validate then insert a story in the database.
insertStoryHandler ::
  ConnectionPool ->
  Account ->
  StoryReq ->
  Handler StoryRep
insertStoryHandler pool account req =
  if name == ""
    then throwError $ badRequest "Invalid story name"
    else liftIO $ Repo.insertStory pool story
  where
    name = storyReqName req
    address = accountAddress account
    story = Story (cs address) (cs name)

-- Update a story name in the database.
updateStoryHandler ::
  ConnectionPool ->
  Account ->
  StoryId ->
  StoryReq ->
  Handler StoryRep
updateStoryHandler pool account storyId storyReq =
  if storyReqName storyReq == ""
    then throwError $ badRequest "Invalid story name"
    else updateStory' pool account storyId storyReq

-- Update story helper.
updateStory' ::
  ConnectionPool ->
  Account ->
  StoryId ->
  StoryReq ->
  Handler StoryRep
updateStory' pool account storyId storyReq = do
  maybeStory <- liftIO $ Repo.getStory pool storyId
  case verify account maybeStory of
    Just _ -> liftIO $ Repo.updateStory pool storyId story
    Nothing -> throwError $ notFound "Story not found"
  where
    name = storyReqName storyReq
    story = Story (cs $ accountAddress account) (cs name)

-- Get tasks for a story from the database.
listTasksHandler ::
  ConnectionPool ->
  Maybe StoryId ->
  Handler [TaskDto]
listTasksHandler _ Nothing =
  throwError $ badRequest "Missing storyId query parameter"
listTasksHandler pool (Just storyId) = do
  liftIO $ Repo.listTasks pool storyId

-- Get a task from the database.
getTaskHandler ::
  ConnectionPool ->
  TaskId ->
  Handler TaskDto
getTaskHandler pool taskId = do
  maybeTask <- liftIO $ Repo.getTask pool taskId
  case maybeTask of
    Just task -> return task
    Nothing -> throwError $ notFound "Task not found"

-- Insert a task in the database.
insertTaskHandler ::
  ConnectionPool ->
  Task ->
  Handler TaskDto
insertTaskHandler pool task =
  if taskName task == ""
    then throwError $ badRequest "Invalid task name"
    else insertTask' pool task

-- Insert task helper
insertTask' ::
  ConnectionPool ->
  Task ->
  Handler TaskDto
insertTask' pool task = do
  maybeStory <- liftIO $ Repo.getStory pool (taskStoryId task)
  case maybeStory of
    Just _ -> liftIO $ Repo.insertTask pool task
    Nothing -> throwError $ badRequest "Invalid storyId"

-- Delete a task from the database.
deleteTaskHandler ::
  ConnectionPool ->
  TaskId ->
  Handler NoContent
deleteTaskHandler pool taskId = do
  liftIO $ Repo.deleteTask pool taskId
  return NoContent

-- Update a task name and status in the database.
updateTaskHandler ::
  ConnectionPool ->
  TaskId ->
  Task ->
  Handler TaskDto
updateTaskHandler pool taskId task = do
  if taskName task == ""
    then throwError $ badRequest "Invalid task name"
    else liftIO $ Repo.updateTask pool taskId task
