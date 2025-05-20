{-# LANGUAGE FlexibleContexts #-}

module Handler.TaskHandler
  ( listTasksHandler
  , getTaskHandler
  , insertTaskHandler
  , deleteTaskHandler
  , updateTaskHandler
  ) where

import Database.Models
import Env
import qualified Errors
import qualified Repo.StoryRepo as StoryRepo
import qualified Repo.TaskRepo as TaskRepo

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (isNothing)
import Servant

-- Get tasks for a story from the database.
listTasksHandler :: Maybe StoryId -> HandlerM [TaskRep]
listTasksHandler Nothing = throwError $ Errors.badRequest "Missing storyId query parameter"
listTasksHandler (Just storyId) = do
  (Env _ pool) <- ask
  tasks <- liftIO $ TaskRepo.listTasks pool storyId
  return $ mkTaskRep <$> tasks

-- Get a task from the database.
getTaskHandler :: TaskId -> HandlerM TaskRep
getTaskHandler taskId = do
  (Env _ pool) <- ask
  maybeTask <- liftIO $ TaskRepo.getTask pool taskId
  maybe (throwError $ Errors.notFound "Task not found") (return . mkRep) maybeTask
  where
    mkRep (Task _ name status) = TaskRep taskId name status

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
  maybeStory <- liftIO $ StoryRepo.getStory pool (taskStoryId task)
  if isNothing maybeStory
    then throwError $ Errors.badRequest "Invalid storyId"
    else do
      taskId <- liftIO $ TaskRepo.insertTask pool task
      return $ TaskRep taskId (taskName task) (taskStatus task)

-- Delete a task from the database.
deleteTaskHandler :: TaskId -> HandlerM NoContent
deleteTaskHandler taskId = do
  (Env _ pool) <- ask
  liftIO $ TaskRepo.deleteTask pool taskId
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
  maybeTask <- liftIO $ TaskRepo.getTask pool taskId
  if isNothing maybeTask
    then throwError $ Errors.notFound "Task not found"
    else do
      liftIO $ TaskRepo.updateTask pool taskId task
      return $ TaskRep taskId (taskName task) (taskStatus task)
