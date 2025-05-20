{-# LANGUAGE TypeApplications #-}

module Repo.TaskRepo
  ( listTasks
  , getTask
  , insertTask
  , deleteTask
  , updateTask
  ) where

import Database.Models

import Data.Int
import Data.String.Conversions (cs)
import Database.Esqueleto.Experimental
import qualified Database.Persist.Sql as S

-- For list queries that don't use paging, put a reasonable limit on result-set size.
maxRows :: Int64
maxRows = 100

-- Get tasks for a story
listTasks :: ConnectionPool -> StoryId -> IO [Entity Task]
listTasks pool storyId =
  flip runSqlPersistMPool pool $
    select $ do
      t <- from $ table @Task
      where_ $ t ^. TaskStoryId ==. val storyId
      limit maxRows
      return t

-- Get a task by primary key.
getTask :: ConnectionPool -> TaskId -> IO (Maybe Task)
getTask pool taskId =
  runSqlPersistMPool (S.get taskId) pool

-- Insert a new task in the database.
insertTask :: ConnectionPool -> Task -> IO TaskId
insertTask pool task =
  runSqlPersistMPool (S.insert task) pool

-- Delete a task by primary key.
deleteTask :: ConnectionPool -> TaskId -> IO ()
deleteTask pool taskId =
  runSqlPersistMPool (S.delete taskId) pool

-- Update a task name in the database.
updateTask :: ConnectionPool -> TaskId -> Task -> IO ()
updateTask pool taskId (Task _ name status) =
  flip runSqlPersistMPool pool $ do
    S.update taskId [TaskName S.=. cs name, TaskStatus S.=. status]
