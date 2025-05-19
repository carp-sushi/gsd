{-# LANGUAGE TypeApplications #-}

module Repo.TaskRepo
  ( listTasks
  , getTask
  , insertTask
  , deleteTask
  , updateTask
  ) where

import Data.Int
import Data.String.Conversions (cs)
import Database.Esqueleto.Experimental
import Database.Models
import qualified Database.Persist.Sql as S

-- For list queries that don't use paging, put a reasonable limit on result-set size.
maxRows :: Int64
maxRows = 100

-- Get tasks for a story
listTasks :: ConnectionPool -> StoryId -> IO [Entity Task]
listTasks pool storyId =
  flip S.runSqlPersistMPool pool $
    select $ do
      t <- from $ table @Task
      where_ $ t ^. TaskStoryId ==. val storyId
      limit maxRows
      return t

-- Get a task by primary key.
getTask :: ConnectionPool -> TaskId -> IO (Maybe TaskRep)
getTask pool taskId =
  flip runSqlPersistMPool pool $ do
    maybeTask <- S.get taskId
    return $ mkRep <$> maybeTask
  where
    mkRep (Task _ name status) = TaskRep taskId name status

-- Insert a new task in the database.
insertTask :: ConnectionPool -> TaskReq -> IO TaskRep
insertTask pool task =
  flip S.runSqlPersistMPool pool $ do
    taskId <- S.insert task
    return $ TaskRep taskId (taskName task) (taskStatus task)

-- Delete a task by primary key.
deleteTask :: ConnectionPool -> TaskId -> IO ()
deleteTask pool taskId =
  S.runSqlPersistMPool (S.delete taskId) pool

-- Update a task name in the database.
updateTask :: ConnectionPool -> TaskId -> TaskReq -> IO TaskRep
updateTask pool taskId (Task _ name status) =
  flip S.runSqlPersistMPool pool $ do
    S.update taskId [TaskName S.=. sname, TaskStatus S.=. status]
    return $ TaskRep taskId sname status
  where
    sname = cs name
