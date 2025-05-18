{-# LANGUAGE TypeApplications #-}

module Repo
  ( getStory
  , insertStory
  , listStories
  , deleteStory
  , updateStory
  , listTasks
  , getTask
  , insertTask
  , deleteTask
  , updateTask
  ) where

import Models

import Data.Int
import Data.String.Conversions (cs)
import Database.Esqueleto.Experimental
import qualified Database.Persist.Sql as S

-- Get a story by primary key.
getStory :: ConnectionPool -> StoryId -> IO (Maybe StoryRep)
getStory pool storyId =
  flip S.runSqlPersistMPool pool $ do
    maybeStory <- S.get storyId
    return $ mkRep <$> maybeStory
  where
    mkRep (Story name) = StoryRep storyId name

-- Insert a new story in the database.
insertStory :: ConnectionPool -> StoryReq -> IO StoryRep
insertStory pool story@(Story name) =
  flip S.runSqlPersistMPool pool $ do
    storyId <- S.insert story
    return $ StoryRep storyId name

-- List a page of stories
listStories :: ConnectionPool -> Int -> Int -> IO [Entity Story]
listStories pool page size =
  flip S.runSqlPersistMPool pool $
    select $ do
      s <- from $ table @Story
      limit $ fromIntegral size
      offset $ fromIntegral $ (page - 1) * size
      return s

-- Delete a story by primary key.
deleteStory :: ConnectionPool -> StoryId -> IO ()
deleteStory pool storyId =
  S.runSqlPersistMPool (S.delete storyId) pool

-- Update a story name in the database.
updateStory :: ConnectionPool -> StoryId -> StoryReq -> IO StoryRep
updateStory pool storyId (Story name) =
  flip S.runSqlPersistMPool pool $ do
    S.update storyId [StoryName S.=. sname]
    return $ StoryRep storyId sname
  where
    sname = cs name

-- Get tasks for a story
listTasks :: ConnectionPool -> StoryId -> IO [Entity Task]
listTasks pool storyId =
  flip S.runSqlPersistMPool pool $
    select $ do
      t <- from $ table @Task
      where_ $ t ^. TaskStoryId ==. val storyId
      limit maxRows
      return t

-- For list queries that don't use paging, put a reasonable limit on result-set size.
maxRows :: Int64
maxRows = 100

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
