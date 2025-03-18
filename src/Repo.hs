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

import Data.String.Conversions (cs)
import Database.Persist.Sql

-- Get a story by primary key.
getStory :: ConnectionPool -> StoryId -> IO (Maybe StoryRep)
getStory pool storyId =
  flip runSqlPersistMPool pool $ do
    maybeStory <- get storyId
    return $ mkRep <$> maybeStory
  where
    mkRep (Story name) = StoryRep storyId name

-- Insert a new story in the database.
insertStory :: ConnectionPool -> StoryReq -> IO StoryRep
insertStory pool story@(Story name) =
  flip runSqlPersistMPool pool $ do
    storyId <- insert story
    return $ StoryRep storyId name

-- List a page of stories
listStories :: ConnectionPool -> Int -> Int -> IO [StoryRep]
listStories pool page size =
  flip runSqlPersistMPool pool $ do
    stories <- selectList [] [LimitTo size, OffsetBy $ (page - 1) * size]
    return $ mkStoryRep <$> stories

-- Delete a story by primary key.
deleteStory :: ConnectionPool -> StoryId -> IO ()
deleteStory pool storyId =
  runSqlPersistMPool (delete storyId) pool

-- Update a story name in the database.
updateStory :: ConnectionPool -> StoryId -> StoryReq -> IO StoryRep
updateStory pool storyId (Story name) =
  flip runSqlPersistMPool pool $ do
    update storyId [StoryName =. sname]
    return $ StoryRep storyId sname
  where
    sname = cs name

-- Get tasks for a story
listTasks :: ConnectionPool -> StoryId -> IO [TaskRep]
listTasks pool storyId =
  flip runSqlPersistMPool pool $ do
    tasks <- selectList [TaskStoryId ==. storyId] [LimitTo maxTasks]
    return $ mkTaskRep <$> tasks
  where
    maxTasks = 100

-- Get a task by primary key.
getTask :: ConnectionPool -> TaskId -> IO (Maybe TaskRep)
getTask pool taskId =
  flip runSqlPersistMPool pool $ do
    maybeTask <- get taskId
    return $ mkRep <$> maybeTask
  where
    mkRep (Task _ name status) = TaskRep taskId name status

-- Insert a new task in the database.
insertTask :: ConnectionPool -> TaskReq -> IO TaskRep
insertTask pool task =
  flip runSqlPersistMPool pool $ do
    taskId <- insert task
    return $ TaskRep taskId (taskName task) (taskStatus task)

-- Delete a task by primary key.
deleteTask :: ConnectionPool -> TaskId -> IO ()
deleteTask pool taskId =
  runSqlPersistMPool (delete taskId) pool

-- Update a task name in the database.
updateTask :: ConnectionPool -> TaskId -> TaskReq -> IO TaskRep
updateTask pool taskId (Task _ name status) =
  flip runSqlPersistMPool pool $ do
    update taskId [TaskName =. sname, TaskStatus =. status]
    return $ TaskRep taskId sname status
  where
    sname = cs name
