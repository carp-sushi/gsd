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
    return $ mkReply <$> maybeStory
  where
    mkReply (Story _ name) = StoryRep storyId name

-- Insert a new story in the database.
insertStory :: ConnectionPool -> Story -> IO StoryRep
insertStory pool story =
  flip runSqlPersistMPool pool $ do
    storyId <- insert story
    return $ StoryRep storyId $ storyName story

-- List a page of stories
listStories :: ConnectionPool -> String -> Int -> Int -> IO [StoryRep]
listStories pool address page size =
  flip runSqlPersistMPool pool $ do
    stories <- selectList [StoryAddress ==. address] [LimitTo size, OffsetBy $ (page - 1) * size]
    return $ mkStoryRep <$> stories

-- Delete a story by primary key.
deleteStory :: ConnectionPool -> StoryId -> IO ()
deleteStory pool storyId =
  flip runSqlPersistMPool pool $ do
    delete storyId

-- Update a story name in the database.
updateStory :: ConnectionPool -> StoryId -> Story -> IO StoryRep
updateStory pool storyId story =
  flip runSqlPersistMPool pool $ do
    update storyId [StoryName =. sname]
    return $ StoryRep storyId sname
  where
    sname = cs $ storyName story

-- Get tasks for a story
listTasks :: ConnectionPool -> StoryId -> IO [TaskDto]
listTasks pool storyId =
  flip runSqlPersistMPool pool $ do
    tasks <- selectList [TaskStoryId ==. storyId] [LimitTo maxTasks]
    return $ mkTaskDto <$> tasks
  where
    maxTasks = 100

-- Get a task by primary key.
getTask :: ConnectionPool -> TaskId -> IO (Maybe TaskDto)
getTask pool taskId =
  flip runSqlPersistMPool pool $ do
    maybeTask <- get taskId
    return $ mkDto <$> maybeTask
  where
    mkDto (Task _ name status) = TaskDto taskId name status

-- Insert a new task in the database.
insertTask :: ConnectionPool -> Task -> IO TaskDto
insertTask pool task =
  flip runSqlPersistMPool pool $ do
    taskId <- insert task
    return $ TaskDto taskId (taskName task) (taskStatus task)

-- Delete a task by primary key.
deleteTask :: ConnectionPool -> TaskId -> IO ()
deleteTask pool taskId =
  flip runSqlPersistMPool pool $ do
    delete taskId

-- Update a task name in the database.
updateTask :: ConnectionPool -> TaskId -> Task -> IO TaskDto
updateTask pool taskId (Task _ name status) =
  flip runSqlPersistMPool pool $ do
    update taskId [TaskName =. sname, TaskStatus =. status]
    return $ TaskDto taskId sname status
  where
    sname = cs name
