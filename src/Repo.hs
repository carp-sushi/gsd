module Repo
  ( getStory
  , insertStory
  , listStories
  , deleteStory
  , updateStory
  , listTasks
  , getTask
  ) where

import Models

import Data.String.Conversions (cs)
import Database.Persist.Sql

-- Get a story by primary key.
getStory :: ConnectionPool -> StoryId -> IO (Maybe StoryDto)
getStory pool storyId =
  flip runSqlPersistMPool pool $ do
    maybeStory <- get storyId
    return $ mkDto <$> maybeStory
  where
    mkDto (Story name) = StoryDto storyId name

-- Insert a new story in the database.
insertStory :: ConnectionPool -> Story -> IO (Maybe StoryDto)
insertStory pool story@(Story name) =
  flip runSqlPersistMPool pool $ do
    storyId <- insert story
    return $ Just $ StoryDto storyId name

-- List a page of stories
listStories :: ConnectionPool -> Int -> Int -> IO [StoryDto]
listStories pool page size =
  flip runSqlPersistMPool pool $ do
    stories <- queryPage
    return $ mkStoryDto <$> stories
  where
    queryPage = do
      selectList
        []
        [ LimitTo size
        , OffsetBy $ (page - 1) * size
        ]

-- Delete a story by primary key.
deleteStory :: ConnectionPool -> StoryId -> IO ()
deleteStory pool storyId =
  flip runSqlPersistMPool pool $ do
    delete storyId

-- Update a story name in the database.
updateStory :: ConnectionPool -> StoryId -> Story -> IO (Maybe StoryDto)
updateStory pool storyId (Story name) =
  flip runSqlPersistMPool pool $ do
    update storyId [StoryName =. sname]
    return $ Just $ StoryDto storyId sname
  where
    sname = cs name

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
