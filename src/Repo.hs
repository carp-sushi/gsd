module Repo
  ( getStory
  , insertStory
  , listStories
  , deleteStory
  , updateStory
  ) where

import Models

import Data.String.Conversions (cs)
import Data.Text (Text)
import Database.Persist.Sql

-- Use a static page size
pageSize :: Int
pageSize = 10

-- Get a story by primary key.
getStory :: ConnectionPool -> StoryId -> IO (Maybe Story)
getStory pool storyId =
  flip runSqlPersistMPool pool $ do
    get storyId

-- Insert a new story in the database.
insertStory :: ConnectionPool -> Story -> IO (Maybe Story)
insertStory pool story =
  flip runSqlPersistMPool pool $ do
    _ <- insert story
    return $ Just story

-- List a page fo stories
listStories :: ConnectionPool -> Int -> IO [Story]
listStories pool page =
  flip runSqlPersistMPool pool $ do
    stories <- queryPage
    return $ entityVal <$> stories
  where
    queryPage = do
      selectList
        []
        [ LimitTo pageSize
        , OffsetBy $ (page - 1) * pageSize
        ]

-- Delete a story by primary key.
deleteStory :: ConnectionPool -> StoryId -> IO ()
deleteStory pool storyId =
  flip runSqlPersistMPool pool $ do
    delete storyId

-- Update a story name in the database.
updateStory :: ConnectionPool -> StoryId -> Text -> IO (Maybe Story)
updateStory pool storyId name =
  flip runSqlPersistMPool pool $ do
    update storyId [StoryName =. (cs name)]
    get storyId
