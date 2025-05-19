{-# LANGUAGE FlexibleContexts #-}

module Handler.StoryHandler
  ( deleteStoryHandler
  , getStoryHandler
  , insertStoryHandler
  , listStoriesHandler
  , updateStoryHandler
  ) where

import Database.Models
import Env
import qualified Errors
import Handler.Params (getPageParams)
import qualified Repo.StoryRepo as Repo

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Servant

-- Get a story from the database.
getStoryHandler :: StoryId -> HandlerM StoryRep
getStoryHandler storyId = do
  (Env _ pool) <- ask
  maybeStory <- liftIO $ Repo.getStory pool storyId
  maybe (throwError $ Errors.notFound "Story not found") (return . mkRep) maybeStory
  where
    mkRep (Story name) = StoryRep storyId name

-- Get a page of stories from the database.
listStoriesHandler :: Maybe Int -> Maybe Int -> HandlerM [StoryRep]
listStoriesHandler maybePage maybeSize = do
  (Env _ pool) <- ask
  let (page, size) = getPageParams maybePage maybeSize
  stories <- liftIO $ Repo.listStories pool page size
  return $ mkStoryRep <$> stories

-- Delete a story from the database.
deleteStoryHandler :: StoryId -> HandlerM NoContent
deleteStoryHandler storyId = do
  (Env _ pool) <- ask
  liftIO $ Repo.deleteStory pool storyId
  return NoContent

-- Validate then insert a story in the database.
insertStoryHandler :: StoryReq -> HandlerM StoryRep
insertStoryHandler story@(Story name) =
  if name == ""
    then throwError $ Errors.badRequest "Invalid story name"
    else insertStory' story

-- Insert story helper
insertStory' :: StoryReq -> HandlerM StoryRep
insertStory' story = do
  (Env _ pool) <- ask
  storyId <- liftIO $ Repo.insertStory pool story
  return $ StoryRep storyId (storyName story)

-- Update a story name in the database.
updateStoryHandler :: StoryId -> StoryReq -> HandlerM StoryRep
updateStoryHandler storyId story@(Story name) =
  if name == ""
    then throwError $ Errors.badRequest "Invalid story name"
    else updateStory' storyId story

-- Update story helper.
updateStory' :: StoryId -> StoryReq -> HandlerM StoryRep
updateStory' storyId story = do
  (Env _ pool) <- ask
  _ <- liftIO $ Repo.updateStory pool storyId story
  return $ StoryRep storyId (storyName story)
