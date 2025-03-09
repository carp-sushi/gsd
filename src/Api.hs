{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( Api
  , api
  ) where

import Models

import Data.Proxy
import Servant.API

-- API type
type Api = StoryApi :<|> TaskApi

-- Story API type
type StoryApi =
  "stories" :> QueryParam "page" Int :> QueryParam "size" Int :> Get '[JSON] [StoryDto]
    :<|> "stories" :> ReqBody '[JSON] Story :> PostCreated '[JSON] StoryDto
    :<|> "stories" :> Capture "storyId" StoryId :> Get '[JSON] StoryDto
    :<|> "stories" :> Capture "storyId" StoryId :> DeleteNoContent
    :<|> "stories" :> Capture "storyId" StoryId :> ReqBody '[JSON] Story :> Put '[JSON] StoryDto

-- Task API type
type TaskApi =
  "tasks" :> QueryParam "storyId" StoryId :> Get '[JSON] [TaskDto]
    :<|> "tasks" :> Capture "taskId" TaskId :> Get '[JSON] TaskDto
    :<|> "tasks" :> ReqBody '[JSON] Task :> PostCreated '[JSON] TaskDto
    :<|> "tasks" :> Capture "taskId" TaskId :> DeleteNoContent
    :<|> "tasks" :> Capture "taskId" TaskId :> ReqBody '[JSON] Task :> Put '[JSON] TaskDto

-- API boilerplate
api :: Proxy Api
api = Proxy
