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
  "stories" :> QueryParam "page" Int :> QueryParam "size" Int :> Get '[JSON] [StoryRep]
    :<|> "stories" :> ReqBody '[JSON] StoryReq :> PostCreated '[JSON] StoryRep
    :<|> "stories" :> Capture "storyId" StoryId :> Get '[JSON] StoryRep
    :<|> "stories" :> Capture "storyId" StoryId :> DeleteNoContent
    :<|> "stories" :> Capture "storyId" StoryId :> ReqBody '[JSON] StoryReq :> Put '[JSON] StoryRep

-- Task API type
type TaskApi =
  "tasks" :> QueryParam "storyId" StoryId :> Get '[JSON] [TaskRep]
    :<|> "tasks" :> Capture "taskId" TaskId :> Get '[JSON] TaskRep
    :<|> "tasks" :> ReqBody '[JSON] TaskReq :> PostCreated '[JSON] TaskRep
    :<|> "tasks" :> Capture "taskId" TaskId :> DeleteNoContent
    :<|> "tasks" :> Capture "taskId" TaskId :> ReqBody '[JSON] TaskReq :> Put '[JSON] TaskRep

-- API boilerplate
api :: Proxy Api
api = Proxy
