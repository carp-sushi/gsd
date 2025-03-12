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

-- Story API
type StoryApi =
  "stories" :> AuthProtect "account-auth" :> QueryParam "page" Int :> QueryParam "size" Int :> Get '[JSON] [StoryRep]
    :<|> "stories" :> AuthProtect "account-auth" :> ReqBody '[JSON] StoryReq :> PostCreated '[JSON] StoryRep
    :<|> "stories" :> AuthProtect "account-auth" :> Capture "storyId" StoryId :> Get '[JSON] StoryRep
    :<|> "stories" :> AuthProtect "account-auth" :> Capture "storyId" StoryId :> DeleteNoContent
    :<|> "stories" :> AuthProtect "account-auth" :> Capture "storyId" StoryId :> ReqBody '[JSON] StoryReq :> Put '[JSON] StoryRep

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
