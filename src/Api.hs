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
type Api =
  "stories" :> QueryParam "page" Int :> QueryParam "size" Int :> Get '[JSON] [StoryDto]
    :<|> "stories" :> ReqBody '[JSON] Story :> PostCreated '[JSON] StoryDto
    :<|> "stories" :> Capture "storyId" StoryId :> Get '[JSON] StoryDto
    :<|> "stories" :> Capture "storyId" StoryId :> DeleteNoContent
    :<|> "stories" :> Capture "storyId" StoryId :> ReqBody '[JSON] Story :> Put '[JSON] StoryDto
    :<|> "tasks" :> QueryParam "storyId" StoryId :> Get '[JSON] [TaskDto]
    :<|> "tasks" :> Capture "taskId" TaskId :> Get '[JSON] TaskDto

-- API boilerplate
api :: Proxy Api
api = Proxy
