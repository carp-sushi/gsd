{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( Api
  , api
  ) where

import Models (Story, StoryDto, StoryId)

import Data.Proxy
import Servant.API

-- API type
type Api =
  "stories" :> QueryParam "page" Int :> QueryParam "size" Int :> Get '[JSON] [StoryDto]
    :<|> "stories" :> ReqBody '[JSON] Story :> Post '[JSON] (Maybe StoryDto)
    :<|> "stories" :> Capture "storyId" StoryId :> Get '[JSON] (Maybe StoryDto)
    :<|> "stories" :> Capture "storyId" StoryId :> DeleteNoContent
    :<|> "stories" :> Capture "storyId" StoryId :> ReqBody '[JSON] Story :> Put '[JSON] (Maybe StoryDto)

-- API boilerplate
api :: Proxy Api
api = Proxy
