{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Api
  ( Api
  , api
  ) where

import Models (Story, StoryId)

import Data.Proxy
import Servant.API

import Data.Text (Text)

-- API type
type Api =
  "stories" :> QueryParam "page" Int :> Get '[JSON] [Story]
    :<|> "stories" :> ReqBody '[JSON] Story :> Post '[JSON] (Maybe Story)
    :<|> "stories" :> Capture "storyId" StoryId :> Get '[JSON] (Maybe Story)
    :<|> "stories" :> Capture "storyId" StoryId :> DeleteNoContent
    :<|> "stories" :> Capture "storyId" StoryId :> ReqBody '[JSON] Text :> Patch '[JSON] (Maybe Story)

-- API boilerplate
api :: Proxy Api
api = Proxy
