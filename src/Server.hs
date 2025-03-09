module Server
  ( app
  ) where

import Api
import Ctx
import Handlers

import Control.Monad.Trans.Reader (runReaderT)
import Servant

-- Transform custom handler monads to servant handlers.
transform :: Ctx -> HandlerM a -> Handler a
transform ctx hm =
  runReaderT hm ctx

-- Create the application.
app :: Ctx -> Application
app ctx =
  serve api $
    hoistServer api (transform ctx) server

-- Create the API server.
server :: ServerT Api HandlerM
server =
  storyHandlers :<|> taskHandlers
  where
    storyHandlers =
      listStoriesHandler
        :<|> insertStoryHandler
        :<|> getStoryHandler
        :<|> deleteStoryHandler
        :<|> updateStoryHandler
    taskHandlers =
      listTasksHandler
        :<|> getTaskHandler
        :<|> insertTaskHandler
        :<|> deleteTaskHandler
        :<|> updateTaskHandler
