module Server
  ( app
  ) where

import Api
import Ctx
import Errors (customFormatters)
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
  serveWithContext api serverCtx server
  where
    serverCtx = customFormatters :. EmptyContext
    server = hoistServer api (transform ctx) mkServer

-- Create the API server.
mkServer :: ServerT Api HandlerM
mkServer =
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
