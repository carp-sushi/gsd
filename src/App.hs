module App
  ( app
  )
where

import Api
import Control.Monad.Trans.Reader (runReaderT)
import Env
import Errors (customFormatters)
import Handlers
import Servant

-- Create the application.
app :: Env -> Application
app env =
  serveWithContext api ctx server
  where
    ctx = customFormatters :. EmptyContext
    server = hoistServer api (transform env) mkServer

-- Transform custom handler monads to servant handlers.
transform :: Env -> HandlerM a -> Handler a
transform env handlerM =
  runReaderT handlerM env

-- Create the API server.
mkServer :: ServerT Api HandlerM
mkServer =
  storyHandlers :<|> taskHandlers
  where
    storyHandlers =
      listStoriesHandler
        :<|> createStoryHandler
        :<|> getStoryHandler
        :<|> deleteStoryHandler
        :<|> updateStoryHandler
    taskHandlers =
      listTasksHandler
        :<|> getTaskHandler
        :<|> createTaskHandler
        :<|> deleteTaskHandler
        :<|> updateTaskHandler
