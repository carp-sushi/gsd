module Server
  ( app
  )
where

import Api
import Control.Monad.Trans.Reader (runReaderT)
import Env
import Errors (customFormatters)
import Handlers
import Servant

-- Transform custom handler monads to servant handlers.
transform :: Env -> HandlerM a -> Handler a
transform env hm =
  runReaderT hm env

-- Create the application.
app :: Env -> Application
app env =
  serveWithContext api ctx server
  where
    ctx = customFormatters :. EmptyContext
    server = hoistServer api (transform env) mkServer

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
