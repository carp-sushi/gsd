module App
  ( app
  )
where

import Api (Api, api)
import Control.Monad.Trans.Reader (runReaderT)
import Env (Env, HandlerM)
import Errors (customErrorFormatters)
import Handlers
import Servant

-- Create the application.
app :: Env -> Application
app env =
  let ctx = customErrorFormatters :. EmptyContext
      server = hoistServer api (transform env) mkServer
   in serveWithContext api ctx server

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
