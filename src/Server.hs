module Server
  ( app
  ) where

import Api
import Auth
import Ctx
import Errors (customFormatters)
import Handlers

import Servant

-- Create the application.
app :: Ctx -> Application
app Ctx {pool_ = pool} =
  serveWithContext api serverCtx server
  where
    serverCtx = authHandler :. customFormatters :. EmptyContext
    server = stories :<|> tasks
    stories =
      listStoriesHandler pool
        :<|> insertStoryHandler pool
        :<|> getStoryHandler pool
        :<|> deleteStoryHandler pool
        :<|> updateStoryHandler pool
    tasks =
      listTasksHandler pool
        :<|> getTaskHandler pool
        :<|> insertTaskHandler pool
        :<|> deleteTaskHandler pool
        :<|> updateTaskHandler pool
