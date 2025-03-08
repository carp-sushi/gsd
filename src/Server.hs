module Server
  ( serverApp
  ) where

import Api (api)
import Handlers

import Database.Persist.Sql (ConnectionPool)
import Servant

-- Create the API server
serverApp :: ConnectionPool -> Application
serverApp pool =
  serve api $
    listStoriesHandler pool
      :<|> insertStoryHandler pool
      :<|> getStoryHandler pool
      :<|> deleteStoryHandler pool
      :<|> updateStoryHandler pool
      :<|> listTasksHandler pool
      :<|> getTaskHandler pool
