module Service where

import Env (Env (..))
import Models
import qualified Repo

-- | Defines how to read stories
class StoryReader a where
  getStory :: a -> StoryId -> IO (Maybe StoryRep)
  listStories :: a -> Int -> Int -> IO [StoryRep]

-- Ties story reader functionality to the repo via environment (connection pool).
instance StoryReader Env where
  getStory (Env pool) = Repo.getStory pool
  listStories (Env pool) = Repo.listStories pool

-- | Defines how to write stories
class StoryWriter a where
  createStory :: a -> StoryReq -> IO StoryRep
  updateStory :: a -> StoryId -> StoryReq -> IO StoryRep
  deleteStory :: a -> StoryId -> IO ()

-- Ties story writer functionality to the repo via environment (connection pool).
instance StoryWriter Env where
  createStory (Env pool) = Repo.insertStory pool
  updateStory (Env pool) = Repo.updateStory pool
  deleteStory (Env pool) = Repo.deleteStory pool

-- | Defines how to read tasks
class TaskReader a where
  getTask :: a -> TaskId -> IO (Maybe TaskRep)
  listTasks :: a -> StoryId -> IO [TaskRep]

-- Ties task reader functionality to the repo via environment (connection pool).
instance TaskReader Env where
  getTask (Env pool) = Repo.getTask pool
  listTasks (Env pool) = Repo.listTasks pool

-- | Defines how to write tasks
class TaskWriter a where
  createTask :: a -> TaskReq -> IO TaskRep
  updateTask :: a -> TaskId -> TaskReq -> IO TaskRep
  deleteTask :: a -> TaskId -> IO ()

-- Ties task writer functionality to the repo via environment (connection pool).
instance TaskWriter Env where
  createTask (Env pool) = Repo.insertTask pool
  updateTask (Env pool) = Repo.updateTask pool
  deleteTask (Env pool) = Repo.deleteTask pool
