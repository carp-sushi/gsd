import Config
import qualified Database as DB
import Env
import Models
import qualified Repo
import Server (app)

import Network.HTTP.Types
import Servant hiding (Header)

import Test.Hspec
import Test.Hspec.Wai
import Test.Tasty
import Test.Tasty.Hspec

-- Setup test application with in-memory SQLite database.
setupApp :: IO Application
setupApp = do
  pool <- DB.createTestPool ":memory:" 1
  DB.runMigrations pool
  story <- Repo.insertStory pool (Story "First Story")
  _ <- Repo.insertStory pool (Story "Delete Me")
  _ <- Repo.insertTask pool $ Task (storyId_ story) "Task" Todo
  _ <- Repo.insertTask pool $ Task (storyId_ story) "Delete Me" Todo
  return $ app $ Env (Config "n/a" 0 1) pool

-- JSON content type headers for POST and PUT requests.
jsonCT :: [Header]
jsonCT =
  [("Content-Type", "application/json")]

-- POST helper
postJson url body =
  request methodPost url jsonCT body

-- PUT helper
putJson url body =
  request methodPut url jsonCT body

-- Create story tests
spec_createStory :: Spec
spec_createStory =
  with (liftIO setupApp) $ do
    describe "POST /stories" $ do
      it "should create a story" $ do
        postJson "/stories" "{\"name\":\"Test\"}" `shouldRespondWith` 201
      it "should fail to create a story with an empty name" $ do
        postJson "/stories" "{\"name\":\"\"}" `shouldRespondWith` 400
      it "should fail to create a story with an empty body" $ do
        postJson "/stories" "{}" `shouldRespondWith` 400

-- List story tests
spec_listStories :: Spec
spec_listStories =
  with (liftIO setupApp) $ do
    describe "GET /stories" $ do
      it "should list stories" $ do
        get "/stories" `shouldRespondWith` 200

-- Get story tests
spec_getStory :: Spec
spec_getStory =
  with (liftIO setupApp) $ do
    describe "GET /stories/{id}" $ do
      it "should return an existing story" $ do
        get "/stories/1" `shouldRespondWith` 200
      it "should return a 404 for a non-existing story" $ do
        get "/stories/101" `shouldRespondWith` 404

-- Update story tests
spec_updateStory :: Spec
spec_updateStory =
  with (liftIO setupApp) $ do
    describe "PUT /stories/{id}" $ do
      it "should update a story" $ do
        putJson "/stories/1" "{\"name\":\"Update\"}" `shouldRespondWith` 200
      it "should fail to update a story with an empty name" $ do
        putJson "/stories/1" "{\"name\":\"\"}" `shouldRespondWith` 400
      it "should fail to update a story with an empty body" $ do
        putJson "/stories/1" "{}" `shouldRespondWith` 400

-- Delete story tests
spec_deleteStory :: Spec
spec_deleteStory =
  with (liftIO setupApp) $ do
    describe "DELETE /stories/{id}" $ do
      it "should delete an existing story" $ do
        get "/stories/2" `shouldRespondWith` 200
        delete "/stories/2" `shouldRespondWith` 204
        get "/stories/2" `shouldRespondWith` 404

-- Create task tests
spec_createTask :: Spec
spec_createTask =
  with (liftIO setupApp) $ do
    describe "POST /tasks" $ do
      it "should create a task" $ do
        let body = "{\"storyId\":1,\"name\":\"Test\",\"status\":\"Todo\"}"
        postJson "/tasks" body `shouldRespondWith` 201
      it "should fail to create a task with an empty name" $ do
        let body = "{\"storyId\":1,\"name\":\"\",\"status\":\"Todo\"}"
        postJson "/tasks" body `shouldRespondWith` 400
      it "should fail to create a task with an empty body" $ do
        postJson "/tasks" "{}" `shouldRespondWith` 400

-- List task tests
spec_listTasks :: Spec
spec_listTasks =
  with (liftIO setupApp) $ do
    describe "GET /tasks" $ do
      it "should list tasks" $ do
        get "/tasks?storyId=1" `shouldRespondWith` 200
      it "should fail with missing query param" $ do
        get "/tasks" `shouldRespondWith` 400

-- Get task tests
spec_getTask :: Spec
spec_getTask =
  with (liftIO setupApp) $ do
    describe "GET /tasks/{id}" $ do
      it "should return an existing task" $ do
        get "/tasks/1" `shouldRespondWith` 200
      it "should return a 404 for a non-existing task" $ do
        get "/tasks/101" `shouldRespondWith` 404

-- Update task tests
spec_updateTask :: Spec
spec_updateTask =
  with (liftIO setupApp) $ do
    describe "PUT /tasks/{id}" $ do
      it "should update a task" $ do
        let body = "{\"storyId\":1,\"name\":\"Update\",\"status\":\"Done\"}"
        putJson "/tasks/1" body `shouldRespondWith` 200
      it "should fail to update a task with an empty name" $ do
        let body = "{\"storyId\":1,\"name\":\"\",\"status\":\"Todo\"}"
        putJson "/tasks/1" body `shouldRespondWith` 400
      it "should fail to update a task with an empty body" $ do
        putJson "/tasks/1" "{}" `shouldRespondWith` 400

-- Delete task tests
spec_deleteTask :: Spec
spec_deleteTask =
  with (liftIO setupApp) $ do
    describe "DELETE /tasks/{id}" $ do
      it "should delete an existing task" $ do
        get "/tasks/2" `shouldRespondWith` 200
        delete "/tasks/2" `shouldRespondWith` 204
        get "/tasks/2" `shouldRespondWith` 404

-- Collect all specs
allSpecs :: [Spec]
allSpecs =
  [ spec_createStory
  , spec_listStories
  , spec_getStory
  , spec_updateStory
  , spec_deleteStory
  , spec_createTask
  , spec_listTasks
  , spec_getTask
  , spec_updateTask
  , spec_deleteTask
  ]

-- Run tests
main :: IO ()
main = do
  specs <- fmap concat (mapM testSpecs allSpecs)
  defaultMain (testGroup "gsd-server specs" specs)
