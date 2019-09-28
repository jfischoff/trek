module Database.Trek.CmdLineSpec where
import Database.Trek.CmdLine
import Test.Hspec
import System.Exit
import Control.Concurrent
import System.IO.Temp
import Data.String.Here

aroundAll :: forall a. ((a -> IO ()) -> IO ()) -> SpecWith a -> Spec
aroundAll withFunc specWith = do
  let start :: IO a
      start = do
        var <- newEmptyMVar

        withFunc $ putMVar var

        theA <- takeMVar var

        pure theA

      stop :: a -> IO ()
      stop _ = pure ()

  beforeAll start $ afterAll stop $ specWith

data TestMigrations = TestMigrations
  { successfulMigration  :: FilePath
  , migrationDirectory   :: FilePath
  , extraMigration       :: FilePath
  }

makeTestMigrations :: FilePath -> TestMigrations
makeTestMigrations = undefined

aroundAllMigration :: SpecWith TestMigrations -> Spec
aroundAllMigration = aroundAll $
  \f -> withSystemTempDirectory "migration" $ f . makeTestMigrations

successfulMigrationRecord :: String
successfulMigrationRecord = [here|
{ "rollback"   : fdsqfg12
, "id"         : 1
, "migrations" :
  [ { "name"           : "foo"
    , "version"        : "12/12/1980"
    , "hash"           : "xofdshagnosfdasngs"
    , "rollback"       : "fdsqfg12"
    , "application_id" : 1
    , "created_at"     : "12/12/2020"
    }
  ]
}
|]

extraMigrationRecord :: String
extraMigrationRecord = [here|
{ "rollback"   : fdsqfg12
, "id"         : 1
, "migrations" :
  [ { "name"           : "bar"
    , "version"        : "12/12/1980"
    , "hash"           : "barbar"
    , "rollback"       : "fdsqfg12"
    , "application_id" : 1
    , "created_at"     : "12/13/2020"
    }
  ]
|]

bothRecords :: String
bothRecords = [here|
  { "rollback"   : fdsqfg12
  , "id"         : 1
  , "migrations" :
    [ { "name"           : "foo"
      , "version"        : "12/12/1980"
      , "hash"           : "xofdshagnosfdasngs"
      , "rollback"       : "fdsqfg12"
      , "application_id" : 1
      , "created_at"     : "12/12/2020"
      }
    , { "name"           : "bar"
      , "version"        : "12/12/1980"
      , "hash"           : "barbar"
      , "rollback"       : "fdsqfg12"
      , "application_id" : 1
      , "created_at"     : "12/13/2020"
      }
    ]
  }
  |]

nothingToApply :: String
nothingToApply = "Nothing to apply!"

applyListApplicationsSpecs :: SpecWith TestMigrations
applyListApplicationsSpecs = do
  it "apply returns migration record when successful" $ \TestMigrations {..} -> do
    apply [successfulMigration] `shouldReturn` (ExitSuccess, successfulMigrationRecord, "")
  it "migrates more then one in a group" $ \TestMigrations {..} -> do
    apply [successfulMigration, extraMigration] `shouldReturn`
      (ExitSuccess, bothRecords, "")
  it "errors on empty" $ \TestMigrations {..} -> do
    _ <- apply [successfulMigration]
    apply [successfulMigration] `shouldReturn` (ExitFailure 64, "", nothingToApply)
  it "warns on empty" $ \TestMigrations {..} -> do
    _ <- apply [successfulMigration]
    apply [successfulMigration, "--warn-empty-migration"] `shouldReturn` (ExitSuccess, "", nothingToApply)
  it "ignores on empty" $ \TestMigrations {..} -> do
    _ <- apply [successfulMigration]
    apply [successfulMigration, "--no-warn-empty-migration"] `shouldReturn` (ExitSuccess, "", "")
  it "list initially returns nothing" $ \TestMigrations {..} ->
    listApplications [] `shouldReturn` (ExitSuccess, "[]", "")

{-


trek list
trek teardown

the idea is that I can make a Impl out of this for the spec.
Also I can test that somethings are exactly as I expect them.

-- On failure it outputs the state to rollback to
trek migrate --error-on-conflict FILEPATH
trek list
trek teardown

-}



spec :: Spec
spec = do
  aroundAllMigration applyListApplicationsSpecs