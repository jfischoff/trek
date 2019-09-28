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
  , conflictingMigration :: FilePath
  , migrationDirectory   :: FilePath
  , extraMigration       :: FilePath
  }

makeTestMigrations :: FilePath -> TestMigrations
makeTestMigrations = undefined

aroundAllMigration :: SpecWith TestMigrations -> Spec
aroundAllMigration = aroundAll $
  \f -> withSystemTempDirectory "migration" $ f . makeTestMigrations

noSetupMessage :: (ExitCode, String, String)
noSetupMessage = (ExitFailure 16, "", "Not Setup! Execute `trek setup` to setup\n")

setupTeardownSpecs :: Spec
setupTeardownSpecs = do
  it "setup doesn't accept arguments" $ setup ["hey"] `shouldReturn`
    (ExitFailure 4, "", "hey is not valid argument for `trek setup`. `trek setup` does not take additional arguments")
  it "setup initially succeed" $ setup [] `shouldReturn` (ExitSuccess, "", "")
  it "setup >> setup fails" $ (setup [] >> setup []) `shouldReturn`
    (ExitFailure 8, "", "Setup Already! >:(\n")
  it "teardown doesn't accept arguments" $ teardown ["hey"] `shouldReturn`
    (ExitFailure 4, "", "hey is not valid argument for `trek teardown`. `trek teardown` does not take additional arguments")
  it "teardown fails without setup" $ teardown [] `shouldReturn` noSetupMessage

  it "teardown succeeds after setup" $ (setup [] >> teardown [])
    `shouldReturn` (ExitSuccess, "", "")

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

conflictingMigrationWarning :: String
conflictingMigrationWarning = "The following versions have hash conflicts [\"12/12/1980\"=\"conflictingHash\"]"

nothingToApply :: String
nothingToApply = "Nothing to apply!"

applyListApplicationsSpecs :: SpecWith TestMigrations
applyListApplicationsSpecs = do
  it "apply without setup fails" $ \TestMigrations {..} -> apply [successfulMigration] `shouldReturn`
    noSetupMessage
  it "apply returns migration record when successful" $ \TestMigrations {..} -> do
    _ <- setup []
    apply [successfulMigration] `shouldReturn` (ExitSuccess, successfulMigrationRecord, "")
  it "reports a hash collision error" $ \TestMigrations {..} -> do
    _ <- setup []
    _ <- apply [successfulMigration]
    apply [conflictingMigration] `shouldReturn` (ExitFailure 32, conflictingMigrationWarning, "")
  it "reports a hash collision warning" $ \TestMigrations {..} -> do
    _ <- setup []
    _ <- apply [successfulMigration]
    apply ["--warn-hash-conflicts", migrationDirectory] `shouldReturn`
      (ExitSuccess, extraMigrationRecord, conflictingMigrationWarning)
  it "reports a hash collision warning" $ \TestMigrations {..} -> do
    _ <- setup []
    _ <- apply [successfulMigration]
    apply ["--no-warn-hash-conflicts", migrationDirectory] `shouldReturn`
      (ExitSuccess, extraMigrationRecord, "")
  it "migrates more then one in a group" $ \TestMigrations {..} -> do
    _ <- setup []
    apply [successfulMigration, extraMigration] `shouldReturn`
      (ExitSuccess, bothRecords, "")

  it "errors on empty" $ \TestMigrations {..} -> do
    _ <- setup []
    _ <- apply [successfulMigration]
    apply [successfulMigration] `shouldReturn` (ExitFailure 64, "", nothingToApply)

  it "warns on empty" $ \TestMigrations {..} -> do
    _ <- setup []
    _ <- apply [successfulMigration]
    apply [successfulMigration, "--warn-empty-migration"] `shouldReturn` (ExitSuccess, "", nothingToApply)

  it "ignores on empty" $ \TestMigrations {..} -> do
    _ <- setup []
    _ <- apply [successfulMigration]
    apply [successfulMigration, "--no-warn-empty-migration"] `shouldReturn` (ExitSuccess, "", "")

  it "hash-conflicts fails when not setup" $ \TestMigrations {..} -> do
    hashConflicts [] `shouldReturn` noSetupMessage
  it "hash-conflicts fails when not setup" $ \TestMigrations {..} -> do
    _ <- setup []
    _ <- apply [successfulMigration]
    hashConflicts [conflictingMigration] `shouldReturn`
      (ExitSuccess, "[\"12/12/1980\"=\"conflictingHash\"]", "")

{-

trek hash-conflicts FILEPATH
exitcode: 16
stderr: Not Setup! Execute `trek setup` to setup.

trek add-hashes [VERSION=HASH]
exitcode: 16
stderr: Not Setup! Execute `trek setup` to setup.

trek remove-hashes [VERSION]
exitcode: 16
stderr: Not Setup! Execute `trek setup` to setup.

trek teardown
exitcode: 16

In the case that we don't care about hashes

trek migrate FILEPATH
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
  setupTeardownSpecs
  aroundAllMigration applyListApplicationsSpecs