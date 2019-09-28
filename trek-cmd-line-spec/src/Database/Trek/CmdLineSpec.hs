module Database.Trek.CmdLineSpec where
import Database.Trek.CmdLine
import Test.Hspec
import System.Exit
import Control.Concurrent
import System.IO.Temp
import System.IO

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

setupTeardownSpecs :: Spec
setupTeardownSpecs = do
  it "setup doesn't accept arguments" $ setup ["hey"] `shouldReturn`
    (ExitFailure 4, "", "hey is not valid argument for `trek setup`. `trek setup` does not take additional arguments")
  it "setup initially succeed" $ setup [] `shouldReturn` (ExitSuccess, "", "")
  it "setup >> setup fails" $ (setup [] >> setup []) `shouldReturn`
    (ExitFailure 8, "", "Setup Already! >:(\n")
  it "teardown doesn't accept arguments" $ teardown ["hey"] `shouldReturn`
    (ExitFailure 4, "", "hey is not valid argument for `trek teardown`. `trek teardown` does not take additional arguments")
  it "teardown fails without setup" $ teardown [] `shouldReturn`
    (ExitFailure 16, "", "Not Setup! Excute `trek setup` to setup\n")
  it "teardown succeeds after setup" $ (setup [] >> teardown [])
    `shouldReturn` (ExitSuccess, "", "")

applyListApplicationsSpecs :: SpecWith FilePath
applyListApplicationsSpecs = do
  it "apply without setup fails" $ \migrationFile -> apply [migrationFile] `shouldReturn`
    (ExitFailure 16, "", "")

{-
> trek migrate FILEPATH
exitcode: 16
stderr: Not Setup! Execute `trek setup` to setup.
exitcode: 32
stderr: The following versions have hash conflicts [VERSION]
> trek migrate FILEPATH --warn-hash-conflicts
stderr: The following versions have hash conflicts [VERSION]
{ name           : foo
, version        : 12/12/1980
, hash           : xofdshagnosfdasngs
, rollback       : fdsqfg12
, application_id : 1
, created_at     : 12/12/2020
}
> trek migrate FILEPATH --no-warn-hash-conflicts
{ name           : foo
, version        : 12/12/1980
, hash           : xofdshagnosfdasngs
, rollback       : fdsqfg12
, application_id : 1
, created_at     : 12/12/2020
}
> trek migrate FILEPATH
{ rollback   : fdsqfg12
, id         : 1
, migrations :
  [ { name           : foo
    , version        : 12/12/1980
    , hash           : xofdshagnosfdasngs
    , application_id : 1
    , created_at     : 12/12/2020
    }
  , { name           : foo
    , version        : 12/12/1981
    , hash           : xofdshagnosfdasngs
    , application_id : 1
    , created_at     : 12/12/2021
    }
  ]
}
> trek migrate FILEPATH
exitcode: 64
stderr: Nothing to migrate!
> trek migrate FILEPATH --warn-empty-migration
stderr: Nothing to migrate!
> trek migrate FILEPATH --no-warn-empty-migration

trek hash-conflicts FILEPATH
exitcode: 16
stderr: Not Setup! Execute `trek setup` to setup.

trek add-hashes [VERSION]
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

aroundAllMigration :: SpecWith FilePath -> Spec
aroundAllMigration = aroundAll $
  \f -> withSystemTempFile "migration" $
    \filePath h -> hClose h >> f filePath

spec :: Spec
spec = do
  setupTeardownSpecs
  aroundAllMigration applyListApplicationsSpecs