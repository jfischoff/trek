module Database.Trek.CmdLineSpec where
-- import Database.Trek.CmdLine
import Test.Hspec
-- import System.Exit
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
makeTestMigrations _migrationDirectory = undefined

aroundAllMigration :: SpecWith TestMigrations -> Spec
aroundAllMigration = aroundAll $
  \f -> withSystemTempDirectory "migration" $ f . makeTestMigrations

successfulMigrationRecord :: String
successfulMigrationRecord = [here|
{ "id"         : "503dc144019a6d61db66b854c6e01127b94f644d"
, "migrations" :
  [ { "name"           : "foo"
    , "version"        : "12/12/1980"
    , "hash"           : "xofdshagnosfdasngs"
    , "created_at"     : "12/12/2020"
    }
  ]
}
|]

extraMigrationRecord :: String
extraMigrationRecord = [here|
{ "id"         : "503dc144019a6d61db66b854c6e01127b94f644d"
, "migrations" :
  [ { "name"           : "bar"
    , "version"        : "12/12/1980"
    , "hash"           : "barbar"
    , "created_at"     : "12/13/2020"
    }
  ]
}
|]

bothRecords :: String
bothRecords = [here|
  { "id"         : "503dc144019a6d61db66b854c6e01127b94f644d"
  , "migrations" :
    [ { "name"           : "foo"
      , "version"        : "12/12/1980"
      , "hash"           : "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"
      , "created_at"     : "12/12/2020"
      }
    , { "name"           : "bar"
      , "version"        : "12/12/1980"
      , "hash"           : "62cdb7020ff920e5aa642c3d4066950dd1f01f4d"
      , "created_at"     : "12/13/2020"
      }
    ]
  }
  |]

nothingToApply :: String
nothingToApply = "Nothing to apply!"


spec :: Spec
spec = do
  aroundAllMigration applyListApplicationsSpecs
