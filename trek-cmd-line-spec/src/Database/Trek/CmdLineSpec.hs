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
}
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

twoApplicationRecords :: String
twoApplicationRecords = [here|
[ { "rollback"   : fdsqfg12
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
, { "rollback"   : fdsqfg12
  , "id"         : 2
  , "migrations" :
    [ { "name"           : "bar"
      , "version"        : "12/13/1980"
      , "hash"           : "barbar"
      , "rollback"       : "fdsqfg12"
      , "application_id" : 2
      , "created_at"     : "12/13/2020"
      }
    ]
  }
]
|]

applyListApplicationsSpecs :: SpecWith TestMigrations
applyListApplicationsSpecs = pure ()

spec :: Spec
spec = do
  aroundAllMigration applyListApplicationsSpecs
