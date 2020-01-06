module Database.Trek.CmdLineSpec where
-- import Database.Trek.CmdLine
import Test.Hspec
-- import System.Exit
import Control.Concurrent
import System.IO.Temp
import Data.String.Here
import qualified Database.Postgres.Temp as Temp
import Control.Exception
import Data.ByteString

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

withSetup :: (ByteString -> IO ()) -> IO ()
withSetup f = do
  -- Helper to throw exceptions
  let throwE x = either throwIO pure =<< x

  throwE $ Temp.withDbCache $ \dbCache -> do
    let combinedConfig = Temp.defaultConfig <> Temp.cacheConfig dbCache
    Temp.withConfig combinedConfig $ f . Temp.toConnectionString

aroundAllMigration :: SpecWith (FilePath, ByteString) -> Spec
aroundAllMigration = aroundAll $
  \f -> withSystemTempDirectory "migration" $ \dir ->
    withSetup $ \connStr -> f (dir, connStr)

fooMigrationText :: String
fooMigrationText = "CREATE TABLE foo (id int);"

-- TODO calculate hash
fooMigrationRecord :: String
fooMigrationRecord = [here|
{ "hash"         : "503dc144019a6d61db66b854c6e01127b94f644d"
, "created_at"     : "12/13/2020"
, "migrations" :
  [ { "name"           : "foo"
    , "version"        : "12/12/1980"
    , "hash"           : "503dc144019a6d61db66b854c6e01127b94f644d"
    }
  ]
}
|]

barMigrationRecord :: String
barMigrationRecord = [here|
{ "hash"         : "503dc144019a6d61db66b854c6e01127b94f644d"
, "created_at"     : "12/13/2020"
, "migrations" :
  [ { "name"           : "bar"
    , "version"        : "12/12/1980"
    , "hash"           : "503dc144019a6d61db66b854c6e01127b94f644d"
    }
  ]
}
|]

allMigrationsOutput :: String
allMigrationsOutput = [here|
  { "hash"         : "503dc144019a6d61db66b854c6e01127b94f644d"
  , "created_at"     : "12/13/2020"
  , "migrations" :
    [ { "name"           : "foo"
      , "version"        : "12/12/1980"
      , "hash"           : "0beec7b5ea3f0fdbc95d0dd47f3c5bc275da8a33"
      }
    , { "name"           : "bar"
      , "version"        : "12/12/1980"
      , "hash"           : "62cdb7020ff920e5aa642c3d4066950dd1f01f4d"
      }
    ]
  }
  |]

spec :: Spec
spec = aroundAllMigration $ do
  it "throws an exit code of 1 if the sql fails" $ \_ -> pending
  it "throw an exit code of 2 if there are not file in DIRPATH" $ \_ -> pending
  it "and invalid NAME-VERSION.sql file causes and exit code 3" $ \_ -> pending

  it "produces the right output with a single migration" $ \_ -> pending
  it "produces the right output with two migrations" $ \_ -> pending

  -- TODO need to write adapter
  -- test the core properities

  -- TODO need to figure (or not) a way to test the environment var options parser
  -- Probably going to kick because it seems really difficult to do from a black box
  -- perspective.

  -- property any migration made with `create` will not cause a ExitFailure 3 to get thrown.
  it "apply can parse any migration made with create" $ \_ -> pending

  -- start three creates at the same time and show that two succeed.
  it "create retries one time" $ \_ -> pending
