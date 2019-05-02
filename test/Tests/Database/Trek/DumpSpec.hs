module Tests.Database.Trek.DumpSpec where

import Database.Trek.Dump

import Test.Hspec hiding (shouldBe, shouldThrow, shouldSatisfy, shouldReturn)
import Test.Hspec.Expectations.MonadThrow
import Tests.Database.Trek.DbUtils
import Database.Trek.Types
import Tests.Database.Trek.TestUtils
import Database.Trek.Types
import Database.Trek.Db
import System.IO.Temp
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Async (Async)
import Control.Concurrent
import Control.Monad (forever)
import Data.Time
import Data.Time.Format
import Data.Time.QQ
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import Data.List
import Database.PostgreSQL.Transact
import Control.Monad (void)
import qualified Database.Postgres.Temp as Temp
import qualified Database.Postgres.Temp.Internal as Temp
import qualified Database.PostgreSQL.Simple.Options as PS
import qualified Database.PostgreSQL.Simple as Psql

main :: IO ()
main = hspec spec

testDump :: Dump
testDump = Dump
  { dSchema =
      "CREATE TABLE stuff (id SERIAL PRIMARY KEY);" <>
      migrationProduction <>
      auditLog
  , dAppliedMigrations = pure $
      ProdMigration
        { pmVersion = [utcIso8601| 2048-12-01 |]
        , pmName = "stuff"
        }
  }

stuffVersion :: UTCTime
stuffVersion = [utcIso8601| 2048-12-01 |]

stuffMigration :: Migration
stuffMigration = Migration
  { mVersion = stuffVersion
  , mName = "stuff"
  , mQuery = "CREATE TABLE stuff (id SERIAL PRIMARY KEY);"
  }

spec :: Spec
spec = describe "Dump" $ do
  withTempDir "trek-dump" $ do
    it "save/load dump works" $ \directoryPath -> do
      let dumpFileName = directoryPath ++ "/dump"
      saveDump dumpFileName testDump
      loadSchema dumpFileName `shouldReturn` Just testDump
    it "load of a non-existent file gives Nothing" $ \directoryPath -> do
      let dumpFileName = directoryPath ++ "/dump2"
      loadSchema dumpFileName `shouldReturn` Nothing

  withTestDbAndDirSpec "trek-dump" $ do
    it "migrate/dump/apply roundtrips" $ \(conn, db, filePath) -> do
      flip runDBTSerializable conn $ do
        setupProd
        applyMigrationGroup Prod Nothing $ pure stuffMigration

      let runner = Runner $ flip runDBTSerializable conn
          connString = Temp.connectionString db
          Right partial = PS.parseConnectionString  connString
          Right options = PS.completeOptions partial

      -- I need to be able to cleanly shutdown the thing
      -- need to be able to cancel all the connections

      dumpSchema (Runner $ flip runDBTSerializable conn) connString filePath

      Temp.terminateConnections db

      applySchema "" options Dev filePath

      conn1 <- Psql.connectPostgreSQL $ BSC.pack connString

      flip runDBTSerializable conn1 $ do
        verifyTableExists "stuff" `shouldReturn` True
        verifyTableExists "migrations" `shouldReturn` True
        verifyTableExists "applications" `shouldReturn` True

      pure () :: IO ()







