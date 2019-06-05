module Tests.Database.Trek.CoreSpec where

import Database.Trek.Core
import Database.Trek.Types
import Tests.Database.Trek.DbUtils
import qualified Database.Trek.Db as Db
import Tests.Database.Trek.TestUtils
import Data.Foldable
import Control.Concurrent
import qualified Database.Postgres.Temp.Internal as Temp
import qualified Database.PostgreSQL.Simple.Options as PS
import qualified Database.PostgreSQL.Simple as PS
import Data.Time.QQ
import Test.Hspec hiding (shouldBe, shouldThrow, shouldSatisfy, shouldReturn)
import Test.Hspec.Expectations.MonadThrow
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import System.Directory
import System.Process
import System.Exit
import Data.Maybe
import Data.List (sort)

stuffVersion :: UTCTime
stuffVersion = [utcIso8601| 2048-12-01 |]

stuffName :: Text
stuffName = "stuff"

stuffQuery :: ByteString
stuffQuery = "CREATE TABLE stuff (id SERIAL PRIMARY KEY);"

stuffMigration :: Migration
stuffMigration = Migration
  { mVersion = stuffVersion
  , mName = stuffName
  , mQuery = stuffQuery
  }

thangVersion :: UTCTime
thangVersion = [utcIso8601| 2048-12-02 |]

thangName :: Text
thangName = "thang"

thangQuery :: ByteString
thangQuery = "CREATE TABLE thang (id SERIAL PRIMARY KEY);"

thangMigration :: Migration
thangMigration = Migration
  { mVersion = thangVersion
  , mName = thangName
  , mQuery = thangQuery
  }

fooMigration :: Migration
fooMigration = Migration
  { mVersion = [utcIso8601| 2048-12-03 |]
  , mName = "foo"
  , mQuery = "CREATE TABLE foo (id SERIAL PRIMARY KEY);"
  }

barMigration :: Migration
barMigration = Migration
  { mVersion = [utcIso8601| 2048-12-04 |]
  , mName = "bar"
  , mQuery = "CREATE TABLE bar (id SERIAL PRIMARY KEY);"
  }

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Core" $ do
  describe "migrationsToApply" $ do
    it "empty gives empty" $ do
      migrationsToApply [] [] `shouldBe` ([], []) :: IO ()
    it "already applied gives empty" $ do
      migrationsToApply
        [stuffMigration]
        [(mVersion stuffMigration, hashQuery $ mQuery stuffMigration)]
          `shouldBe` ([], []) :: IO ()


  withTestDB $ do
    describe "setup" $ do
      it "is valid" $ withDB $ do
        setup
        Db.diffSetup `shouldReturn` mempty

      it "throws if run twice" $ withDB $
        shouldThrow setup $ \SetupRanTwice -> True

      it "teardown removes all the tables" $ withDB $ do
        teardown
        Db.diffSetup `shouldReturn` Db.cleanSchemaDiff

    describe "migrate" $ do
      it "adds new tables and updates the migration table" $ withDB $ do
        setup
        let migrations = [stuffMigration, thangMigration]

        Db.tableExists "stuff" `shouldReturn` False
        Db.tableExists "thang" `shouldReturn` False

        migrate migrations

        Db.tableExists "stuff" `shouldReturn` True
        Db.tableExists "thang" `shouldReturn` True

      it "call runMigration twice does nothing" $ withDB $ do
        let migrations = [stuffMigration, thangMigration]

        Db.tableExists "stuff" `shouldReturn` True
        Db.tableExists "thang" `shouldReturn` True

        before <- Db.getAllApplicationRecords

        migrate migrations `shouldReturn` Nothing

        Db.tableExists "stuff" `shouldReturn` True
        Db.tableExists "thang" `shouldReturn` True

        Db.getAllApplicationRecords `shouldReturn` before

      it "running runMigration with a new migration and old ones applies the new one" $ withDB $ do
        let migrations = [stuffMigration, thangMigration, fooMigration]

        before <- toList <$> Db.getAllApplicationRecords

        Db.ApplicationRow {..} <- fromMaybe (error "migration failed to apply")
          <$> migrate migrations

        Db.tableExists "foo" `shouldReturn` True

        fmap (sort . toList) Db.getAllApplicationRecords `shouldReturn` sort
          ( Db.Application
              [ Db.MigrationRow
                { mrVersion = [utcIso8601| 2048-12-03 |]
                , mrName = "foo"
                , mrHash = hashQuery $ mQuery fooMigration
                , mrCreatedAt = arCreatedAt
                , mrApplicationId = arId
                }
              ]
            : before
          )


      it "running runMigration with just a new migration works" $ withDB $ do
        let migrations = [barMigration]

        before <- toList <$> Db.getAllApplicationRecords

        Db.ApplicationRow {..} <- fromMaybe (error "migration failed to apply")
          <$> migrate migrations

        Db.tableExists "bar" `shouldReturn` True

        fmap (sort . toList) Db.getAllApplicationRecords `shouldReturn` sort
          ( Db.Application
              [ Db.MigrationRow
                { mrVersion = [utcIso8601| 2048-12-04 |]
                , mrName = "bar"
                , mrHash = hashQuery $ mQuery barMigration
                , mrCreatedAt = arCreatedAt
                , mrApplicationId = arId
                }
              ]
          : before
          )