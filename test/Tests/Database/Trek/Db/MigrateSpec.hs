module Tests.Database.Trek.Db.MigrateSpec where

import Database.Trek.Db.Migrate
import Database.Trek.Db.Internal
import Database.Trek.Types
import Tests.Database.Trek.DbUtils
import qualified Database.Trek.Db.Internal as Db
import Data.Foldable
import Data.Time.QQ
import Test.Hspec hiding (shouldBe, shouldThrow, shouldSatisfy, shouldReturn)
import Test.Hspec.Expectations.MonadThrow
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Maybe
import Data.List (sort)
import Control.Monad (void)

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

barConflictMigration :: Migration
barConflictMigration = Migration
  { mVersion = [utcIso8601| 2048-12-04 |]
  , mName = "bar"
  , mQuery = "CREATE TABLE barconflict (id SERIAL PRIMARY KEY);"
  }

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Core" $ do
  describe "migrationsToApply" $ do
    it "empty gives empty" $ do
      differenceMigrationsByVersion [] [] `shouldBe` [] :: IO ()
    it "already applied gives empty" $ do
      differenceMigrationsByVersion
        [stuffMigration]
        [mVersion stuffMigration]
          `shouldBe` [] :: IO ()


  withTestDB $ do
    describe "setup" $ do
      it "is valid" $ withDB $ do
        setup
        Db.diffSetup `shouldReturn` mempty

      it "throws if run twice" $ withDB $
        shouldThrow setup $ \ME_SetupRanTwice -> True

      it "teardown removes all the tables" $ withDB $ do
        teardown
        Db.diffSetup `shouldReturn` Db.cleanSchemaDiff

    describe "migrate" $ do
      it "adds new tables and updates the migration table" $ withDB $ do
        setup
        let migrations = [stuffMigration, thangMigration]

        Db.tableExists "stuff" `shouldReturn` False
        Db.tableExists "thang" `shouldReturn` False

        void $ migrate migrations

        Db.tableExists "stuff" `shouldReturn` True
        Db.tableExists "thang" `shouldReturn` True

      it "call runMigration twice does nothing" $ withDB $ do
        let migrations = [stuffMigration, thangMigration]

        Db.tableExists "stuff" `shouldReturn` True
        Db.tableExists "thang" `shouldReturn` True

        theBefore <- Db.getAllApplicationRecords

        fmap (either (error . show) id) (migrate migrations) `shouldReturn` Nothing

        Db.tableExists "stuff" `shouldReturn` True
        Db.tableExists "thang" `shouldReturn` True

        Db.getAllApplicationRecords `shouldReturn` theBefore

      it "running runMigration with a new migration and old ones applies the new one" $ withDB $ do
        let migrations = [stuffMigration, thangMigration, fooMigration]

        theBefore <- toList <$> Db.getAllApplicationRecords

        Application {..} <- either (error. show) (fromMaybe (error "migration failed to apply"))
          <$> migrate migrations

        Db.tableExists "foo" `shouldReturn` True

        fmap (sort . toList) Db.getAllApplicationRecords `shouldReturn` sort
          ( Db.ApplicationRecord
              [ Db.MigrationRow
                { mrVersion = [utcIso8601| 2048-12-03 |]
                , mrName = "foo"
                , mrHash = hashQuery $ mQuery fooMigration
                , mrCreatedAt = arCreatedAt
                , mrApplicationId = arId
                }
              ]
            : theBefore
          )

      it "running runMigration with just a new migration works" $ withDB $ do
        let migrations = [barMigration]

        theBefore <- toList <$> Db.getAllApplicationRecords

        Application {..} <- either (error. show) (fromMaybe (error "migration failed to apply"))
          <$> migrate migrations

        Db.tableExists "bar" `shouldReturn` True

        fmap (sort . toList) Db.getAllApplicationRecords `shouldReturn` sort
          ( Db.ApplicationRecord
              [ Db.MigrationRow
                { mrVersion = [utcIso8601| 2048-12-04 |]
                , mrName = "bar"
                , mrHash = hashQuery $ mQuery barMigration
                , mrCreatedAt = arCreatedAt
                , mrApplicationId = arId
                }
              ]
          : theBefore
          )

      it "running a migration with a HashConflict throws and continueing does nothing" $ withDB $ do
        let migrations = [barConflictMigration]

        theBefore <- toList <$> Db.getAllApplicationRecords

        Right Nothing <- migrate migrations

        Db.tableExists "barconflict" `shouldReturn` False

        fmap (sort . toList) Db.getAllApplicationRecords `shouldReturn` sort theBefore