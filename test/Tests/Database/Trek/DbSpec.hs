module Tests.Database.Trek.DbSpec (spec, main) where
import Test.Hspec hiding (shouldBe, shouldThrow, shouldSatisfy, shouldReturn)
import Test.Hspec.Expectations.MonadThrow
import qualified Database.PostgreSQL.Simple as Psql
import Control.Monad (void)
import Database.PostgreSQL.Transact
import Database.Trek.Db.Internal
import Database.Trek.Db.Types
import Database.PostgreSQL.Simple.SqlQQ
import Data.List (sort)
import Data.Time.QQ
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import Data.Foldable
import Tests.Database.Trek.DbUtils

main :: IO ()
main = hspec spec

assertTableExists :: String -> DB ()
assertTableExists tableName = verifyTableExists tableName `shouldReturn` True

stuffVersion :: UTCTime
stuffVersion = [utcIso8601| 2048-12-01 |]

stuffMigration :: Migration
stuffMigration = Migration
  { mVersion = stuffVersion
  , mName = "stuff"
  , mQuery = "CREATE TABLE stuff (id SERIAL PRIMARY KEY);"
  }

thangVersion :: UTCTime
thangVersion = [utcIso8601| 2048-12-02 |]

thangMigration :: Migration
thangMigration = Migration
  { mVersion = thangVersion
  , mName = "thang"
  , mQuery = "CREATE TABLE thang (id SERIAL PRIMARY KEY);"
  }

migrationGroup :: NonEmpty Migration
migrationGroup = stuffMigration NonEmpty.:| [thangMigration]

resetMigrations :: DB ()
resetMigrations = do
  clearMigrations
  void $ execute_ [sql|DROP TABLE IF EXISTS stuff ; DROP TABLE IF EXISTS thang; |]

verifyAuditLog :: DB ()
verifyAuditLog = do
  columnNames <- map Psql.fromOnly <$> query_ [sql|
      SELECT
       COLUMN_NAME
      FROM
       information_schema.COLUMNS
      WHERE
       TABLE_NAME = 'applications';
    |]

  let expected =
       [ "id"
       , "created_at"
       ] :: [String]

  sort columnNames `shouldBe` sort expected

spec :: Spec
spec = describe "Db" $ do
  withTestDB $ describe "setup" $ do
    it "listMigrations fails if db is not setup" $ withDB $
      shouldThrow listMigrations $ \ME_NoSetup -> True

    it "setup works" $ withDB $ do
      setup

      verifyAuditLog

      columnNames <- map Psql.fromOnly <$> query_ [sql|
          SELECT
           COLUMN_NAME
          FROM
           information_schema.COLUMNS
          WHERE
           TABLE_NAME = 'migrations';
        |]

      let expected =
           [ "version"
           , "name"
           , "created_at"
           , "hash"
           , "application_id"
           ] :: [String]

      sort columnNames `shouldBe` sort expected

    it "setup throws if applied a second time" $ withDB $
      shouldThrow setup $ \ME_SetupRanTwice -> True

    it "listMigrations returns empty if no migrations" $ withDB $
        listMigrations `shouldReturn` []

    it "applyMigration/getAppliedVersions returns the version applied" $ withDB $ do
      Application {..} <- createApplication

      applyMigration arId stuffMigration

      listMigrations `shouldReturn` [(stuffVersion, hashQuery $ mQuery stuffMigration)]

    it "applyMigration updates the schema" $ withDB $ assertTableExists "stuff"

    it "applyMigration throws if it is already applied" $ withDB $ do
      Application {..} <- createApplication

      shouldThrow (applyMigration arId stuffMigration) $
        \(e :: MigrationException) -> e == ME_MigrationAlreadyApplied stuffVersion

    it "applyMigrations applies all the migrations" $ withDB $ do
      resetMigrations

      Application {..}  <- applyMigrations migrationGroup

      getMigrationsInApplication arId `shouldReturn`
        fmap mVersion migrationGroup

    it "applyMigrations updates the schema" $ withDB $
      mapM_ assertTableExists ["stuff", "thang"]

    it "applyMigrations throws if it is already applied" $ withDB $
      shouldThrow (applyMigrations migrationGroup) $
        \(e :: MigrationException) -> e == ME_MigrationAlreadyApplied stuffVersion

    it "insertMigration/getMigrationRow roundtrips basically" $ withDB $ do
      resetMigrations
      Application {..} <- createApplication

      let theVersion = [utcIso8601| 2048-12-01 |]
          theName = "initial"

          initial = Migration
            { mVersion = theVersion
            , mName = theName
            , mQuery = "hey"
            }

          expected = MigrationRow
            { mrVersion = theVersion
            , mrName = theName
            , mrCreatedAt = arCreatedAt
            , mrHash = hashQuery $ mQuery initial
            , mrApplicationId = arId
            }

      insertMigration $ toMigrationInput arId initial

      getMigrationRow theVersion `shouldReturn` expected

    it "applyMigrations/getAllMigrationRows" $ withDB $ do
      resetMigrations
      Application {..} <- applyMigrations migrationGroup
      getAllMigrations `shouldReturn`
        [ MigrationRow
          { mrVersion = [utcIso8601| 2048-12-01 |]
          , mrName = "stuff"
          , mrHash = hashQuery $ mQuery stuffMigration
          , mrApplicationId = arId
          , mrCreatedAt = arCreatedAt
          }
        , MigrationRow
          { mrVersion = [utcIso8601| 2048-12-02 |]
          , mrName = "thang"
          , mrHash = hashQuery $ mQuery thangMigration
          , mrApplicationId = arId
          , mrCreatedAt = arCreatedAt
          }
        ]

      -- getAllApplicationRecords works too
      fmap toList getAllApplicationRecords `shouldReturn` [ApplicationRecord
        [ MigrationRow
            { mrVersion = [utcIso8601| 2048-12-01 |]
            , mrName = "stuff"
            , mrHash = hashQuery $ mQuery stuffMigration
            , mrApplicationId = arId
            , mrCreatedAt = arCreatedAt
            }
          , MigrationRow
            { mrVersion = [utcIso8601| 2048-12-02 |]
            , mrName = "thang"
            , mrHash = hashQuery $ mQuery thangMigration
            , mrApplicationId = arId
            , mrCreatedAt = arCreatedAt
            }
          ]
        ]


