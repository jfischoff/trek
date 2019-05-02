module Tests.Database.Trek.DbSpec where

import Test.Hspec hiding (shouldBe, shouldThrow, shouldSatisfy, shouldReturn)
import Test.Hspec.Expectations.MonadThrow
import qualified Database.Postgres.Temp as Temp
import qualified Database.PostgreSQL.Simple as Psql
import Control.Monad (void)
import qualified Data.ByteString.Char8 as BSC
import Control.Exception
import Database.PostgreSQL.Transact
import Database.Trek.Db
import Database.Trek.Types
import Database.PostgreSQL.Simple.SqlQQ
import Data.List (sort)
import Data.Time.QQ
import Data.Time
import Control.Monad.IO.Class
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import Text.InterpolatedString.Perl6
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

sharedSpecs :: Mode -> SpecWith (Psql.Connection, Temp.DB)
sharedSpecs mode = do
  it "getAppliedMigrations returns empty if no migrations" $ withDB $
      getAppliedMigrations `shouldReturn` []

  it "applyMigration/getAppliedVersions returns the version applied" $ withDB $ do
    applicationId <- createApplication Nothing

    applyMigration mode applicationId stuffMigration

    getAppliedMigrations `shouldReturn` [(stuffVersion, if mode == Dev then Just $ hashMigration stuffMigration else Nothing)]

  it "applyMigration updates the schema" $ withDB $ assertTableExists "stuff"

  it "applyMigration throws if it is already applied" $ withDB $ do
    applicationId <- createApplication Nothing

    shouldThrow (applyMigration mode applicationId stuffMigration) $
      \(e :: MigrationException) -> e == MigrationAlreadyApplied stuffVersion

  it "applyMigrationGroup applies all the migrations" $ withDB $ do
    resetMigrations

    applicationId <- applyMigrationGroup mode Nothing migrationGroup

    getMigrationsInApplication applicationId `shouldReturn`
      fmap mVersion migrationGroup

  it "applyMigrationGroup updates the schema" $ withDB $
    mapM_ assertTableExists ["stuff", "thang"]

  it "applyMigrationGroup throws if it is already applied" $ withDB $
    shouldThrow (applyMigrationGroup mode Nothing migrationGroup) $
      \(e :: MigrationException) -> e == MigrationAlreadyApplied stuffVersion
  it "applyMigrationGroup stores the pitb for all migrations in the group" $ withDB $ do
    resetMigrations

    let thePitb = "backup-key"

    applicationId <- applyMigrationGroup mode (Just thePitb) migrationGroup

    forM_ (fmap mVersion migrationGroup) $ \version ->
      pitbToUndoMigration version `shouldReturn` thePitb

  it "oldestMigrationPitb returns only pitb when there is one version" $ withDB $ do
    resetMigrations

    let thePitb = "backup-key"

    applicationId <- applyMigrationGroup mode (Just thePitb) migrationGroup

    oldestMigrationPitb (fmap mVersion migrationGroup) `shouldReturn` thePitb

  it "oldestMigrationPitb throws if invalid version is used" $ withDB $
    shouldThrow (oldestMigrationPitb $ pure [utcIso8601| 2008-12-02 |]) $
      \(e :: MigrationException) -> e == InvalidMigrationVersion [[utcIso8601| 2008-12-02 |]]


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
       , "pitb"
       , "created_at"
       ] :: [String]

  sort columnNames `shouldBe` sort expected


spec :: Spec
spec = describe "Db" $ do
  withTestDB $ describe "dev setup" $ do
    it "getAppliedMigrations fails if db is not setup" $ withDB $
      shouldThrow getAppliedMigrations $ \(e :: Psql.SqlError) -> e == migrationTableMissing

  withTestDB $ describe "dev setup" $ do
    it "setupDev works" $ withDB $ do
      setupDev

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

    it "setupDev throws if applied a second time" $ withDB $
      shouldThrow setupDev $ \(e :: Psql.SqlError) -> e == applicationsAlreadyExists

    it "insertDevMigration/getDevMigrationRow roundtrips basically" $ withDB $ do
      applicationId <- createApplication Nothing
      now <- liftIO getCurrentTime

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
            , mrCreatedAt = now
            , mrHash = hashMigration initial
            , mrApplicationId = applicationId
            }

      insertDevMigration applicationId initial

      actualRow <- getDevMigrationRow theVersion

      mrVersion actualRow `shouldBe` mrVersion expected
      mrName actualRow `shouldBe` mrName expected
      shouldSatisfy (mrCreatedAt actualRow `diffUTCTime` mrCreatedAt expected) (< 100)
      mrHash actualRow `shouldBe` mrHash expected
      mrApplicationId actualRow `shouldBe` mrApplicationId expected

      clearMigrations

    sharedSpecs Dev

  withTestDB $ describe "prod setup" $ do
    it "setupProd works" $ withDB $ do
      setupProd

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
           , "application_id"
           ] :: [String]

      sort columnNames `shouldBe` sort expected

      diffProdSetup `shouldReturn` mempty

    it "setupProd throws if applied a second time" $ withDB $
      shouldThrow setupProd $ \(e :: Psql.SqlError) -> e == applicationsAlreadyExists

    it "insertProdMigration/getProdMigrationRow roundtrips basically" $ withDB $ do
      applicationId <- createApplication Nothing
      now <- liftIO getCurrentTime

      let theVersion = [utcIso8601| 2048-12-01 |]
          theName = "initial"

          initial = Migration
            { mVersion = theVersion
            , mName = theName
            , mQuery = "hey"
            }

          expected = ProdMigrationRow
            { pmrVersion = theVersion
            , pmrName = theName
            , pmrCreatedAt = now
            , pmrApplicationId = applicationId
            }

      insertProdMigration applicationId initial

      actualRow <- getProdMigrationRow theVersion

      pmrVersion actualRow `shouldBe` pmrVersion expected
      pmrName actualRow `shouldBe` pmrName expected
      shouldSatisfy (pmrCreatedAt actualRow `diffUTCTime` pmrCreatedAt expected) (< 100)
      pmrApplicationId actualRow `shouldBe` pmrApplicationId expected

      clearMigrations

    it "applyMigrationGroup/getAllProdMigrations" $ withDB $ do
      _ <- applyMigrationGroup Prod Nothing migrationGroup
      getAllProdMigrations `shouldReturn`
        [ ProdMigration
          { pmVersion = [utcIso8601| 2048-12-01 |]
          , pmName = "stuff"
          }
        , ProdMigration
          { pmVersion = [utcIso8601| 2048-12-02 |]
          , pmName = "thang"
          }
        ]

    it "getApplicationRecords also works" $ withDB $ do
      getAllApplicationRecords `shouldReturn` [ProdApplicationRecord Nothing
          [ ProdMigration
            { pmVersion = [utcIso8601| 2048-12-01 |]
            , pmName = "stuff"
            }
          , ProdMigration
            { pmVersion = [utcIso8601| 2048-12-02 |]
            , pmName = "thang"
            }
          ]
        ]

      resetMigrations

    sharedSpecs Prod