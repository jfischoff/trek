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

main :: IO ()
main = hspec spec

createTempConnection :: IO (Psql.Connection, Temp.DB)
createTempConnection = do
  db <- either throwIO pure =<< Temp.start []
  let connString = Temp.connectionString db
  connection <- Psql.connectPostgreSQL $ BSC.pack connString
  return (connection, db)

-- Either run the job or not
setupDB :: IO (Psql.Connection, Temp.DB)
setupDB = do
  (connection, db) <- createTempConnection
  let url = Temp.connectionString db
  return (connection, db)

withTestDB :: SpecWith (Psql.Connection, Temp.DB) -> Spec
withTestDB = beforeAll setupDB . afterAll stopDB

stopDB :: (Psql.Connection, Temp.DB) -> IO ()
stopDB (c, x) = void $ Psql.close c >> Temp.stop x

withDB :: DB a -> (Psql.Connection, Temp.DB) -> IO a
withDB x (c, _) = runDBTSerializable x c

sharedSpecs :: Mode -> SpecWith (Psql.Connection, Temp.DB)
sharedSpecs mode = do  
  it "getAppliedMigrations returns empty if no migrations" $ withDB $
      getAppliedMigrations `shouldReturn` []

  it "applyMigration/getAppliedVersions returns the version applied" $ withDB $ do
    let theVersion = [utcIso8601| 2048-12-01 |]
      
        initial = Migration
          { mVersion = [utcIso8601| 2048-12-01 |]
          , mName = "initial"
          , mQuery = "CREATE TABLE stuff (id SERIAL PRIMARY KEY);"
          }

    applicationId <- createApplication Nothing

    applyMigration mode applicationId initial

    getAppliedMigrations `shouldReturn` [(theVersion, if mode == Dev then Just $ hashMigration initial else Nothing)]

    stuffExists <- Psql.fromOnly . head <$> query_ [sql|
      SELECT EXISTS (
      SELECT 1
      FROM   information_schema.tables 
      WHERE  table_schema = 'public'
      AND    table_name = 'stuff'
      );
      |]
    
    stuffExists `shouldBe` True

  it "applyMigration updates the schema" $ const pending
  it "applyMigration throws if it is already applied" $ const pending
  it "applyMigrationGroup applies all the migrations" $ const pending
  it "applyMigrationGroup updates the schema" $ const pending
  it "applyMigrationGroup throws if it is already applied" $ const pending
  it "applyMigrationGroup stores the pitb for all migrations in the group" $ const pending
  it "oldestMigrationPitb returns only pitb one there is one version" $ const pending
  it "oldestMigrationPitb throws if invalid version is used" $ const pending

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
    it "applyMigration/getAppliedMigrations returns a Hash and the version" $ const pending
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
    it "setupProd throws if applied a second time" $ const pending

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
    
    sharedSpecs Prod
    it "applyMigration/getAppliedMigrations returns a Nothing Hash and the version" $ const pending