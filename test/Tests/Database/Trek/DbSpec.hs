module Tests.Database.Trek.DbSpec where

import Test.Hspec
import qualified Database.Postgres.Temp as Temp
import qualified Database.PostgreSQL.Simple as Psql
import Control.Monad (void)
import Data.ByteString.Char8 as BSC
import Control.Exception

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

sharedSpecs :: SpecWith (Psql.Connection, Temp.DB)
sharedSpecs = do
  it "allMigrations returns empty if no migrations" $ const pending
  it "applyMigration/getAppliedVersions returns the version applied" $ const pending
  it "applyMigration updates the schema" $ const pending
  it "applyMigration throws if it is already applied" $ const pending
  it "applyMigrationGroup applies all the migrations" $ const pending
  it "applyMigrationGroup updates the schema" $ const pending
  it "applyMigrationGroup throws if it is already applied" $ const pending
  it "applyMigrationGroup stores the pitb for all migrations in the group" $ const pending
  it "oldestMigrationPitb returns only pitb one there is one version" $ const pending
  it "oldestMigrationPitb throws if invalid version is used" $ const pending

spec :: Spec
spec = describe "Db" $ do
  it "allMigrations fails if db is not setup" $ pending
  withTestDB $ describe "dev setup" $ do
    it "setupDev works" $ const pending
    it "setupDev throws if applied a second time" $ const pending
    sharedSpecs
    it "applyMigration/getAppliedMigrations returns a Hash and the version" $ const pending
  withTestDB $ describe "prod setup" $ do
    it "setupProd works" $ const pending
    it "setupProd throws if applied a second time" $ const pending
    sharedSpecs
    it "applyMigration/getAppliedMigrations returns a Nothing Hash and the version" $ const pending