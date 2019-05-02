module Tests.Database.Trek.RunSpec where

import Database.Trek.Run
import Database.Trek.Types
import Tests.Database.Trek.DbUtils
import qualified Database.Trek.Db as Db
import Tests.Database.Trek.TestUtils
import Data.Foldable
import Control.Concurrent
import qualified Database.Postgres.Temp.Internal as Temp
import qualified Database.PostgreSQL.Simple.Options as PS
import Data.Time.QQ
import Test.Hspec
import Data.Text (Text)
import Data.ByteString (ByteString)

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

main :: IO ()
main = hspec spec

withConfig :: (Config -> IO ()) -> IO ()
withConfig f = withTestDBAndDirectory "trek-run-spec" $ \(conn, db, tmpDir) -> do
  cConnection <- newMVar conn
  let cSchemaFilePath = tmpDir ++ "/dump"
      cRollbacker = error "rollback not implemented"
      cBackerUpper = Nothing
      Right partial = PS.parseConnectionString $ Temp.connectionString db
      Right cDbOptions = PS.completeOptions partial
      cQaBackupDir = tmpDir ++ "/backup"

  f Config {..}

withTestConfig :: SpecWith Config -> Spec
withTestConfig = aroundAll withConfig

spec :: Spec
spec = describe "Run" $ do
  it "hashConflicts works" $ do
    hashConflicts [] [] `shouldBe` []
    hashConflicts [([utcIso8601| 2048-12-02 |],"a")] [] `shouldBe` []

    hashConflicts
      [([utcIso8601| 2048-12-02 |],"a")] [([utcIso8601| 2048-12-02 |], Just "a")]
        `shouldBe` []

    hashConflicts
      [([utcIso8601| 2048-12-02 |],"a")] [([utcIso8601| 2048-12-02 |], Just "b")]
        `shouldBe` [[utcIso8601| 2048-12-02 |]]

    hashConflicts
      [([utcIso8601| 2048-12-02 |],"a")] [([utcIso8601| 2048-12-03 |], Just "b")]
        `shouldBe` []

  it "filterByVersions works" $ do
    filterByVersions [] [] `shouldBe` []
    filterByVersions [stuffMigration] [thangVersion] `shouldBe` []
    filterByVersions [stuffMigration, thangMigration] [thangVersion] `shouldBe` [thangMigration]

  it "diffToUnappliedMigrations" $ do
    diffToUnappliedMigrations [] [] `shouldBe` []
    diffToUnappliedMigrations [[utcIso8601| 2048-12-02 |]] [] `shouldBe` [[utcIso8601| 2048-12-02 |]]
    diffToUnappliedMigrations [[utcIso8601| 2048-12-02 |]] [[utcIso8601| 2048-12-02 |]] `shouldBe` []

    diffToUnappliedMigrations
      [[utcIso8601| 2048-12-02 |]]
      [[utcIso8601| 2048-12-02 |], [utcIso8601| 2048-12-03 |]]
        `shouldBe` []

    diffToUnappliedMigrations
      [[utcIso8601| 2048-12-02 |], [utcIso8601| 2048-12-03 |]]
      [[utcIso8601| 2048-12-02 |]]
        `shouldBe` [[utcIso8601| 2048-12-03 |]]

  withTestConfig $ do
    forM_ [minBound .. maxBound] $ \mode -> describe ("setup mode " ++ show mode ) $ do
      it "is valid" $ \config -> do
        setup config mode
        diffSetup config mode `shouldReturn` mempty

      it "throws if run twice" $ \config -> do
        shouldThrow (setup config mode) $ \SetupRanTwice -> True

      it "teardown removes all the tables" $ \config -> do
        teardown config
        diffSetup config mode `shouldReturn` Db.cleanSchemaDiff mode

    describe "runMigration" $ do
      it "adds new tables and updates the migration table" $ \config -> do
        let mode = Dev
        setup config mode
        let migrations = [stuffMigration, thangMigration]

        runDb config (Db.tableExists "stuff") `shouldReturn` False
        runDb config (Db.tableExists "thang") `shouldReturn` False

        runMigration config mode migrations

        runDb config (Db.tableExists "stuff") `shouldReturn` True
        runDb config (Db.tableExists "thang") `shouldReturn` True

      it "listApplications shows the application" $ \config -> do
        listApplications config `shouldReturn` [Db.ProdApplicationRecord Nothing
            [ Db.ProdMigration
              { pmVersion = [utcIso8601| 2048-12-01 |]
              , pmName = "stuff"
              }
            , Db.ProdMigration
              { pmVersion = [utcIso8601| 2048-12-02 |]
              , pmName = "thang"
              }
            ]
          ]

      it "call runMigration twice does nothing" $ \config -> do
        let mode = Dev
        let migrations = [stuffMigration, thangMigration]

        runDb config (Db.tableExists "stuff") `shouldReturn` True
        runDb config (Db.tableExists "thang") `shouldReturn` True

        runMigration config mode migrations

        runDb config (Db.tableExists "stuff") `shouldReturn` True
        runDb config (Db.tableExists "thang") `shouldReturn` True

        listApplications config `shouldReturn` [Db.ProdApplicationRecord Nothing
            [ Db.ProdMigration
              { pmVersion = [utcIso8601| 2048-12-01 |]
              , pmName = "stuff"
              }
            , Db.ProdMigration
              { pmVersion = [utcIso8601| 2048-12-02 |]
              , pmName = "thang"
              }
            ]
          ]