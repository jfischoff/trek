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
import qualified Database.PostgreSQL.Simple as PS
import Data.Time.QQ
import Test.Hspec
import Data.Text (Text)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BSC
import System.Directory
import System.Process
import System.Exit

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

-- testRollbacker :: String -> PointInTime -> IO ()
-- testRollbacker db

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

  forM_ [minBound .. maxBound] $ \mode -> withTestConfig $ do
    describe ("setup mode " ++ show mode ) $ do
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

      it "running runMigration with a new migration and old ones applies the new one" $ \config -> do
        let migrations = [stuffMigration, thangMigration, Migration
              { mVersion = [utcIso8601| 2048-12-03 |]
              , mName = "foo"
              , mQuery = "CREATE TABLE foo (id SERIAL PRIMARY KEY);"
              }]

        runMigration config mode migrations

        runDb config (Db.tableExists "foo") `shouldReturn` True

        listApplications config `shouldReturn`
          [ Db.ProdApplicationRecord Nothing
              [ Db.ProdMigration
                { pmVersion = [utcIso8601| 2048-12-01 |]
                , pmName = "stuff"
                }
              , Db.ProdMigration
                { pmVersion = [utcIso8601| 2048-12-02 |]
                , pmName = "thang"
                }
              ]
          , Db.ProdApplicationRecord Nothing
              [ Db.ProdMigration
                { pmVersion = [utcIso8601| 2048-12-03 |]
                , pmName = "foo"
                }
              ]
          ]

      it "running runMigration with just a new migration works" $ \config -> do
        let migrations = [Migration
              { mVersion = [utcIso8601| 2048-12-04 |]
              , mName = "bar"
              , mQuery = "CREATE TABLE bar (id SERIAL PRIMARY KEY);"
              }]

        runMigration config mode migrations

        runDb config (Db.tableExists "bar") `shouldReturn` True

        listApplications config `shouldReturn`
          [ Db.ProdApplicationRecord Nothing
              [ Db.ProdMigration
                { pmVersion = [utcIso8601| 2048-12-01 |]
                , pmName = "stuff"
                }
              , Db.ProdMigration
                { pmVersion = [utcIso8601| 2048-12-02 |]
                , pmName = "thang"
                }
              ]
          , Db.ProdApplicationRecord Nothing
              [ Db.ProdMigration
                { pmVersion = [utcIso8601| 2048-12-03 |]
                , pmName = "foo"
                }
              ]
          , Db.ProdApplicationRecord Nothing
              [ Db.ProdMigration
                { pmVersion = [utcIso8601| 2048-12-04 |]
                , pmName = "bar"
                }
              ]
          ]

  -- To do this test I need a way to stop postgres without removing the tmp folder
  -- I can stop the process
  -- and then adjust the folder
  -- and then restart it
  describe "QA Mode" $ withTestDbAndDirSpec "qa-mode" $ do
    describe "pitr" $ do
      it "works in general" $ \(conn, db, tmpDir) -> do
        let mainFilePath = Temp.mainDir db
            dataDir = mainFilePath ++ "/data"
            walArchiveDir = mainFilePath ++ "/archive"
            baseBackupFile = mainFilePath ++ "/backup"

        appendFile (dataDir ++ "/pg_hba.conf") $ "local replication all trust"
        let archiveLine = "archive_command = " ++
              "'test ! -f " ++ walArchiveDir ++ "/%f && cp %p " ++ walArchiveDir ++ "/%f'\n"

        appendFile (dataDir ++ "/postgresql.conf") $ archiveLine

        createDirectory walArchiveDir

        Temp.reloadConfig db

        res <- system ("pg_basebackup -D " ++ baseBackupFile ++ " --format=tar -p" ++ show (Temp.port db) ++ " -h" ++ mainFilePath)
        res `shouldBe` ExitSuccess

        -- setup the config to take backups and restore
        cConnection <- newMVar conn
        let cSchemaFilePath = tmpDir ++ "/dump"
            cBackerUpper = Just $ defaultBackerUpper cConnection
            Right partial = PS.parseConnectionString $ Temp.connectionString db
            Right cDbOptions = PS.completeOptions partial
            cQaBackupDir = tmpDir ++ "/backup"

            cRollbacker = \pitrLabel -> do
              -- need to close the connection and then reconnect ... hmm probably should be automatic?
              modifyMVar_ cConnection $ \conn -> do
                putStrLn "rolling back"
                PS.close conn
                Temp.stopPostgres db `shouldReturn` Just ExitSuccess

                removeDirectoryRecursive dataDir
                createDirectory dataDir
                let untarCommand = "tar -C" ++ dataDir ++ " -xf " ++ baseBackupFile ++ "/base.tar"
                _ <- system untarCommand
                _ <- system ("chmod -R 700 " ++ dataDir)
                writeFile (dataDir ++ "/recovery.conf") $ "recovery_target_name='" ++ pitrLabel ++ "'\nrecovery_target_inclusive=true\nrestore_command='"
                  ++ "cp " ++ walArchiveDir ++ "/%f %p'"

                Temp.startPostgres db

                PS.connectPostgreSQL $ BSC.pack $ Temp.connectionString db

            config = Config {..}

        -- Setup for QA mode
        setup config Qa

        runDb config (Db.tableExists "stuff") `shouldReturn` False

        runMigration config Qa [stuffMigration]

        runDb config (Db.tableExists "stuff") `shouldReturn` True

        runMigration config Qa [stuffMigration
          { mQuery = "CREATE TABLE stuffy (id SERIAL PRIMARY KEY)"
          }]

        runDb config (Db.tableExists "stuff") `shouldReturn` False
        runDb config (Db.tableExists "stuffy") `shouldReturn` True

