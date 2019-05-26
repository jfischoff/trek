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

spec :: Spec
spec = describe "Core" $ do
  withTestDB $ do
    describe "setup" $ do
      it "is valid" $ withDB $ do
        setup
        diffSetup `shouldReturn` mempty

      it "throws if run twice" $ withDB $
        shouldThrow setup $ \SetupRanTwice -> True

      it "teardown removes all the tables" $ withDB $ do
        teardown
        diffSetup `shouldReturn` Db.cleanSchemaDiff Prod

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

        migrate migrations

        Db.tableExists "stuff" `shouldReturn` True
        Db.tableExists "thang" `shouldReturn` True

        Db.getAllApplicationRecords `shouldReturn` [Db.ProdApplicationRecord Nothing
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

      it "running runMigration with a new migration and old ones applies the new one" $ withDB $ do
        let migrations = [stuffMigration, thangMigration, Migration
              { mVersion = [utcIso8601| 2048-12-03 |]
              , mName = "foo"
              , mQuery = "CREATE TABLE foo (id SERIAL PRIMARY KEY);"
              }]

        migrate migrations

        Db.tableExists "foo" `shouldReturn` True

        Db.getAllApplicationRecords `shouldReturn`
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

      it "running runMigration with just a new migration works" $ withDB $ do
        let migrations = [Migration
              { mVersion = [utcIso8601| 2048-12-04 |]
              , mName = "bar"
              , mQuery = "CREATE TABLE bar (id SERIAL PRIMARY KEY);"
              }]

        migrate migrations

        Db.tableExists "bar" `shouldReturn` True

        Db.getAllApplicationRecords `shouldReturn`
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