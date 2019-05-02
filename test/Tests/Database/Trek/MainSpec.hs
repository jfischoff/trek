module Tests.Database.Trek.MainSpec where

import Test.Hspec
import Tests.Database.Trek.DbUtils
import Tests.Database.Trek.TestUtils
import Database.Trek.Main
import Database.Trek.Types

import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Async (Async)
import Control.Concurrent
import Control.Monad (forever)
import Data.Time
import Data.Time.Format
import Data.Time.QQ
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Text (Text)
import Data.List

main :: IO ()
main = hspec spec

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

spec :: Spec
spec = describe "Main" $ do
  withTempDir "trek-main" $ describe "Migration File Operations" $ do
    it "readMigrations gives empty to start" $ \fileDirectory -> do
      readMigrations fileDirectory `shouldReturn` []

    it "save/load migration" $ \fileDirectory -> do
      let fileName = createFileName stuffName stuffVersion ++ ".sql"
          filePath = fileDirectory ++ "/" ++ fileName
      BS.writeFile filePath stuffQuery

      loadMigration filePath `shouldReturn` stuffMigration

    it "readMigrations gives stuff after stuff is written" $ \fileDirectory -> do
      readMigrations fileDirectory `shouldReturn` [stuffMigration]

    it "readMigrations gives stuff and thang after both are written" $ \fileDirectory -> do

      let fileName = createFileName thangName thangVersion ++ ".sql"
          filePath = fileDirectory ++ "/" ++ fileName
      BS.writeFile filePath thangQuery

      loadMigration filePath `shouldReturn` thangMigration

      fmap sort (readMigrations fileDirectory) `shouldReturn` sort [stuffMigration, thangMigration]

  --
  withTestDB $ describe "prod setup" $ do
    -- can it create the db?
    -- maybe?
    -- Let's assume it cannot for now
    --
    -- test one
    -- setup works
    -- fails if run twice
    --
    -- runMigration after setup works
    --
    -- Should I change runMigration to work with a list of migrations?
    -- seems so

    pure ()