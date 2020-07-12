module Tests.Database.Trek.RunSpec where
import Database.Trek.Run
import Test.Hspec
import Data.Maybe
import Data.List.Split
import Data.Time.Format
import System.IO.Temp
import System.Directory
import Control.Exception
import Data.Time
import System.FilePath.Posix
import Data.List.NonEmpty (NonEmpty)

foo :: String
foo = "CREATE SCHEMA IF NOT EXISTS test; CREATE TABLE test.foo (id SERIAL PRIMARY KEY)"

bar :: String
bar = "CREATE SCHEMA IF NOT EXISTS test; CREATE TABLE test.bar (id SERIAL PRIMARY KEY)"

quux :: String
quux = "CREATE SCHEMA IF NOT EXISTS test; CREATE TABLE test.quux (id SERIAL PRIMARY KEY)"

inputGroup :: NonEmpty FilePath -> IO FilePath
inputGroup = error "inputGroup"

inputAction :: String -> IO ()
inputAction = undefined


spec :: Spec
spec = do
  describe "Database.Trek.Run" $ do
    it "creates a file" $ do
      withSystemTempDirectory "trek-test" $ \tmp -> do
        old <- getCurrentDirectory
        bracket_ (setCurrentDirectory tmp) (setCurrentDirectory old) $ do
          createDirectory "path"
          let name = "path/migration.sql"
          output <- create name
          let (dir, theFileName) = splitFileName output
              [date, actualName] = splitOn "_" theFileName
          dir `shouldBe` "path/"
          actualName `shouldBe` "migration.sql"
          isJust (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H-%M-%S" date :: Maybe UTCTime) `shouldBe` True
          doesFileExist output `shouldReturn` True

{-
  describe "Database.Trek.Run.apply" $ do
-}
