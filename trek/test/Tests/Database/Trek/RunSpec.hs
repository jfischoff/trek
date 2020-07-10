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

spec :: Spec
spec = describe "Database.Trek.Parser" $ do
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
