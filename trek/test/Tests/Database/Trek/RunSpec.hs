module Tests.Database.Trek.RunSpec where
import Database.Trek.Run
import Test.Hspec
import Data.Maybe
import Data.List.Split
import Data.Time.Format.ISO8601
import System.IO.Temp
import System.Directory
import Control.Exception
import Data.Time

spec :: Spec
spec = describe "Database.Trek.Parser" $ do
  it "creates a file" $ do
    withSystemTempDirectory "trek-test" $ \tmp -> do
      old <- getCurrentDirectory
      bracket_ (setCurrentDirectory tmp) (setCurrentDirectory old) $ do
        let name = "migration"
        output <- create name
        let [actualName, date] = splitOn "_" output
        actualName `shouldBe` name
        isJust (iso8601ParseM date :: Maybe UTCTime) `shouldBe` True
        doesFileExist output `shouldReturn` True
