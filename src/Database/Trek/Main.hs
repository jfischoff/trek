module Database.Trek.Main where

import Database.Trek.Types
import Database.Trek.Db
import Database.Trek.Dump
import Database.Trek.Run

import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable
import Data.Pool
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Transact
import qualified Data.List.NonEmpty as NonEmpty
import Data.Time
import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import System.FilePath.Posix
import Control.Exception
import Data.List.Split
import System.Directory
import Data.Maybe
import qualified Data.Map.Strict as Map
import Control.Arrow
import qualified Data.Set as Set
import Control.Monad (join)
import qualified Database.PostgreSQL.Simple.Options as PS

createFileName :: Text -> UTCTime -> FilePath
createFileName name version = T.unpack name ++ "_" ++ formatTime defaultTimeLocale dateFormat version

parseVersionName :: FilePath -> Either String (Text, Version)
parseVersionName filePath = case splitOn "_" filePath of
  [namePart, datePart] -> fmap (T.pack namePart,) $ case parseTimeM False defaultTimeLocale dateFormat datePart of
    Nothing -> Left $ "Failed to parse " ++ show datePart ++ " as an ISO 8601 date"
    Just date -> pure date
  [] -> Left "Empty file path!"
  [x] -> Left $ "Was unable to split " ++ show filePath ++ " on '_'"
  xs -> Left $ "Split by 'x' gave too many parts with "
      ++ show filePath
      ++ ". Expected one '-' but found many"

loadMigration :: FilePath -> IO Migration
loadMigration filePath = do
  mQuery <- BS.readFile filePath
  let fileName = dropExtension $ takeBaseName filePath
  (mName, mVersion) <- either (throwIO . FailedToParseMigratrionFilePaths . pure) pure $
    parseVersionName fileName
  pure Migration {..}

-- Throws FailedToParseMigratrionFilePaths
readMigrations :: FilePath -> IO [Migration]
readMigrations directoryPath = do
  files <- listDirectory directoryPath
  mapM (loadMigration . (\x -> directoryPath ++ "/" ++ x)) files

data RunConfig = RunConfig

run :: RunConfig -> IO ()
run = undefined

parseArgs :: IO RunConfig
parseArgs = undefined

main :: IO ()
main = run =<< parseArgs