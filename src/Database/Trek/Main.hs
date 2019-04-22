module Database.Trek.Main where

import Database.Trek.Types
import Database.Trek.Db
import Database.Trek.Dump

import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable
import Data.Pool
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Transact
import qualified Data.List.NonEmpty as NonEmpty
import Data.Time
import qualified Data.ByteString as BS
import System.FilePath.Posix
import Control.Exception
import Data.List.Split
import System.Directory

data Config = Config
  { cMigrationDirectory :: FilePath
  , cSchemaFilePath :: FilePath
  , cPool :: Pool PS.Connection
  , cRollbacker :: PointInTime -> IO ()
  , cBackerUpper :: Maybe (IO PointInTime)
  , cDropDb :: DB ()
  }

filterByVersions :: [Migration] -> [Version] -> [Migration]
filterByVersions = undefined

qaModeMigrationsToApply
  :: Config
  -> Maybe Dump
  -> [Migration]
  -> [(Version, Maybe Hash)]
  -> IO [Migration]
qaModeMigrationsToApply Config {..} _mDump allMigrations appliedMigrations = do
  -- TODO find all of the hash conflicts
  filterByVersions allMigrations <$> case findAllHashConflicts allMigrations appliedMigrations of
    [] -> pure $ diffToUnappliedMigrations (map mVersion allMigrations) $ map fst appliedMigrations
    x:xs -> do
      cRollbacker =<< runDb cPool (oldestMigrationPitb $ x NonEmpty.:| xs)
      newAppliedVersions <- runDb cPool getAppliedVersions
      pure $ diffToUnappliedMigrations (map mVersion allMigrations) $
        newAppliedVersions

-- Throws FailedToParseMigratrionFilePaths
readMigrations :: FilePath -> IO [Migration]
readMigrations directoryPath = do
  files <- listDirectory directoryPath
  mapM (loadMigration . (\x -> directoryPath ++ "/" ++ x)) files

diffToUnappliedMigrations :: [Version] -> [Version] -> [Version]
diffToUnappliedMigrations = undefined

loadMigration :: FilePath -> IO Migration
loadMigration filePath = do
  mQuery <- BS.readFile filePath
  let fileName = dropExtension $ takeBaseName filePath
  (mName, mVersion) <- either (throwIO . FailedToParseMigratrionFilePaths . pure) pure $
    parseVersionName fileName
  pure Migration {..}

dateFormat :: String
dateFormat = iso8601DateFormat (Just "%H-%M-%S")

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

hashConflicts :: [(Version, Hash)] -> [(Version, Maybe Hash)] -> [Version]
hashConflicts = undefined


-- This is a complicated function
-- The logic is simple for production
-- but for dev/qa it is much different.
-- I think there are really three modes
-- If it dev you just blow away the db
-- If it is QA you go to a pitb
-- If it is production now backups
determineMigrationsToApply
  :: Config
  -> Mode
  -> Maybe Dump
  -> [Migration]
  -> [(Version, Maybe Hash)]
  -> IO [Migration]
determineMigrationsToApply config mode mDump allMigrations appliedMigrations = case mode of
  Dev  -> devModeMigrationsToApply config mDump allMigrations appliedMigrations
  Qa   -> qaModeMigrationsToApply config mDump allMigrations appliedMigrations
  Prod -> pure $ prodModeMigrationsToApply allMigrations appliedMigrations

prodModeMigrationsToApply
  :: [Migration]
  -> [(Version, Maybe Hash)]
  -> [Migration]
prodModeMigrationsToApply allMigrations appliedMigrations =
  filterByVersions allMigrations $ diffToUnappliedMigrations (map mVersion allMigrations) $ map fst appliedMigrations

findAllHashConflicts
  :: [Migration]
  -> [(Version, Maybe Hash)]
  -> [Version]
findAllHashConflicts _allMigrations _appliedMigrations = undefined

resetDb :: Config -> Maybe Dump -> [Migration] -> IO [Migration]
resetDb Config {..} mDump allMigrations = do
  runDb cPool cDropDb
  for_ mDump $ applySchema Dev
  newAppliedVersions <- runDb cPool getAppliedVersions
  pure $ filterByVersions allMigrations $ diffToUnappliedMigrations (map mVersion allMigrations) newAppliedVersions


hasHashConflict
  :: [Migration]
  -> [(Version, Maybe Hash)]
  -> Bool
hasHashConflict = undefined
-- Dev mode will drop the db if the hash is wrong and apply all the migrations again
-- needs the optional schema migrations
devModeMigrationsToApply
  :: Config
  -> Maybe Dump
  -> [Migration]
  -> [(Version, Maybe Hash)]
  -> IO [Migration]
devModeMigrationsToApply config mDump allMigrations appliedMigrations = do
  if hasHashConflict allMigrations appliedMigrations
    then resetDb config mDump allMigrations
    else pure $ filterByVersions allMigrations $ diffToUnappliedMigrations (map mVersion allMigrations) $ map fst appliedMigrations

-- TODO I need a dry run capability
-- I should be able to ignore the dump
-- I should be able to automatically create a dump
-- Should it take a backup?
-- The migration should check hashes


runMigration :: Config -> Mode -> IO ()
runMigration config@Config {..} mode = do
  -- Load the dump if it is there and has not been loaded before
  -- TODO this is wrong. You can't just load a dump. You can only do this
  -- if the db is empty or if the dump is new. Basically you do this if
  -- the dump is new.
  -- I'm not totally sure but I think what this means doing a logical dump
  -- I think it should do a schema dump and then diff the two schemas
  -- if they are the same
  mSchema <- loadSchema cSchemaFilePath
  for_ mSchema $ applySchema mode
  allMigrations <- readMigrations cMigrationDirectory

  appliedMigrations <- runDb cPool getAppliedMigrations

  unappliedMigrations <-
    determineMigrationsToApply config mode mSchema allMigrations appliedMigrations

  pitb <- sequenceA cBackerUpper

  for_ (NonEmpty.nonEmpty unappliedMigrations) $
    runDb cPool . applyMigrationGroup mode pitb

data RunConfig = RunConfig

run :: RunConfig -> IO ()
run = undefined

parseArgs :: IO RunConfig
parseArgs = undefined

main :: IO ()
main = run =<< parseArgs