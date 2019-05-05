module Database.Trek.Run where

import Database.Trek.Types
import Database.Trek.Dump

import Data.Text (Text)
import qualified Data.Text as T
import Data.Foldable
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
import Control.Concurrent
import qualified Database.Trek.Db as Db
import Database.PostgreSQL.Simple.SqlQQ

runDb :: Config -> DB a -> IO a
runDb Config {..} db = withMVar cConnection $ Db.mapSqlError . runDBTSerializable db

defaultBackerUpper :: MVar PS.Connection -> IO PointInTime
defaultBackerUpper mconn = do
  pitrLabel <- formatTime defaultTimeLocale dateFormat <$> getCurrentTime
  _ :: String <- withMVar mconn $ runDBTSerializable
    ( PS.fromOnly . head <$> query [sql|
        SELECT pg_xlogfile_name(pg_create_restore_point(?))
      |] (PS.Only (pitrLabel :: String))
    )
  pure pitrLabel

data Config = Config
  { cSchemaFilePath :: FilePath
  , cConnection     :: MVar PS.Connection
  , cRollbacker     :: PointInTime -> IO ()
  , cBackerUpper    :: Maybe (IO PointInTime)
  , cDbOptions      :: PS.Options
  , cQaBackupDir    :: FilePath
  }

filterByVersions :: [Migration] -> [Version] -> [Migration]
filterByVersions migrations =
  mapMaybe (\x -> find (\m -> mVersion m == x) migrations)

qaModeMigrationsToApply
  :: Config
  -> FilePath
  -> [Migration]
  -> [(Version, Maybe Hash)]
  -> IO [Migration]
qaModeMigrationsToApply config@Config {..} filePath allMigrations appliedMigrations = do
  -- TODO find all of the hash conflicts
  filterByVersions allMigrations <$> case findAllHashConflicts allMigrations appliedMigrations of
    [] -> pure $ diffToUnappliedMigrations (map mVersion allMigrations) $ map fst appliedMigrations
    x:xs -> do
      cRollbacker =<< runDb config (Db.oldestMigrationPitb $ x NonEmpty.:| xs)
      newAppliedVersions <- runDb config Db.getAppliedVersions
      pure $ diffToUnappliedMigrations (map mVersion allMigrations) $
        newAppliedVersions

diffToUnappliedMigrations :: [Version] -> [Version] -> [Version]
diffToUnappliedMigrations allMigrations appliedMigrations
  = Set.toList
  $ Set.fromList allMigrations `Set.difference` Set.fromList appliedMigrations

dateFormat :: String
dateFormat = iso8601DateFormat (Just "%H-%M-%S")

-- First filter by versions that have a hash
hashConflicts :: [(Version, Hash)] -> [(Version, Maybe Hash)] -> [Version]
hashConflicts newVersions oldVersions =
  let hashableVersions = mapMaybe (\(x, my) -> (x,) <$> my) oldVersions
      hashableMap = Map.fromList hashableVersions
      lookupDifferentHash theMap (key, newHash) =
        case Map.lookup key theMap of
          Just existingHash
              | existingHash /= newHash -> Just key
          _ -> Nothing

  in mapMaybe (lookupDifferentHash hashableMap) newVersions

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
  -> FilePath
  -> [Migration]
  -> [(Version, Maybe Hash)]
  -> IO [Migration]
determineMigrationsToApply config mode dumpFile allMigrations appliedMigrations = case mode of
  Dev  -> devModeMigrationsToApply config dumpFile allMigrations appliedMigrations
  Qa   -> qaModeMigrationsToApply config dumpFile allMigrations appliedMigrations
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
findAllHashConflicts allMigrations appliedMigrations =
  hashConflicts (map ((mVersion *** hashMigration) . join (,)) allMigrations) appliedMigrations

resetDb :: Config -> FilePath -> [Migration] -> IO [Migration]
resetDb config@Config {..} dumpFile allMigrations = do
  applySchema cQaBackupDir cDbOptions Dev dumpFile
  newAppliedVersions <- runDb config Db.getAppliedVersions
  pure $ filterByVersions allMigrations $ diffToUnappliedMigrations (map mVersion allMigrations) newAppliedVersions

hasHashConflict
  :: [Migration]
  -> [(Version, Maybe Hash)]
  -> Bool
hasHashConflict x = not . null . findAllHashConflicts x
-- Dev mode will drop the db if the hash is wrong and apply all the migrations again
-- needs the optional schema migrations
devModeMigrationsToApply
  :: Config
  -> FilePath
  -> [Migration]
  -> [(Version, Maybe Hash)]
  -> IO [Migration]
devModeMigrationsToApply config dumpFile allMigrations appliedMigrations = do
  if hasHashConflict allMigrations appliedMigrations
    then resetDb config dumpFile allMigrations
    else pure $ filterByVersions allMigrations $ diffToUnappliedMigrations (map mVersion allMigrations) $ map fst appliedMigrations

-- TODO I need a dry run capability
-- I should be able to ignore the dump
-- I should be able to automatically create a dump
-- Should it take a backup?
-- The migration should check hashes

setup :: Config -> Mode -> IO ()
setup config = runDb config . Db.setup

diffSetup :: Config -> Mode -> IO Db.SchemaDiff
diffSetup config = runDb config . Db.diffSetup

teardown :: Config -> IO ()
teardown config = runDb config Db.teardown

runMigration :: Config -> Mode -> [Migration] -> IO ()
runMigration config@Config {..} mode allMigrations = do
  appliedMigrations <- runDb config Db.getAppliedMigrations

  unappliedMigrations <-
    determineMigrationsToApply config mode cSchemaFilePath allMigrations appliedMigrations

  pitb <- sequenceA cBackerUpper

  for_ (NonEmpty.nonEmpty unappliedMigrations) $
    runDb config . Db.applyMigrationGroup mode pitb

listApplications :: Config -> IO [Db.ProdApplicationRecord]
listApplications config = runDb config Db.getAllApplicationRecords

