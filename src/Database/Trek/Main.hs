module Database.Trek.Main where

import Database.Trek.Types
import Database.Trek.Db
import Database.Trek.Dump

import Data.Text (Text)
import Data.Foldable

qaModeMigrationsToApply
  :: Config
  -> Maybe Dump
  -> [(FilePath, Version, Hash)]
  -> [(Version, Maybe Hash)]
  -> IO [FilePath]
qaModeMigrationsToApply Config {..} _mDump allMigrations appliedMigrations = do
  -- TODO find all of the hash conflicts
  case findAllHashConflicts allMigrations appliedMigrations of
    [] -> pure $ map fst $ diffToUnappliedMigrations (map drop3 allMigrations) $ map fst appliedMigrations
    xs -> do
      cRollbacker =<< runDb cPool (oldestMigrationPitb xs)
      newAppliedVersions <- runDb cPool getAppliedVersions
      pure $ map fst $ diffToUnappliedMigrations (map drop3 allMigrations) $
        newAppliedVersions

-- Throws FailedToParseMigratrionFilePaths
readMigrations :: FilePath -> IO [(FilePath, Version, Hash)]
readMigrations = undefined

diffToUnappliedMigrations :: [(FilePath, Version)] -> [Version] -> [(FilePath, Version)]
diffToUnappliedMigrations = undefined

loadMigration :: FilePath -> IO Migration
loadMigration = undefined

parseVersionName :: FilePath -> Either String (Text, Version)
parseVersionName = undefined

hashConflicts :: [(Version, Hash)] -> [(Version, Maybe Hash)] -> [Version]
hashConflicts = undefined

drop3 :: (a, b, c) -> (a, b)
drop3 = undefined

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
  -> [(FilePath, Version, Hash)]
  -> [(Version, Maybe Hash)]
  -> IO [FilePath]
determineMigrationsToApply config mode mDump allMigrations appliedMigrations = case mode of
  Dev  -> devModeMigrationsToApply config mDump allMigrations appliedMigrations
  Qa   -> qaModeMigrationsToApply config mDump allMigrations appliedMigrations
  Prod -> pure $ prodModeMigrationsToApply allMigrations appliedMigrations

prodModeMigrationsToApply
  :: [(FilePath, Version, Hash)]
  -> [(Version, Maybe Hash)]
  -> [FilePath]
prodModeMigrationsToApply allMigrations appliedMigrations =
  map fst $ diffToUnappliedMigrations (map drop3 allMigrations) $ map fst appliedMigrations

findAllHashConflicts
  :: [(FilePath, Version, Hash)]
  -> [(Version, Maybe Hash)]
  -> [Version]
findAllHashConflicts _allMigrations _appliedMigrations = undefined

resetDb :: Config -> Maybe Dump -> [(FilePath, Version, Hash)] -> IO [FilePath]
resetDb Config {..} mDump allMigrations = do
  runDb cPool cDropDb
  for_ mDump $ applySchema Dev
  newAppliedVersions <- runDb cPool getAppliedVersions
  pure $ map fst $ diffToUnappliedMigrations (map drop3 allMigrations) newAppliedVersions


hasHashConflict
  :: [(FilePath, Version, Hash)]
  -> [(Version, Maybe Hash)]
  -> Bool
hasHashConflict = undefined
-- Dev mode will drop the db if the hash is wrong and apply all the migrations again
-- needs the optional schema migrations
devModeMigrationsToApply
  :: Config
  -> Maybe Dump
  -> [(FilePath, Version, Hash)]
  -> [(Version, Maybe Hash)]
  -> IO [FilePath]
devModeMigrationsToApply config mDump allMigrations appliedMigrations = do
  if hasHashConflict allMigrations appliedMigrations
    then resetDb config mDump allMigrations
    else pure $ map fst $ diffToUnappliedMigrations (map drop3 allMigrations) $ map fst appliedMigrations

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

  unappliedMigrationFilePaths <-
    determineMigrationsToApply config mode mSchema allMigrations appliedMigrations

  unappliedMigrations <- mapM loadMigration unappliedMigrationFilePaths

  runDb cPool $ mapM_ applyMigration unappliedMigrations

data RunConfig = RunConfig

run :: RunConfig -> IO ()
run = undefined

parseArgs :: IO RunConfig
parseArgs = undefined

main :: IO ()
main = run =<< parseArgs