module Database.Trek where
import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Transact
import Data.Time
import Data.Text (Text)
-- import Database.PostgreSQL.Simple.Options
import Data.Pool
import Data.Foldable
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)

type Version = UTCTime

type PointInTime = String

type Hash = ByteString

data MigrationExceptions
  = FailedToParseMigratrionFilePaths [FilePath]

data Mode = Dev | Qa | Prod

data Config = Config
  { cMigrationDirectory :: FilePath
  , cSchemaFilePath :: FilePath
  , cPool :: Pool PS.Connection
  , cRollbacker :: PointInTime -> IO ()
  , cBackerUpper :: IO PointInTime
  , cDropDb :: DB ()
  }

data Dump = Dump
  { dSchema     :: PS.Query
  , dAppliedMigrations :: NonEmpty [ProdMigration]
  }

data Migration = Migration
  { mVersion :: Version
  , mName :: Text
  , mQuery :: PS.Query
  }

{-
    How to determine that migrations do not need to be applied if a dump is used?
    If you use a dump then you should not apply those migrations
    but they are not in the migration table
    if you try to apply them they could fail
    the dump does not have the data you need
    Does it need the migration table dump as well?
    that would solve the problem
-}

{-
    The logic for a hash changing is to find the application it is part of
    and then remove all of the migrations that are associated with the application and any
    after it.

    it needs to store in the database that it should revert the db to a certain pitb at that point
-}

-- Only exists in dev mode
revertSaga :: PS.Query
revertSaga = [sql|
CREATE TABLE revert_saga
( pitb text NOT NULL
);

|]

auditLog :: PS.Query
auditLog = [sql|
CREATE TABLE applications
( id SERIAL PRIMARY KEY
, pitb text
, create TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT clock_timestamp()
, before int REFERENCES applications
);
  |]

data ProdMigration = ProdMigration
  { pmVersion :: Version
  , pmName :: Text
  , pmCreate :: UTCTime
  }

migrationProduction :: PS.Query
migrationProduction = [sql|
CREATE TABLE migrations
( version TIMESTAMP PRIMARY KEY
, name text NOT NULL
, create TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT clock_timestamp()
, application_id int REFERENCES applications NOT NULL ON DELETE CASCADE
);
|]

data DevMigration = DevMigration
  { dmVersion :: Version
  , dmName :: Text
  , dmCreate :: UTCTime
  , dmHash :: Maybe Hash
  }

migrationDev :: PS.Query
migrationDev = [sql|
CREATE TABLE migrations
( version TIMESTAMP NOT NULL
, name text NOT NULL
, create TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT clock_timestamp()
, hash text
, application_id int REFERENCES applications NOT NULL ON DELETE CASCADE
);
|]

setupDev :: DB ()
setupDev = void $ execute_ migrationDev

getAppliedVersions :: DB [Version]
getAppliedVersions = undefined

-- Throws FailedToParseMigratrionFilePaths
readMigrations :: FilePath -> IO [(FilePath, Version, Hash)]
readMigrations = undefined

diffToUnappliedMigrations :: [(FilePath, Version)] -> [Version] -> [(FilePath, Version)]
diffToUnappliedMigrations = undefined

loadMigration :: FilePath -> IO Migration
loadMigration = undefined

parseVersionName :: FilePath -> Either String (Text, Version)
parseVersionName = undefined

loadSchema :: FilePath -> IO (Maybe Dump)
loadSchema = undefined

dumpSchema :: Config -> IO ()
dumpSchema = undefined

-- In qa mode we care about whether there is a diff between the current and new schema
-- if there is we can't just use the new one because we might not be able to load the data
-- if there is a difference we can present it
-- we can then dump the data and try to reload it
-- we can also, apply a migration, dump and reload?
applySchema :: Mode -> Dump -> IO ()
applySchema mode dump = undefined

runDb :: Pool PS.Connection -> DB a -> IO a
runDb = undefined

applyMigration :: Migration -> DB ()
applyMigration = undefined

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
findAllHashConflicts allMigrations appliedMigrations = undefined

oldestMigrationPitb :: [Version] -> DB PointInTime
oldestMigrationPitb = undefined

qaModeMigrationsToApply
  :: Config
  -> Maybe Dump
  -> [(FilePath, Version, Hash)]
  -> [(Version, Maybe Hash)]
  -> IO [FilePath]
qaModeMigrationsToApply Config {..} mDump allMigrations appliedMigrations = do
  -- TODO find all of the hash conflicts
  case findAllHashConflicts allMigrations appliedMigrations of
    [] -> pure $ map fst $ diffToUnappliedMigrations (map drop3 allMigrations) $ map fst appliedMigrations
    xs -> do
      cRollbacker =<< runDb cPool (oldestMigrationPitb xs)
      newAppliedVersions <- runDb cPool getAppliedVersions
      pure $ map fst $ diffToUnappliedMigrations (map drop3 allMigrations) $
        newAppliedVersions

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

getAppliedMigrations :: DB [(Version, Maybe Hash)]
getAppliedMigrations = undefined

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


