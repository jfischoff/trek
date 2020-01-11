module Database.Trek.Db
  ( -- * Life cycle management
    setup
  , teardown
    -- * Migration
  , hashConflicts
  , apply
  -- * Queriesi
  , listApplications
  -- * Types
  , InputMigration (..)
  , Version
  , Hash
  , DB
  , OutputGroup (..)
  , InputGroup (..)
  , inputGroup
  , Time
  )
  where
import Database.PostgreSQL.Transact
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Time (UTCTime)
import Data.ByteString (ByteString)
import qualified Database.PostgreSQL.Simple as Psql
import qualified Database.PostgreSQL.Simple.FromField as Psql
import qualified Database.PostgreSQL.Simple.ToField as Psql
import qualified Database.PostgreSQL.Simple.ToRow as Psql
import qualified Database.PostgreSQL.Simple.FromRow as Psql
import Database.PostgreSQL.Simple.SqlQQ
import Control.Monad (void)
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Traversable
import Data.Foldable

type Version = UTCTime

type Hash = ByteString

type Time = UTCTime

data InputMigration = InputMigration
  { inputAction  :: String
  , inputVersion :: Version
  , inputHash    :: Hash
  }

instance Psql.ToRow InputMigration where
  toRow InputMigration {..} = [Psql.toField inputVersion, Psql.toField inputHash]

newtype GroupId = GroupId ByteString
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Psql.FromField, Psql.ToField)
    deriving (Psql.ToRow) via (Psql.Only GroupId)

instance Psql.FromRow GroupId where
  fromRow = GroupId <$> Psql.field

data OutputMigration = OutputMigration
  { omVersion :: Version
  , omHash    :: Hash
  } deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (Psql.FromRow)


data OutputGroup = OutputGroup
  { ogId         :: GroupId
  , ogCreatedAt  :: UTCTime
  , ogMigrations :: NonEmpty OutputMigration
  }
  deriving (Show, Eq)

data InputGroup = InputGroup
  { inputGroupMigrations :: NonEmpty InputMigration
  , inputGroupCreateAd :: UTCTime
  }

data GroupRow = GroupRow
  { arId         :: GroupId
  , arCreatedAt  :: UTCTime
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Psql.ToRow, Psql.FromRow)

-- InputGroup constructor
inputGroup :: NonEmpty InputMigration -> DB InputGroup
inputGroup inputGroupMigrations = do
  inputGroupCreateAd <- liftIO getCurrentTime
  InputGroup {..}

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------
onSetup :: (Bool -> Bool) -> DB a -> DB (Maybe a)
onSetup onF action = do
  setupExists <- Psql.fromOnly . head <$> query_ [sql|
    SELECT EXISTS (
      SELECT 1
      FROM   information_schema.tables
      WHERE  table_schema = 'meta'
      AND    table_name = 'applications'
    )
    |]
  if onF setupExists
    then Just <$> action
    else pure Nothing

withSetup :: DB a -> DB (Maybe a)
withSetup = onSetup id

withoutSetup :: DB a -> DB (Maybe a)
withoutSetup = onSetup not

-------------------------------------------------------------------------------
-- setup/teardown
-------------------------------------------------------------------------------

setup :: DB (Maybe ())
setup = withoutSetup $ void $ execute_ [sql|
  CREATE TABLE meta.applications
  ( id bytea PRIMARY KEY
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL
  );

  CREATE INDEX ON meta.applications (created_at);

  CREATE TABLE meta.actions
  ( version TIMESTAMP WITH TIME ZONE PRIMARY KEY
  , hash bytea NOT NULL
  , application_id bytea NOT NULL REFERENCES meta.applications ON DELETE CASCADE
  , order SERIAL;
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL
  );

  CREATE INDEX ON meta.actions (hash);
  CREATE INDEX ON meta.actions (application_id);
  CREATE INDEX ON meta.actions (order);
  CREATE INDEX ON meta.actions (created_at);

  |]

teardown :: DB (Maybe ())
teardown = withSetup $ void $ execute_ [sql|
    DROP TABLE meta.actions;
    DROP TABLE meta.applications;
  |]

--------------------------
-- apply
-------------------------
listApplications :: DB [OutputGroup]
listApplications = do
  _ <- setup
  mapM getOutputGroup =<<
    query_ [sql|
      SELECT id, created_at
      FROM meta.applications
      ORDER BY order ASC |]

apply :: InputGroup -> DB (Maybe OutputGroup)
apply migrations = do
  _ <- setup
  appliedMigrations <- listApplications

  let unappliedMigrations = differenceMigrationsByVersion
        (toList $ inputGroupMigrations migrations) $ map fst $
          outputGroupsToVersions  appliedMigrations

  forM (nonEmpty unappliedMigrations) $ \ms ->
    applyMigrations ms

applyMigrations :: GroupRow -> NonEmpty (InputMigration) -> DB GroupRow
applyMigrations groupRow migrations = do
  createApplication groupRow
  forM_ migrations $ \migration -> do
    inputAction migration
    insertMigration (applicationId groupRow) migration
  pure application

applyMigration :: Hash -> InputMigration -> DB ()
applyMigration applicationId migration = do


insertMigration :: GroupId -> InputMigration -> DB ()
insertMigration groupId migration = void $ execute
  [sql|
    INSERT INTO meta.actions
    (version, hash, application_id)
    VALUES
    (?, ?, ?)
  |] (migration Psql.:. groupId)

{-
dup :: (t -> a) -> (t -> b) -> t -> (a, b)
dup f g x = (f x, g x) -- ((,) <$> f <*> g)

outputGroupToVersions :: OutputGroup -> NonEmpty (Version, Hash)
outputGroupToVersions = fmap (dup omVersion omHash) . ogMigrations

outputGroupsToVersions :: [OutputGroup] -> [(Version, Hash)]
outputGroupsToVersions = concatMap (toList . outputGroupToVersions)

flattenOutputGroups :: [OutputGroup] -> [OutputMigration]
flattenOutputGroups = concatMap (NonEmpty.toList . ogMigrations)

hashConflicts :: [InputMigration] -> DB [Version]
hashConflicts migrations = hashConflictsInternal migrations . flattenOutputGroups <$> listApplications

hashConflictsInternal :: [InputMigration] -> [OutputMigration] -> [Version]
hashConflictsInternal newVersions oldVersions =
  let hashableMap = Map.fromList $ map (dup omVersion omHash) $ oldVersions
      lookupDifferentHash theMap (key, newHash) =
        case Map.lookup key theMap of
          Just existingHash
              | existingHash /= newHash -> Just key
          _ -> Nothing

  in mapMaybe (lookupDifferentHash hashableMap . dup inputVersion inputHash) newVersions

getOutputGroup :: GroupRow -> DB OutputGroup
getOutputGroup GroupRow {..} = do
  outputMigrations <- NonEmpty.fromList <$> query [sql|
    SELECT version, hash, application_id, created_at
    FROM meta.actions
    WHERE application_id = ?
    ORDER BY created_at ASC
  |] arId
  pure $ OutputGroup arId outputMigrations arCreatedAt
-- | The migration function. Returns the migration group application row if
-- any new migrations were applied.



-------------------------------------------------------------------------------
-- Helpers for 'migrate'
-------------------------------------------------------------------------------
diffToUnappliedMigrations :: [Version] -> [Version] -> [Version]
diffToUnappliedMigrations allMigrations appliedMigrations
  = Set.toList
  $ Set.fromList allMigrations `Set.difference` Set.fromList appliedMigrations

differenceMigrationsByVersion :: [InputMigration] -> [Version] -> [InputMigration]
differenceMigrationsByVersion migrations appliedVersions =
  let versionsToApply = diffToUnappliedMigrations (map inputVersion migrations) appliedVersions
  in filter (\m -> inputVersion m `elem` versionsToApply) migrations





createApplication :: GroupRow -> DB GroupRow
createApplication theHash = fmap head $ query [sql|
  INSERT INTO meta.applications
  DEFAULT VALUES
  RETURNING id, created_at
  |] $ Only theHash



-}
