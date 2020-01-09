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
  , inputMigration
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
  { inputAction :: DB ()
  , inputVersion :: Version
  , inputHash :: Hash
  }

instance Psql.ToRow InputMigration where
  toRow InputMigration {..} = [Psql.toField inputVersion, Psql.toField inputHash]

newtype GroupId = GroupId Int
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Psql.FromField, Psql.ToField)
    deriving (Psql.ToRow) via (Psql.Only GroupId)

instance Psql.FromRow GroupId where
  fromRow = GroupId <$> Psql.field

data OutputGroup = OutputGroup
  { ogId         :: GroupId
  , ogMigrations :: NonEmpty OutputMigration
  , ogCreatedAt  :: UTCTime
  }
  deriving (Show, Eq)

data InputGroup = InputGroup
  { inputGroupMigrations :: NonEmpty InputMigration
  }

-- InputMigration constructor
inputMigration :: DB () -> Version -> Hash -> InputMigration
inputMigration = InputMigration

-- InputGroup constructor
inputGroup :: NonEmpty InputMigration -> DB InputGroup
inputGroup = pure . InputGroup

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
  ( id SERIAL PRIMARY KEY
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp
  );

  CREATE TABLE meta.actions
  ( version TIMESTAMP WITH TIME ZONE PRIMARY KEY
  , hash bytea NOT NULL
  , application_id int NOT NULL REFERENCES meta.applications ON DELETE CASCADE
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp
  );

  |]

teardown :: DB (Maybe ())
teardown = withSetup $ void $ execute_ [sql|
    DROP TABLE meta.actions;
    DROP TABLE meta.applications;
  |]

--------------------------
-- apply
-------------------------
data OutputMigration = OutputMigration
  { omVersion :: Version
  , omHash :: Hash
  , omGroupId :: GroupId
  , omCreatedAt :: UTCTime
  } deriving stock (Eq, Show, Ord, Generic)
    deriving anyclass (Psql.FromRow)

data GroupRow = GroupRow
  { arId         :: GroupId
  , arCreatedAt  :: UTCTime
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Psql.FromRow)

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
apply :: InputGroup -> DB (Maybe OutputGroup)
apply migrations = do
  _ <- setup
  appliedMigrations <- listApplications

  let unappliedMigrations = differenceMigrationsByVersion
        (toList $ inputGroupMigrations migrations) $ map fst $
          outputGroupsToVersions  appliedMigrations

  forM (nonEmpty unappliedMigrations) $ \ms ->
    getOutputGroup =<< applyMigrations ms

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

applyMigrations :: NonEmpty (InputMigration) -> DB GroupRow
applyMigrations migrations = do
  application@GroupRow {..} <- createApplication
  mapM_ (applyMigration arId) migrations
  pure application

applyMigration :: GroupId -> InputMigration -> DB ()
applyMigration applicationId migration = do
  inputAction migration
  insertMigration applicationId migration

createApplication :: DB GroupRow
createApplication = fmap head $ query_ [sql|
  INSERT INTO meta.applications
  DEFAULT VALUES
  RETURNING id, created_at
  |]

insertMigration :: GroupId -> InputMigration -> DB ()
insertMigration groupId migration = void $ execute
  [sql|
    INSERT INTO meta.actions
    (version, hash, application_id)
    VALUES
    (?, ?, ?)
  |] (migration Psql.:. groupId)

listApplications :: DB [OutputGroup]
listApplications = do
  _ <- setup
  mapM getOutputGroup =<<
    query_ [sql|
      SELECT id, created_at
      FROM meta.applications
      ORDER BY created_at ASC |]
