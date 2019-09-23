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
  , NoSetup(..)
  , AlreadySetup (..)
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
import Data.String.Here.Interpolated (i)
import Control.Monad (void, join)
import GHC.Generics
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.Traversable
import Data.Foldable
import Control.Arrow ((***))

type Version = UTCTime

type Hash = ByteString

data NoSetup = NoSetup

data AlreadySetup = AlreadySetup

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
  { ogId :: GroupId
  , ogMigrations :: NonEmpty OutputMigration
  }
  deriving (Show, Eq)

data InputGroup = InputGroup
  { inputGroupMigrations :: NonEmpty InputMigration
  }

-- InputMigration constructor
inputMigration :: DB () -> Version -> Hash -> InputMigration
inputMigration = InputMigration

-- InputGroup constructor
inputGroup :: NonEmpty InputMigration -> InputGroup
inputGroup = InputGroup

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------
verifyTableExists :: Psql.Query -> Psql.Query -> DB Bool
verifyTableExists schemaName tableName = Psql.fromOnly . head <$> query_ [i|
  SELECT EXISTS (
  SELECT 1
  FROM   information_schema.tables
  WHERE  table_schema = '${schemaName}'
  AND    table_name = '${tableName}'
  );
  |]

onSetup :: (Bool -> Bool) -> DB a -> DB (Maybe a)
onSetup onF action = do
  setupExists <- verifyTableExists "meta" "applications"
  if onF setupExists
    then Just <$> action
    else pure Nothing

withSetup :: DB a -> DB (Maybe a)
withSetup = onSetup id

withoutSetup :: DB a -> DB (Maybe a)
withoutSetup = onSetup not

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

data ApplicationRow = ApplicationRow
  { arId         :: GroupId
  , arCreatedAt  :: UTCTime
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (Psql.FromRow)

outputGroupToVersions :: OutputGroup -> NonEmpty (Version, Hash)
outputGroupToVersions = fmap ((omVersion *** omHash) . join (,)) . ogMigrations

outputGroupsToVersions :: [OutputGroup] -> [(Version, Hash)]
outputGroupsToVersions = concatMap (toList . outputGroupToVersions)

hashConflicts :: [InputMigration] -> DB (Maybe [Version])
hashConflicts migrations =
  fmap (hashConflictsInternal
    (map ((inputVersion *** inputHash) . join (,)) migrations) . outputGroupsToVersions)
    <$> listApplications

getOutputGroup :: GroupId -> DB OutputGroup
getOutputGroup aId = do
  outputMigrations <- NonEmpty.fromList <$> query [sql|
    SELECT version, hash, application_id, created_at
    FROM meta.actions
    WHERE application_id = ?
  |] aId
  pure $ OutputGroup aId outputMigrations
-- | The migration function. Returns the migration group application row if
-- any new migrations were applied.
apply :: InputGroup -> DB (Maybe (Maybe OutputGroup))
apply migrations = do
  mAppliedMigration <- listApplications
  forM mAppliedMigration $ \appliedMigrations -> do
    let unappliedMigrations = differenceMigrationsByVersion
          (toList $ inputGroupMigrations migrations) $ map fst $ outputGroupsToVersions  appliedMigrations

    forM (nonEmpty unappliedMigrations) $ \ms -> do
      ApplicationRow {..} <- applyMigrations ms
      getOutputGroup arId

-------------------------------------------------------------------------------
-- Helpers for 'migrate'
-------------------------------------------------------------------------------

hashConflictsInternal :: [(Version, Hash)] -> [(Version, Hash)] -> [Version]
hashConflictsInternal newVersions oldVersions =
  let hashableMap = Map.fromList oldVersions
      lookupDifferentHash theMap (key, newHash) =
        case Map.lookup key theMap of
          Just existingHash
              | existingHash /= newHash -> Just key
          _ -> Nothing

  in mapMaybe (lookupDifferentHash hashableMap) newVersions

diffToUnappliedMigrations :: [Version] -> [Version] -> [Version]
diffToUnappliedMigrations allMigrations appliedMigrations
  = Set.toList
  $ Set.fromList allMigrations `Set.difference` Set.fromList appliedMigrations

differenceMigrationsByVersion :: [InputMigration] -> [Version] -> [InputMigration]
differenceMigrationsByVersion migrations appliedVersions =
  let versionsToApply = diffToUnappliedMigrations (map inputVersion migrations) appliedVersions
  in filter (\m -> inputVersion m `elem` versionsToApply) migrations

applyMigrations :: NonEmpty (InputMigration) -> DB ApplicationRow
applyMigrations migrations = do
  application@ApplicationRow {..} <- createApplication
  mapM_ (applyMigration arId) migrations
  pure application

migrationHasBeenApplied :: InputMigration -> DB Bool
migrationHasBeenApplied = versionExists . inputVersion

applyMigration :: GroupId -> InputMigration -> DB Bool
applyMigration applicationId migration = do
  migrationHasBeenApplied migration >>= \case
    True -> pure False
    False -> do
      inputAction migration
      insertMigration applicationId migration
      pure True

createApplication :: DB ApplicationRow
createApplication = fmap head $ query_ [sql|
  INSERT INTO meta.applications
  DEFAULT VALUES
  RETURNING id, created_at
  |]


versionExists :: Version -> DB Bool
versionExists = fmap (Psql.fromOnly . head) . query [sql|
  SELECT EXISTS (
    SELECT 1
    FROM meta.actions
    WHERE version = ?
  )
  |] . Psql.Only

insertMigration :: GroupId -> InputMigration -> DB ()
insertMigration groupId migration = void $ execute
  [sql|
    INSERT INTO meta.actions
    (version, hash, application_id)
    VALUES
    (?, ?, ?)
  |] (migration Psql.:. groupId)

listApplications :: DB (Maybe [OutputGroup])
listApplications = withSetup $ do
  as <- query_ [sql| SELECT id FROM meta.applications |]
  forM as getOutputGroup

