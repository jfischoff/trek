module Database.Trek.Db
  ( -- * Life cycle management
    ApplyType (..)
  , apply
  -- * Types
  , InputMigration (..)
  , OutputMigration (..)
  , GroupId (..)
  , Version
  , Hash
  , DB
  , OutputGroup (..)
  , InputGroup (..)
  , inputGroup
  , Time
  , makeGroupHash
  )
  where
import Database.PostgreSQL.Transact
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.ByteString (ByteString)
import qualified Database.PostgreSQL.Simple as Psql
import qualified Database.PostgreSQL.Simple.FromField as Psql
import qualified Database.PostgreSQL.Simple.ToField as Psql
import qualified Database.PostgreSQL.Simple.ToRow as Psql
import qualified Database.PostgreSQL.Simple.FromRow as Psql
import Database.PostgreSQL.Simple.SqlQQ
import Control.Monad (void)
import GHC.Generics
import qualified Data.Set as Set
import Data.Traversable
import Data.Foldable
import Control.Monad.IO.Class
import Data.Time
import Crypto.Hash.SHA1
import Database.PostgreSQL.Simple.Types
import Data.Time.Clock.POSIX
import qualified Data.ByteString.Char8 as BSC
import Data.Function

type Version = UTCTime

type Hash = ByteString

type Time = UTCTime

data InputMigration = InputMigration
  { inputAction  :: DB ()
  , inputVersion :: Version
  , inputHash    :: Binary Hash
  }

instance Psql.ToRow InputMigration where
  toRow InputMigration {..} = [Psql.toField inputVersion, Psql.toField inputHash]

newtype GroupId = GroupId (Binary ByteString)
    deriving stock (Show, Eq, Ord, Generic)
    deriving newtype (Psql.FromField, Psql.ToField)
    deriving (Psql.ToRow) via (Psql.Only GroupId)

instance Psql.FromRow GroupId where
  fromRow = GroupId <$> Psql.field

data OutputMigration = OutputMigration
  { omVersion :: Version
  , omHash    :: Binary Hash
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
  inputGroupCreateAd <- liftIO $
    fmap ( posixSecondsToUTCTime
         . (1e-4 *)
         . (fromIntegral :: Integer -> NominalDiffTime)
         . floor
         . (1e4 *)
         ) $ getPOSIXTime
  pure InputGroup {..}

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

withoutSetup :: DB a -> DB (Maybe a)
withoutSetup = onSetup not

-------------------------------------------------------------------------------
-- setup/teardown
-------------------------------------------------------------------------------

setup :: DB (Maybe ())
setup = withoutSetup $ void $ execute_ [sql|
  CREATE SCHEMA meta;

  CREATE TABLE meta.applications
  ( id bytea PRIMARY KEY
  , rowOrder SERIAL NOT NULL
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL
  );

  CREATE INDEX ON meta.applications (rowOrder);
  CREATE INDEX ON meta.applications (created_at);

  CREATE TABLE meta.actions
  ( version TIMESTAMP WITH TIME ZONE PRIMARY KEY
  , hash bytea NOT NULL
  , application_id bytea NOT NULL REFERENCES meta.applications ON DELETE CASCADE
  , rowOrder SERIAL NOT NULL
  );

  CREATE INDEX ON meta.actions (hash);
  CREATE INDEX ON meta.actions (application_id);
  CREATE INDEX ON meta.actions (rowOrder);

  |]

--------------------------
-- apply
-------------------------
createApplication :: GroupRow -> DB ()
createApplication groupRow = void $ execute [sql|
  INSERT INTO meta.applications
  (id, created_at)
  VALUES
  (?, ?)
  |] groupRow

dup :: (t -> a) -> (t -> b) -> t -> (a, b)
dup f g x = (f x, g x)

outputGroupsToVersions :: [OutputGroup] -> [(Version, Hash)]
outputGroupsToVersions = concatMap (toList . outputGroupToVersions)

outputGroupToVersions :: OutputGroup -> NonEmpty (Version, Hash)
outputGroupToVersions = fmap (dup omVersion (fromBinary . omHash)) . ogMigrations

diffToUnappliedMigrations :: [Version] -> [Version] -> [Version]
diffToUnappliedMigrations allMigrations appliedMigrations
  = Set.toList
  $ Set.fromList allMigrations `Set.difference` Set.fromList appliedMigrations

differenceMigrationsByVersion :: [InputMigration] -> [Version] -> [InputMigration]
differenceMigrationsByVersion migrations appliedVersions =
  let versionsToApply = diffToUnappliedMigrations (map inputVersion migrations) appliedVersions
  in filter (\m -> inputVersion m `elem` versionsToApply) migrations

getOutputGroup :: GroupId -> DB OutputGroup
getOutputGroup groupId = do
  outputMigrations <- NonEmpty.fromList <$> query [sql|
    SELECT version, hash
    FROM meta.actions
    WHERE application_id = ?
    ORDER BY rowOrder ASC
  |] groupId

  arCreatedAt <- fmap (Psql.fromOnly . head) $ query [sql|
    SELECT created_at
    FROM meta.applications
    WHERE id = ?
  |] groupId

  pure $ OutputGroup groupId arCreatedAt outputMigrations

listApplications :: DB [OutputGroup]
listApplications = do
  _ <- setup
  mapM getOutputGroup =<<
    query_ [sql|
      SELECT id
      FROM meta.applications
      ORDER BY rowOrder ASC |]

makeGroupHash :: UTCTime -> [InputMigration] -> Hash
makeGroupHash createdAt migrations = hash $
  mconcat ((BSC.pack $ show createdAt) : map (fromBinary . inputHash) migrations)

inputGroupToGroupRow :: InputGroup -> GroupRow
inputGroupToGroupRow InputGroup {..} =
  let arId         = GroupId $ Binary $ makeGroupHash inputGroupCreateAd $ NonEmpty.toList inputGroupMigrations
      arCreatedAt  = inputGroupCreateAd

  in GroupRow {..}

data ApplyType = RunMigrations | SetMigrate
  deriving (Show, Eq, Ord, Enum, Bounded, Generic)

apply :: ApplyType -> InputGroup -> DB (Maybe OutputGroup)
apply applyType migrations = do
  _ <- setup
  appliedMigrations <- listApplications

  let unappliedMigrations = differenceMigrationsByVersion
        (toList $ inputGroupMigrations migrations) $ map fst $
          outputGroupsToVersions  appliedMigrations

  forM (nonEmpty unappliedMigrations) $ \ms -> do
    let groupRow = inputGroupToGroupRow $
          migrations { inputGroupMigrations = ms }
        applier = case applyType of
          RunMigrations -> applyMigrations
          SetMigrate    -> setMigrate

    applier groupRow ms
    getOutputGroup $ arId groupRow

applyMigrations :: GroupRow -> NonEmpty (InputMigration) -> DB ()
applyMigrations groupRow migrations = do
  createApplication groupRow
  applyMigrationsWith migrations $ \migration -> do
    inputAction migration
    insertMigration (arId groupRow) migration

setMigrate :: GroupRow -> NonEmpty (InputMigration) -> DB ()
setMigrate groupRow migrations = do
  createApplication groupRow
  applyMigrationsWith migrations $ insertMigration (arId groupRow)

applyMigrationsWith :: NonEmpty (InputMigration)
                    -> (InputMigration -> DB ())
                    -> DB ()
applyMigrationsWith migrations f =
  forM_ (NonEmpty.sortBy (compare `on` inputVersion) migrations) f

insertMigration :: GroupId -> InputMigration -> DB ()
insertMigration groupId migration = void $ execute
  [sql|
    INSERT INTO meta.actions
    (version, hash, application_id)
    VALUES
    (?, ?, ?)
  |] (migration Psql.:. groupId)
