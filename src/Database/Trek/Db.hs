module Database.Trek.Db where

import Database.Trek.Types

import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Types as PS
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Transact
import Control.Monad (void)
import Data.Time
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Text.InterpolatedString.Perl6
import Control.Monad.Catch
import GHC.Generics
import Data.Traversable

newtype Runner = Runner { unRunner :: forall a. DB a -> IO a }

data MigrationInput = MigrationInput
  { miVersion       :: Version
  , miName          :: Text
  , miHash          :: PS.Binary Hash
  , miApplicationId :: ApplicationId
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (ToRow)

toMigrationInput :: ApplicationId -> Migration -> MigrationInput
toMigrationInput miApplicationId migration =
  let Migration
        { mVersion = miVersion
        , mName = miName
        , mQuery = theQuery
        } = migration
      miHash = PS.Binary $ hashQuery theQuery
  in MigrationInput {..}

data MigrationRow = MigrationRow
  { mrVersion       :: Version
  , mrName          :: Text
  , mrHash          :: Hash
  , mrApplicationId :: ApplicationId
  , mrCreatedAt     :: UTCTime
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (FromRow)

data ApplicationInput = ApplicationInput
  { aiMigrations :: MigrationInput
  } deriving (Show, Eq, Ord, Generic)

data ApplicationRow = ApplicationRow
  { arId         :: ApplicationId
  , arCreatedAt  :: UTCTime
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (FromRow)

data Application = Application
  { aMigrations :: [MigrationRow]
  } deriving (Show, Eq, Ord, Generic)

getAllApplicationRecords :: DB (Map ApplicationId Application)
getAllApplicationRecords = do
  as <- query_ [sql|
      SELECT id, created_at
      FROM applications
      ORDER BY created_at
    |]
  fmap Map.fromList $ forM as $ \ApplicationRow {..} -> do
    aMigrations <- query [sql|
      SELECT version, name, hash, application_id, created_at
      FROM migrations
      WHERE application_id = ?
      |] arId
    pure (arId, Application {..})

createApplicationTable :: PS.Query
createApplicationTable = [sql|
  CREATE TABLE applications
  ( id SERIAL PRIMARY KEY
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp
  );
  |]

createApplication :: DB ApplicationRow
createApplication = fmap head $ query_ [sql|
  INSERT INTO applications
  DEFAULT VALUES
  RETURNING id, created_at
  |]

createMigrationTable :: PS.Query
createMigrationTable = [sql|
  CREATE TABLE migrations
  ( version TIMESTAMP WITH TIME ZONE PRIMARY KEY
  , name text NOT NULL
  , hash bytea NOT NULL
  , application_id int NOT NULL REFERENCES applications ON DELETE CASCADE
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp
  );
|]

getAllMigrations :: DB [MigrationRow]
getAllMigrations = query_ [sql|
    SELECT version, name, hash, application_id, created_at
    FROM migrations
    ORDER BY created_at
  |]

insertMigration :: MigrationInput -> DB ()
insertMigration migration = void $ execute
  [sql|
    INSERT INTO migrations
    (version, name, hash, application_id)
    VALUES
    (?, ?, ?, ?)
  |] migration

getMigrationRow :: Version -> DB MigrationRow
getMigrationRow version = fmap head $ query [sql|
    SELECT version, name, hash, application_id, created_at
    FROM migrations
    WHERE version = ?
  |] $ PS.Only version

toMigrationException :: PS.SqlError -> MigrationException
toMigrationException = \case
  PS.SqlError
    { sqlState = "42P07"
    , sqlExecStatus = PS.FatalError
    , sqlErrorMsg = "relation \"applications\" already exists"
    , sqlErrorDetail = ""
    , sqlErrorHint = ""
    } -> SetupRanTwice
  PS.SqlError
    { sqlState = "42P07"
    , sqlExecStatus = PS.FatalError
    , sqlErrorMsg = "relation \"migrations\" already exists"
    } -> SetupRanTwice
  PS.SqlError
    { sqlExecStatus = PS.FatalError
    , sqlErrorMsg = "relation \"migrations\" does not exist"
    } -> NoSetup
  PS.SqlError
    { sqlExecStatus = PS.FatalError
    , sqlErrorMsg = "relation \"applications\" does not exist"
    } -> NoSetup
  x -> UnknownSqlException x

mapSqlError :: MonadCatch m => m a -> m a
mapSqlError = handle (throwM . toMigrationException)

setup :: DB ()
setup = void $ do
  void $ execute_ createApplicationTable
  execute_ createMigrationTable

teardown :: DB ()
teardown = void $ execute_ [sql|
    DROP TABLE migrations;
    DROP TABLE applications;
  |]

applicationsAlreadyExists :: PS.SqlError
applicationsAlreadyExists = PS.SqlError
  { sqlState = "42P07"
  , sqlExecStatus = PS.FatalError
  , sqlErrorMsg = "relation \"applications\" already exists"
  , sqlErrorDetail = ""
  , sqlErrorHint = ""
  }

getAppliedVersions :: DB [Version]
getAppliedVersions = map fst <$> getAppliedMigrations

applyMigrationGroup :: NonEmpty Migration -> DB ApplicationRow
applyMigrationGroup migrations = do
  applicationRow@ApplicationRow {..} <- createApplication
  mapM_ (applyMigration arId) migrations
  pure applicationRow

getMigrationsInApplication :: ApplicationId -> DB (NonEmpty Version)
getMigrationsInApplication applicationId = do
  versions <- fmap (map PS.fromOnly) $ query [sql|
      SELECT version
      FROM migrations
      WHERE application_id = ?
    |] $ PS.Only applicationId

  case versions of
    [] -> throwM $ EmptyApplication applicationId
    x : xs -> pure $ x NonEmpty.:| xs

applyMigration :: ApplicationId -> Migration -> DB ()
applyMigration applicationId migration = do
  migrationHasBeenApplied migration >>= \case
    True -> throwM $ MigrationAlreadyApplied $ mVersion migration
    False -> pure ()

  void $ execute_ $ PS.Query $  mQuery migration
  insertMigration $ toMigrationInput applicationId migration

versionExists :: Version -> DB Bool
versionExists = fmap (PS.fromOnly . head) . query [sql|
SELECT EXISTS (
  SELECT 1
  FROM migrations
  WHERE version = ?
)
|] . PS.Only

migrationHasBeenApplied :: Migration -> DB Bool
migrationHasBeenApplied = versionExists . mVersion

migrationTableMissing :: PS.SqlError
migrationTableMissing = PS.SqlError
  { sqlState = "42P01"
  , sqlExecStatus = PS.FatalError
  , sqlErrorMsg = "relation \"migrations\" does not exist"
  , sqlErrorDetail = ""
  , sqlErrorHint = ""
  }

getAppliedMigrations :: DB [(Version, Hash)]
getAppliedMigrations = query_ [sql|
  SELECT version, hash
  FROM migrations
  ORDER BY created_at ASC
  |]

clearMigrations :: DB ()
clearMigrations = void $ execute_ [sql| TRUNCATE applications CASCADE |]

data TableDiff = TableDiff
  { tdMissingColumns :: [Text]
  , tdExtraColumns   :: [Text]
  } deriving(Show, Eq, Ord)

instance Semigroup TableDiff where
  x <> y = TableDiff
    (tdMissingColumns x <> tdMissingColumns y)
    (tdExtraColumns x <> tdExtraColumns y)

instance Monoid TableDiff where
  mempty = TableDiff mempty mempty
  mappend = (<>)

validateColumns :: Text -> [Text] -> DB TableDiff
validateColumns tableName expectedList = do
  columnNames <- Set.fromList . map PS.fromOnly <$> query_ [qq|
          SELECT
            COLUMN_NAME
          FROM
            information_schema.COLUMNS
          WHERE
            TABLE_NAME = '{tableName}';
        |]

  let expected = Set.fromList expectedList
      tdMissingColumns = toList $ expected Set.\\ columnNames
      tdExtraColumns   = toList $ columnNames Set.\\ expected
  pure TableDiff {..}

-- Extra tables are fine in these situations
data SchemaDiff = SchemaDiff
  { sdMissingTables   :: [Text]
  , sdDifferentTables :: Map Text TableDiff
  } deriving (Show, Eq)

addDifferentTables :: SchemaDiff -> Text -> TableDiff -> SchemaDiff
addDifferentTables x n d
  | d == mempty = x
  | otherwise = x { sdDifferentTables = Map.insert n d $ sdDifferentTables x }

instance Semigroup SchemaDiff where
  x <> y = SchemaDiff
    { sdMissingTables   = sdMissingTables x <> sdMissingTables y
    , sdDifferentTables = sdDifferentTables x <> sdDifferentTables y
    }

instance Monoid SchemaDiff where
  mempty = SchemaDiff mempty mempty
  mappend = (<>)

tableExists :: Text -> DB Bool
tableExists tableName = PS.fromOnly . head <$> query_ [qq| SELECT EXISTS (
  SELECT 1
  FROM   information_schema.tables
  WHERE  table_schema = 'public'
  AND    table_name = '{tableName}'
  )|]

diffTable :: Text -> [Text] -> DB SchemaDiff
diffTable tableName columns = tableExists tableName >>= \case
  True ->  addDifferentTables mempty tableName <$> validateColumns tableName columns
  False -> pure $ mempty { sdMissingTables = [tableName]}

diffApplications :: DB SchemaDiff
diffApplications = diffTable "applications"  [ "id", "created_at"]

diffMigrations :: DB SchemaDiff
diffMigrations = diffTable "migrations"
  [ "version"
  , "name"
  , "created_at"
  , "application_id"
  , "hash"
  ]

diffSetup :: DB SchemaDiff
diffSetup = (<>) <$> diffApplications <*> diffMigrations

cleanSchemaDiff :: SchemaDiff
cleanSchemaDiff =
  mempty { sdMissingTables = ["applications", "migrations"]}