module Database.Trek.Db where

import Database.Trek.Types

import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Types as PS
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Transact
import Data.Pool
import Control.Monad (void)
import Data.Time
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Control.Monad (unless)
import Data.Aeson
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Foldable
import Data.Monoid
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Text.InterpolatedString.Perl6
import Control.Monad.Catch
import GHC.Generics
import Control.Concurrent
import Data.Traversable

newtype Runner = Runner { unRunner :: forall a. DB a -> IO a }

instance FromRow ApplicationId where
  fromRow = fmap PS.fromOnly $ fromRow

data DevMigration = DevMigration
  { dmVersion :: Version
  , dmName    :: Text
  , dmHash    :: Maybe Hash
  }

data ProdMigration = ProdMigration
  { pmVersion :: Version
  , pmName    :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON ProdMigration
instance FromJSON ProdMigration
instance FromRow ProdMigration
instance ToRow ProdMigration

data ProdApplicationRecord = ProdApplicationRecord
  { arPitb :: Maybe Text
  , arMigrations :: [ProdMigration]
  } deriving (Show, Eq)

getAllApplicationRecords :: DB [ProdApplicationRecord]
getAllApplicationRecords = do
  as <- query_ [sql|
      SELECT id, pitb
      FROM applications
      ORDER BY created_at
    |]
  forM as $ \(ai, arPitb) -> do
    arMigrations <- query [sql|
      SELECT version, name
      FROM migrations
      WHERE application_id = ?
      |] (ai :: ApplicationId)
    pure ProdApplicationRecord {..}

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
, created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT clock_timestamp()
);
  |]

createApplication :: Maybe PointInTime -> DB ApplicationId
createApplication = fmap head . query [sql|
  INSERT INTO applications
  (pitb)
  VALUES
  (?)
  RETURNING id
  |] . PS.Only

migrationProduction :: PS.Query
migrationProduction = [sql|
CREATE TABLE migrations
( version TIMESTAMP WITH TIME ZONE PRIMARY KEY
, name text NOT NULL
, created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT clock_timestamp()
, application_id int NOT NULL REFERENCES applications ON DELETE CASCADE
);
|]

getAllProdMigrations :: DB [ProdMigration]
getAllProdMigrations = query_ [sql|
    SELECT version, name
    FROM migrations
    ORDER BY created_at
  |]

data ProdMigrationRow = ProdMigrationRow
  { pmrVersion :: Version
  , pmrName :: Text
  , pmrCreatedAt :: UTCTime
  , pmrApplicationId :: ApplicationId
  } deriving (Show, Eq, Ord, Generic)

instance FromRow ProdMigrationRow
instance ToRow ProdMigrationRow

insertProdMigration :: ApplicationId -> Migration -> DB ()
insertProdMigration applicationId (Migration {..}) = void $ execute
  [sql|
    INSERT INTO migrations
    (version, name, application_id)
    VALUES
    (?, ?, ?)
  |] (mVersion, mName, applicationId)

getProdMigrationRow :: Version -> DB ProdMigrationRow
getProdMigrationRow = fmap head . query [sql|
  SELECT version, name, created_at, application_id
  FROM migrations
  WHERE version = ?
  |] . PS.Only


migrationDev :: PS.Query
migrationDev = [sql|
CREATE TABLE migrations
( version TIMESTAMP WITH TIME ZONE NOT NULL
, name text NOT NULL
, created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT clock_timestamp()
, hash bytea
, application_id int NOT NULL REFERENCES applications ON DELETE CASCADE
);
|]

insertDevMigration :: ApplicationId -> Migration -> DB ()
insertDevMigration applicationId migration@(Migration {..}) = do
  let theHash = hashMigration migration
  void $ execute [sql|
    INSERT INTO migrations
    (version, name, hash, application_id)
    VALUES
    (?, ?, ?, ?)
    |]
    (mVersion, mName, PS.Binary theHash, applicationId)

data DevMigrationRow = MigrationRow
  { mrVersion :: Version
  , mrName :: Text
  , mrCreatedAt :: UTCTime
  , mrHash :: ByteString
  , mrApplicationId :: ApplicationId
  } deriving (Show, Ord, Eq, Generic)

instance FromRow DevMigrationRow
instance ToRow DevMigrationRow

getDevMigrationRow :: Version -> DB DevMigrationRow
getDevMigrationRow = fmap head . query [sql|
    SELECT version, name, created_at, hash, application_id
    FROM migrations
    WHERE version = ?
  |] . PS.Only

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

setup :: Mode -> DB ()
setup mode = case mode of
  Prod -> setupProd
  _    -> setupDev

setupDev :: DB ()
setupDev = void $ do
  execute_ auditLog
  execute_ migrationDev

setupProd :: DB ()
setupProd = void $ do
  execute_ auditLog
  execute_ migrationProduction

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

applyMigrationGroup :: Mode -> Maybe PointInTime -> NonEmpty Migration -> DB ApplicationId
applyMigrationGroup mode mpitb migrations = do
  aId <- createApplication mpitb
  mapM_ (applyMigration mode aId) migrations
  pure aId

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

applyProdMigration :: ApplicationId -> Migration -> DB ()
applyProdMigration applicationId migration = do
  void $ execute_ $ PS.Query $  mQuery migration
  insertProdMigration applicationId migration

applyDevMigration :: ApplicationId -> Migration -> DB ()
applyDevMigration applicationId migration = do
  void $ execute_ $ PS.Query $  mQuery migration
  insertDevMigration applicationId migration

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

applyMigration :: Mode -> ApplicationId -> Migration -> DB ()
applyMigration mode applicationId migration = do
  migrationHasBeenApplied migration >>= \case
    True -> throwM $ MigrationAlreadyApplied $ mVersion migration
    False -> pure ()

  case mode of
    Prod -> applyProdMigration applicationId migration
    _    -> applyDevMigration  applicationId migration

pitbToUndoMigration :: Version -> DB PointInTime
pitbToUndoMigration = fmap (PS.fromOnly . head) . query [sql|
    SELECT a.pitb
    FROM applications AS a
    INNER JOIN migrations AS m ON (m.application_id = a.id)
    WHERE m.version = ?
  |] . PS.Only

oldestMigrationPitb :: NonEmpty Version -> DB PointInTime
oldestMigrationPitb versions = do
  let versionsList = NonEmpty.toList versions
  nonExistingVersions <- filter (not . snd) . zip versionsList <$> mapM versionExists versionsList

  unless (null nonExistingVersions) $ throwM $
    InvalidMigrationVersion $ map fst nonExistingVersions

  fmap (PS.fromOnly . head) $ query [sql|
    SELECT a.pitb
    FROM applications AS a
    INNER JOIN migrations AS m ON (a.id = m.application_id)
    WHERE m.version IN ?
    ORDER BY a.created_at ASC
    LIMIT 1
    |] $ PS.Only $ PS.In $ NonEmpty.toList versions

migrationTableMissing :: PS.SqlError
migrationTableMissing = PS.SqlError
  { sqlState = "42P01"
  , sqlExecStatus = PS.FatalError
  , sqlErrorMsg = "relation \"migrations\" does not exist"
  , sqlErrorDetail = ""
  , sqlErrorHint = ""
  }

getAppliedMigrations :: DB [(Version, Maybe Hash)]
getAppliedMigrations = query_ [sql|
  SELECT version, hash
  FROM migrations
  ORDER BY created_at ASC
  |] `catch` (\(_ :: PS.SqlError) ->
  query_ [sql|
    SELECT version, null
    FROM migrations
    ORDER BY created_at ASC
    |]
  )

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
diffApplications = diffTable "applications"  [ "id", "pitb", "created_at"]

diffProdMigrations :: DB SchemaDiff
diffProdMigrations = diffTable "migrations"
  [ "version"
  , "name"
  , "created_at"
  , "application_id"
  ]

diffDevMigrations :: DB SchemaDiff
diffDevMigrations = diffTable "migrations"
  [ "version"
  , "name"
  , "created_at"
  , "application_id"
  , "hash"
  ]

diffProdSetup :: DB SchemaDiff
diffProdSetup = (<>) <$> diffApplications <*> diffProdMigrations

diffDevSetup :: DB SchemaDiff
diffDevSetup = (<>) <$> diffApplications <*> diffDevMigrations

diffSetup :: Mode -> DB SchemaDiff
diffSetup = \case
  Prod -> diffProdSetup
  _ -> diffDevSetup

cleanSchemaDiff :: Mode -> SchemaDiff
cleanSchemaDiff = \case
  Prod -> mempty { sdMissingTables = ["applications", "migrations"]}
  _ -> mempty { sdMissingTables = ["applications", "migrations"]}