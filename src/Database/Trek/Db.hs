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

import Control.Monad.Catch
import GHC.Generics

runDb :: Pool PS.Connection -> DB a -> IO a
runDb = undefined

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
  }

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

setupDev :: DB ()
setupDev = void $ do
  execute_ auditLog
  execute_ migrationDev

setupProd :: DB ()
setupProd = void $ do
  execute_ auditLog
  execute_ migrationProduction

applicationsAlreadyExists :: PS.SqlError
applicationsAlreadyExists = PS.SqlError
  { sqlState = "42P07"
  , sqlExecStatus = PS.FatalError
  , sqlErrorMsg = "relation \"applications\" already exists"
  , sqlErrorDetail = ""
  , sqlErrorHint = ""
  }

getAppliedVersions :: DB [Version]
getAppliedVersions = map PS.fromOnly <$> query_ [sql|
  SELECT version
  FROM migrations
  ORDER BY created_at ASC
  |]

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