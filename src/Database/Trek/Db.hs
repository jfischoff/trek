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
import Data.Int
import Control.Monad.Catch
import GHC.Generics

runDb :: Pool PS.Connection -> DB a -> IO a
runDb = undefined

newtype Runner = Runner { unRunner :: forall a. DB a -> IO a }

newtype ApplicationId = ApplicationId { unApplicationId :: Int64 }
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromField, ToField)
  deriving (ToRow) via (PS.Only ApplicationId)

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
applyMigrationGroup _mode mpitb _migrations = do
  aId <- createApplication mpitb
  undefined

applyProdMigration :: ApplicationId -> Migration -> DB ()
applyProdMigration applicationId migration = do 
  void $ execute_ $ PS.Query $  mQuery migration
  insertProdMigration applicationId migration

applyDevMigration :: ApplicationId -> Migration -> DB ()
applyDevMigration applicationId migration = do
  void $ execute_ $ PS.Query $  mQuery migration
  insertDevMigration applicationId migration

applyMigration :: Mode -> ApplicationId -> Migration -> DB ()
applyMigration mode applicationId migration = case mode of
  Prod -> applyProdMigration applicationId migration
  _    -> applyDevMigration  applicationId migration

oldestMigrationPitb :: NonEmpty Version -> DB PointInTime
oldestMigrationPitb = undefined

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