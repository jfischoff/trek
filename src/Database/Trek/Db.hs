module Database.Trek.Db where

import Database.Trek.Types

import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Transact
import Data.Pool
import Control.Monad (void)
import Data.Time
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Int

runDb :: Pool PS.Connection -> DB a -> IO a
runDb = undefined

newtype Runner = Runner { unRunner :: forall a. DB a -> IO a }

newtype ApplicationId = ApplicationId { unApplicationId :: Int64 }

data DevMigration = DevMigration
  { dmVersion :: Version
  , dmName :: Text
  , dmCreate :: UTCTime
  , dmHash :: Maybe Hash
  }

data ProdMigration = ProdMigration
  { pmVersion :: Version
  , pmName :: Text
  , pmCreate :: UTCTime
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
, create TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT clock_timestamp()
, before int REFERENCES applications
);
  |]

migrationProduction :: PS.Query
migrationProduction = [sql|
CREATE TABLE migrations
( version TIMESTAMP PRIMARY KEY
, name text NOT NULL
, create TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT clock_timestamp()
, application_id int REFERENCES applications NOT NULL ON DELETE CASCADE
);
|]

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

applyMigrationGroup :: Maybe PointInTime -> NonEmpty Migration -> DB ApplicationId
applyMigrationGroup = undefined

applyMigration :: ApplicationId -> Migration -> DB ()
applyMigration = undefined

oldestMigrationPitb :: NonEmpty Version -> DB PointInTime
oldestMigrationPitb = undefined

getAppliedMigrations :: DB [(Version, Maybe Hash)]
getAppliedMigrations = undefined
