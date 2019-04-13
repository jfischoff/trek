module Database.Trek.Db where

import Database.Trek.Types

import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Transact
import Data.Pool
import Control.Monad (void)

runDb :: Pool PS.Connection -> DB a -> IO a
runDb = undefined


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

applyMigration :: Migration -> DB ()
applyMigration = undefined

oldestMigrationPitb :: [Version] -> DB PointInTime
oldestMigrationPitb = undefined

getAppliedMigrations :: DB [(Version, Maybe Hash)]
getAppliedMigrations = undefined
