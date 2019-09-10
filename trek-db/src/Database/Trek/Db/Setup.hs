module Database.Trek.Db.Setup where
import Database.Trek.Db.MigrationException

import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as PS
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
import qualified Data.ByteString as BS


-------------------------------------------------------------------------------
-- Public interface
-------------------------------------------------------------------------------
setup :: DB ()
setup = mapSqlError $ void $ do
  void $ execute_ createApplicationTable
  execute_ createMigrationTable

teardown :: DB ()
teardown = mapSqlError $ void $ execute_ [sql|
    DROP TABLE migrations;
    DROP TABLE applications;
  |]
-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------
createApplicationTable :: PS.Query
createApplicationTable = [sql|
  CREATE TABLE applications
  ( id SERIAL PRIMARY KEY
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp
  );
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