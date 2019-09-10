module Database.Trek.Db
  ( -- * Life cycle management
    setup
  , teardown
    -- * Migration
  , M.hashConflicts
  , migrate
  -- * Queriesi
  , listMigrations
  -- * Types
  , Migration (..)
  , Version
  , Hash
  , MigrationException (..)
  , HashMigration
  )
  where
import Database.Trek.Db.Internal
import qualified Database.Trek.Db.Migrate as M
import Database.Trek.Db.Types
import Database.PostgreSQL.Transact
import Data.Maybe (isJust)

migrate :: [Migration] -> DB (Either NoSetup MigrationResult)
migrate = fmap (fmap isJust) . M.migrate

setup :: DB SetupResult
teardown :: DB (Either NoSetup ())
listMigrations :: DB (Either NoSetup [HashedMigration])