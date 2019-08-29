module Database.Trek.Db
  ( setup
  , teardown
  , migrate
  , M.hashConflicts
  -- Queries
  , listMigrations
  -- Types
  , Migration (..)
  , Version
  , Hash
  , MigrationException (..)
  )
  where
import Database.Trek.Db.Internal
import qualified Database.Trek.Db.Migrate as M
import Database.Trek.Db.Types
import Database.PostgreSQL.Transact
import Data.Maybe (isJust)


migrate :: [Migration] -> DB (Either MigrationException Bool)
migrate = fmap (fmap isJust) . M.migrate