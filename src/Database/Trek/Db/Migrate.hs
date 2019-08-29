module Database.Trek.Db.Migrate where

import Database.PostgreSQL.Transact

import Database.Trek.Db.Types
import qualified Database.Trek.Db.Internal as Db
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Arrow
import Data.Maybe
import qualified Data.List.NonEmpty as NonEmpty
import Control.Monad
import Control.Monad.Catch

hashConflicts :: [Migration] -> [(Version, Hash)] -> [Version]
hashConflicts migrations existingVersions =
  hashConflictsInternal
    (map ((mVersion *** Db.hashQuery . mQuery) . join (,)) migrations)
    existingVersions

-- | The migration function. Returns the migration group application row if
-- any new migrations were applied.
migrate :: [Migration] -> DB (Either MigrationException (Maybe Application))
migrate migrations = try $ Db.mapSqlError $ do
  appliedMigrations <- Db.listMigrations

  let unappliedMigrations = differenceMigrationsByVersion migrations $ map fst appliedMigrations

  forM (NonEmpty.nonEmpty unappliedMigrations) Db.applyMigrations

-------------------------------------------------------------------------------
-- Helpers for 'migrate'
-------------------------------------------------------------------------------
hashConflictsInternal :: [(Version, Hash)] -> [(Version, Hash)] -> [Version]
hashConflictsInternal newVersions oldVersions =
  let hashableMap = Map.fromList oldVersions
      lookupDifferentHash theMap (key, newHash) =
        case Map.lookup key theMap of
          Just existingHash
              | existingHash /= newHash -> Just key
          _ -> Nothing

  in mapMaybe (lookupDifferentHash hashableMap) newVersions

diffToUnappliedMigrations :: [Version] -> [Version] -> [Version]
diffToUnappliedMigrations allMigrations appliedMigrations
  = Set.toList
  $ Set.fromList allMigrations `Set.difference` Set.fromList appliedMigrations

differenceMigrationsByVersion :: [Migration] -> [Version] -> [Migration]
differenceMigrationsByVersion migrations appliedVersions =
  let versionsToApply = diffToUnappliedMigrations (map mVersion migrations) appliedVersions
  in filter (\m -> mVersion m `elem` versionsToApply) migrations



