module Database.Trek.Core where

import Control.Exception
import Database.PostgreSQL.Transact

import Database.Trek.Types
import qualified Database.Trek.Db as Db
import Data.Typeable
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Arrow
import Data.Maybe
import qualified Data.List.NonEmpty as NonEmpty
import Data.Time
import Data.List.Split
import Data.List
import Control.Monad
import Data.Foldable (for_)
import Control.Monad.Catch

filterByVersions :: [Migration] -> [Version] -> [Migration]
filterByVersions migrations =
  mapMaybe (\x -> find (\m -> mVersion m == x) migrations)

dateFormat :: String
dateFormat = iso8601DateFormat (Just "%H-%M-%S")

diffToUnappliedMigrations :: [Version] -> [Version] -> [Version]
diffToUnappliedMigrations allMigrations appliedMigrations
  = Set.toList
  $ Set.fromList allMigrations `Set.difference` Set.fromList appliedMigrations

-- First filter by versions that have a hash
hashConflicts :: [(Version, Hash)] -> [(Version, Hash)] -> [Version]
hashConflicts newVersions oldVersions =
  let hashableMap = Map.fromList oldVersions
      lookupDifferentHash theMap (key, newHash) =
        case Map.lookup key theMap of
          Just existingHash
              | existingHash /= newHash -> Just key
          _ -> Nothing

  in mapMaybe (lookupDifferentHash hashableMap) newVersions

setup :: DB ()
setup = Db.mapSqlError Db.setup

teardown :: DB ()
teardown = Db.mapSqlError Db.teardown

data HashConflict = HashConflict
  { hcConflictingVersions :: [Version]
  , hcContinuation :: DB (Maybe Db.ApplicationRow)
  } deriving (Typeable)

instance Show HashConflict where
  show HashConflict {..}
    =  "Hash conflict with the following versions "
    ++ unwords (map show hcConflictingVersions)

instance Exception HashConflict

differenceMigrationsByVersion :: [Migration] -> [Version] -> [Migration]
differenceMigrationsByVersion migrations appliedVersions =
  let versionsToApply = diffToUnappliedMigrations (map mVersion migrations) appliedVersions
  in filter (\m -> mVersion m `elem` versionsToApply) migrations

migrationsToApply :: [Migration] -> [(Version, Hash)] -> ([Version], [Migration])
migrationsToApply migrations existingVersions =
  ( hashConflicts (map ((mVersion *** hashQuery . mQuery) . join (,)) migrations) existingVersions
  , differenceMigrationsByVersion migrations $ map fst existingVersions
  )

-- Throws HashConflict
migrate :: [Migration] -> DB (Maybe Db.ApplicationRow)
migrate migrations = Db.mapSqlError $ do
  appliedMigrations <- Db.getAppliedMigrations

  let (hashConflicts, unappliedMigrations)
        = migrationsToApply migrations appliedMigrations

      applier = forM (NonEmpty.nonEmpty unappliedMigrations) Db.applyMigrationGroup

  unless (null hashConflicts) $ throwM $ HashConflict hashConflicts applier

  applier

