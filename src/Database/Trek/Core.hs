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
hashConflicts :: [(Version, Hash)] -> [(Version, Maybe Hash)] -> [Version]
hashConflicts newVersions oldVersions =
  let hashableVersions = mapMaybe (\(x, my) -> (x,) <$> my) oldVersions
      hashableMap = Map.fromList hashableVersions
      lookupDifferentHash theMap (key, newHash) =
        case Map.lookup key theMap of
          Just existingHash
              | existingHash /= newHash -> Just key
          _ -> Nothing

  in mapMaybe (lookupDifferentHash hashableMap) newVersions

hasHashConflict
  :: [Migration]
  -> [(Version, Maybe Hash)]
  -> Bool
hasHashConflict x = not . null . findAllHashConflicts x

findAllHashConflicts
  :: [Migration]
  -> [(Version, Maybe Hash)]
  -> [Version]
findAllHashConflicts allMigrations appliedMigrations =
  hashConflicts (map ((mVersion *** hashMigration) . join (,)) allMigrations) appliedMigrations

setup :: DB ()
setup = Db.mapSqlError $ Db.setup Qa

diffSetup :: DB Db.SchemaDiff
diffSetup = Db.diffSetup Prod

teardown :: DB ()
teardown = Db.mapSqlError $ Db.teardown

data HashConflict = HashConflict
  { hcConflictingVersions :: [Version]
  , hcContinuation :: DB ()
  } deriving (Typeable)

instance Show HashConflict where
  show HashConflict {..}
    =  "Hash conflict with the following versions "
    ++ unwords (map show hcConflictingVersions)

instance Exception HashConflict

determineMigrationsToApply :: [Migration] -> [(Version, Hash)] -> ([Version], [Migration])
determineMigrationsToApply = undefined

-- Throws HashConflict
migrate :: [Migration] -> DB ()
migrate migrations = Db.mapSqlError $ do
  appliedMigrations <- Db.getAppliedMigrations

  let pitb = undefined

  let (hashConflicts, unappliedMigrations)
        = determineMigrationsToApply migrations appliedMigrations

      applier = for_ (NonEmpty.nonEmpty unappliedMigrations) $ Db.applyMigrationGroup Qa pitb

  unless (null hashConflicts) $ throwM $ HashConflict hashConflicts applier

  applier

