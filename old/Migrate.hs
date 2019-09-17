module Database.Trek.Db.Migrate where
import Database.Trek.Db.Types

import Database.PostgreSQL.Transact

import qualified Database.Trek.Db.Base as Db
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Arrow
import Data.Maybe
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import Control.Monad
import Control.Monad.Catch
import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple.Types as PS
import Data.Text (Text)
import GHC.Generics
import Database.PostgreSQL.Simple.ToRow
import qualified Data.ByteString as BS

hashConflicts :: [Migration] -> DB [Version]
hashConflicts migrations =
  hashConflictsInternal
    (map ((mVersion *** hashQuery . mQuery) . join (,)) migrations)
    <$>  Db.listMigrations

-- | The migration function. Returns the migration group application row if
-- any new migrations were applied.
migrate :: [Migration] -> DB (Either MigrationException (Maybe ApplicationRow))
migrate migrations = try $ Db.mapSqlError $ do
  appliedMigrations <- Db.listMigrations

  let unappliedMigrations = differenceMigrationsByVersion migrations $ map fst appliedMigrations

  forM (NonEmpty.nonEmpty unappliedMigrations) applyMigrations

-------------------------------------------------------------------------------
-- Helpers for 'migrate'
-------------------------------------------------------------------------------
toMigrationInput :: ApplicationId -> Migration -> MigrationRow To
toMigrationInput miApplicationId migration =
  let Migration
        { mVersion = miVersion
        , mName = miName
        , mQuery = theQuery
        } = migration
      miHash = PS.Binary $ hashQuery theQuery
  in MigrationInput {..}

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

applyMigrations :: NonEmpty Migration -> DB ApplicationRow
applyMigrations migrations = do
  application@ApplicationRow {..} <- createApplication
  mapM_ (applyMigration arId) migrations
  pure application

migrationHasBeenApplied :: Migration -> DB Bool
migrationHasBeenApplied = versionExists . mVersion

applyMigration :: ApplicationId -> Migration -> DB ()
applyMigration applicationId migration = do
  migrationHasBeenApplied migration >>= \case
    True -> throwM $ ME_MigrationAlreadyApplied $ mVersion migration
    False -> pure ()

  void $ execute_ $ PS.Query $  mQuery migration
  insertMigration $ toMigrationInput applicationId migration