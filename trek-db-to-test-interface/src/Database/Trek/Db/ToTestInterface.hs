module Database.Trek.Db.ToTestInterface
  ( setup
  , teardown
  , apply
  , listApplications
  , hashConflicts
  , I.DB
  , I.Version
  , I.InputMigration
  , I.InputGroup
  , I.OutputGroup
  , I.inputGroup
  , inputVersion
  , inputAction
  ) where
import Data.List.NonEmpty (NonEmpty)
import qualified Database.Trek.Db.Interface as I

eitherToMaybe :: Functor m => m (Either a b) -> m (Maybe a)
eitherToMaybe = either (const Nothing) Just

-- Main API
setup :: DB (Maybe ())
setup = eitherToMaybe I.setup
-- Requires setup
teardown :: DB (Maybe ())
teardown = eitherToMaybe I.teardown

apply :: InputGroup -> DB (Maybe (Maybe OutputGroup))
apply = eitherToMaybe . I.apply

listApplications :: DB (Maybe [OutputGroup])
listApplications = eitherToMaybe . I.listApplications

hashConflicts :: [InputMigration]-> DB (Maybe [Version])
hashConflicts = eitherToMaybe . I.hashConflicts

