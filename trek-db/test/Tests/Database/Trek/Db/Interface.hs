module Database.Trek.Db.Interface where
import Data.Trek.Db.Types

data Interface = Interface
  { iSetup    :: DB SetupResult
  , iTeardown :: DB SetupResult
  , iMigrate
      :: Value
      -> NonEmpty Migration
      -> DB (Either NoSetup (Maybe ApplicationGroup))
  , iListMigrations :: DB (Either NoSetup [ApplicationGroup])
  , iHashConflicts :: [Migration] -> DB (Either NoSetup [Vdersion])
  }