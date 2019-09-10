module Database.Trek.Db.Interface where
import Database.Trek.Db.Types
import Database.PostgreSQL.Transact (DB)
import Data.List.NonEmpty (NonEmpty)
import Data.Aeson (Value)

data Interface = Interface
  { iSetup    :: DB SetupResult
  , iTeardown :: DB SetupResult
  , iMigrate
      :: Value
      -> NonEmpty Migration
      -> DB (Either NoSetup (Maybe ApplicationGroup))
  , iListMigrations :: DB (Either NoSetup [ApplicationGroup])
  , iHashConflicts :: [Migration] -> DB (Either NoSetup [Version])
  }

type ConfigureInterface = (String -> Interface)