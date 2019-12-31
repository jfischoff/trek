module Database.Trek.Db.ValidTest
  (extraMigrations, migrations, conflictingMigrations, worldState, WorldState(..), clear, rollback, dbRunner
  , inputVersion, inputAction) where
import Database.Trek.Db.Valid
import Database.Trek.Db.TestInterface.Types
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty, fromList, nonEmpty)
import Data.Foldable

add :: Int -> DB ()
add i = modify (\x -> x { isCounter = isCounter x + i })

extraMigrations :: NonEmpty InputMigration
extraMigrations = pure $ InputMigration (add 6) 6 6

migrations :: NonEmpty InputMigration
migrations = fromList $ map (\x -> InputMigration (add x) x x) $ [1 .. 5]

conflictingMigrations :: NonEmpty InputMigration
conflictingMigrations = fromList $ map (\x -> InputMigration (add x) x $ x + 1) $ [1 .. 5]

worldState :: DB WorldState
worldState = get

clear :: DB ()
clear = put initialWorldState

rollback :: DB a -> DB a
rollback action = do
  oldState <- get
  r <- action
  put oldState
  pure r

dbRunner :: IO (SpecStateM DB)
dbRunner = pure $ SpecState
  { ssRunner   = flip evalStateT initialWorldState
  , ssShutdown = pure ()
  }
