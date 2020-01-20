module Database.Trek.Db.ValidTest
  (foo, bar, quux, rollback, clear, WorldState, worldState, inputGroup, inputAction, toOutput, dbRunner, InputMigration) where
import Database.Trek.Db.Valid
import Database.Trek.Db.TestInterface.Types
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty, fromList, nonEmpty)
import Data.Foldable

add :: Int -> DB ()
add i = modify (\x -> x { isCounter = isCounter x + i })

foo :: InputMigration
foo = InputMigration (add 6) 6 6

bar :: InputMigration
bar = InputMigration (add 5) 5 5

quux :: InputMigration
quux = InputMigration (add 4) 4 4

rollback :: DB a -> DB a
rollback action = do
  oldState <- get
  r <- action
  put oldState
  pure r

dbRunner :: (SpecStateM DB -> IO ()) -> IO ()
dbRunner f = f $  SpecState (flip evalStateT initialInternalState)
