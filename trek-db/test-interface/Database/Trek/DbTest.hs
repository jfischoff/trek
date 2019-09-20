module Database.Trek.DbTest
  ( WorldState
  , inputVersion
  , inputAction
  , extraMigrations
  , migrations
  , conflictingMigrations
  , worldState
  , clear
  , rollback
  , createTempConnection
  , setupDB
  , shutDownDB
  , toSpecState
  , dbRunner
  ) where
import Database.Trek.Db
import Database.Trek.Db.TestInterface.Types
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)
import Data.Time.QQ
import qualified Database.Postgres.Temp.Internal as Temp
import qualified Database.PostgreSQL.Transact as T
import qualified Database.PostgreSQL.Simple as Psql
import Database.PostgreSQL.Simple.SqlQQ
import qualified Data.ByteString.Char8 as BSC
import Control.Exception
import Control.Monad (void)

type WorldState = ByteString

extraMigrations :: NonEmpty InputMigration
extraMigrations = pure $ InputMigration (pure ()) [utcIso8601| 2048-12-01 |] "extra"

migrations :: NonEmpty InputMigration
migrations = pure $ InputMigration (pure ()) [utcIso8601| 2025-12-01 |] "migration"

conflictingMigrations :: NonEmpty InputMigration
conflictingMigrations = pure $ InputMigration (pure ()) [utcIso8601| 2025-12-01 |] "conflicting"

worldState :: DB WorldState
worldState = pure ""

clear :: DB ()
clear = void $ T.execute_ [sql|
  DROP SCHEMA IF EXISTS meta CASCADE;
  CREATE SCHEMA meta; |]

rollback :: DB a -> DB a
rollback = id
--

createTempConnection :: IO (Psql.Connection, Temp.DB)
createTempConnection = do
  db <- either throwIO pure =<< Temp.start Temp.defaultOptions
  connection <- Psql.connectPostgreSQL $ BSC.pack $ Temp.connectionString db
  return (connection, db)

-- Either run the job or not
setupDB :: IO (Psql.Connection, Temp.DB)
setupDB = do
  (connection, db) <- createTempConnection
  return (connection, db)

shutDownDB :: Psql.Connection -> Temp.DB -> IO ()
shutDownDB c x = void $ Psql.close c >> Temp.stop x

toSpecState :: Psql.Connection -> Temp.DB -> IO (SpecStateM DB)
toSpecState conn = pure . SpecState (flip T.runDBTSerializable conn) . shutDownDB conn

dbRunner :: IO (SpecStateM DB)
dbRunner = setupDB >>= uncurry toSpecState