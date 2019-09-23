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
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Time.QQ
import qualified Database.Postgres.Temp.Internal as Temp
import qualified Database.PostgreSQL.Transact as T
import qualified Database.PostgreSQL.Simple as Psql
import Database.PostgreSQL.Simple.SqlQQ
import qualified Data.ByteString.Char8 as BSC
import Control.Exception
import Data.Text (Text)
import Control.Monad (void)

type WorldState = [Text]

createFoo :: DB ()
createFoo = void $ T.execute_ [sql| CREATE TABLE test.foo ()|]

createBar :: DB ()
createBar = void $ T.execute_ [sql| CREATE TABLE test.bar ()|]

createQuux :: DB ()
createQuux = void $ T.execute_ [sql| CREATE TABLE test.quux ()|]

createBar1 :: DB ()
createBar1 = void $ T.execute_ [sql| CREATE TABLE test.bar (id text)|]

createQuux1 :: DB ()
createQuux1 = void $ T.execute_ [sql| CREATE TABLE test.quux (id text)|]

extraMigrations :: NonEmpty InputMigration
extraMigrations = pure $ InputMigration createFoo [utcIso8601| 2048-12-01 |] "extra"

migrations :: NonEmpty InputMigration
migrations = NonEmpty.fromList
  [ InputMigration createBar  [utcIso8601| 2025-12-01 |] "migration-2025-12-01"
  , InputMigration createQuux [utcIso8601| 2025-12-02 |] "migration-2025-12-02"
  ]

conflictingMigrations :: NonEmpty InputMigration
conflictingMigrations = NonEmpty.fromList
  [ InputMigration createBar1  [utcIso8601| 2025-12-01 |] "conflicting-2025-12-01"
  , InputMigration createQuux1 [utcIso8601| 2025-12-02 |] "conflicting-2025-12-02"
  ]

worldState :: DB WorldState
worldState = fmap Psql.fromOnly <$> T.query_
  "SELECT table_name FROM information_schema.tables where table_schema = 'test'"

clear :: DB ()
clear = void $ T.execute_ [sql|
  DROP SCHEMA IF EXISTS meta CASCADE;
  CREATE SCHEMA meta;
  DROP SCHEMA IF EXISTS test CASCADE;
  CREATE SCHEMA test; |]

rollback :: DB a -> DB a
rollback = T.rollback
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