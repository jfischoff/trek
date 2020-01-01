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
  , dbRunner
  ) where
import Database.Trek.Db
import Database.Trek.Db.TestInterface.Types
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Time.QQ
import qualified Database.Postgres.Temp as Temp
import qualified Database.PostgreSQL.Transact as T
import qualified Database.PostgreSQL.Simple as Psql
import Database.PostgreSQL.Simple.SqlQQ
import Control.Exception
import Data.Text (Text)
import Control.Monad (void)
import Data.Pool

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
worldState = do
  xs <- fmap Psql.fromOnly <$> T.query_
    "SELECT table_name FROM information_schema.tables where table_schema = 'test'"
  ys <- fmap Psql.fromOnly <$> T.query_
    "SELECT table_name FROM information_schema.tables where table_schema = 'meta'"
  pure $ xs ++ ys

clear :: DB ()
clear = void $ T.execute_ [sql|
  DROP SCHEMA IF EXISTS meta CASCADE;
  CREATE SCHEMA meta;
  DROP SCHEMA IF EXISTS test CASCADE;
  CREATE SCHEMA test; |]

rollback :: DB a -> DB a
rollback = T.rollback
--
withSetup :: (Pool Psql.Connection -> IO ()) -> IO ()
withSetup f = do
  -- Helper to throw exceptions
  let throwE x = either throwIO pure =<< x

  throwE $ Temp.withDbCache $ \dbCache -> do
    let combinedConfig = Temp.defaultConfig <> Temp.cacheConfig dbCache
    Temp.withConfig combinedConfig $ \db ->
      f =<< createPool (Psql.connectPostgreSQL $ Temp.toConnectionString db) Psql.close 2 60 10

dbRunner :: (SpecStateM DB -> IO ()) -> IO ()
dbRunner f = withSetup $ \pool ->
  withResource pool $ \conn -> f $ SpecState (flip T.runDBTSerializable conn)
