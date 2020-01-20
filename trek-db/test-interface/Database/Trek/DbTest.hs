module Database.Trek.DbTest
  ( foo
  , bar
  , quux
  , inputGroup
  , toOutput
  , rollback
  , dbRunner
  , WorldState
  , worldState
  , clear
  , InputMigration
  ) where
import Database.Trek.Db
import Database.Trek.Db.TestInterface.Types
-- import Data.List.NonEmpty (NonEmpty)
-- import qualified Data.List.NonEmpty as NonEmpty
import Data.Time.QQ
import qualified Database.Postgres.Temp as Temp
import qualified Database.PostgreSQL.Transact as T
import qualified Database.PostgreSQL.Simple as Psql
import Database.PostgreSQL.Simple.SqlQQ
import Control.Exception
-- import Data.Text (Text)
import Control.Monad (void)
import Data.Pool
import qualified Data.List.NonEmpty as NonEmpty
import Database.PostgreSQL.Simple.Types

type WorldState = String

clear :: DB ()
clear = void $ T.execute_ [sql|
  DROP SCHEMA IF EXISTS meta CASCADE;
  CREATE SCHEMA meta;
  DROP SCHEMA IF EXISTS test CASCADE;
  CREATE SCHEMA test; |]

worldState :: DB WorldState
worldState = do
  xs <- fmap Psql.fromOnly <$> T.query_
    "SELECT table_name FROM information_schema.tables where table_schema = 'test'"
  ys <- fmap Psql.fromOnly <$> T.query_
    "SELECT table_name FROM information_schema.tables where table_schema = 'meta'"
  pure $ xs ++ ys

createFoo :: DB ()
createFoo = void $ T.execute_ [sql| CREATE SCHEMA test; CREATE TABLE test.foo (id SERIAL PRIMARY KEY)|]

createBar :: DB ()
createBar = void $ T.execute_ [sql| CREATE TABLE test.bar (id SERIAL PRIMARY KEY)|]

createQuux :: DB ()
createQuux = void $ T.execute_ [sql| CREATE TABLE test.quux (id SERIAL PRIMARY KEY)|]

foo :: InputMigration
foo = InputMigration createFoo [utcIso8601| 2048-12-01 |] (Binary "extra")

bar :: InputMigration
bar = InputMigration createBar  [utcIso8601| 2025-12-01 |] (Binary "migration-2025-12-01")

quux :: InputMigration
quux = InputMigration createQuux [utcIso8601| 2025-12-02 |] (Binary "migration-2025-01-01")

toOutputMigration :: InputMigration -> OutputMigration
toOutputMigration InputMigration {..} = OutputMigration
  { omVersion = inputVersion
  , omHash    = inputHash
  }

toOutput :: InputGroup -> DB OutputGroup
toOutput InputGroup {..} = pure OutputGroup
  { ogId = GroupId $ Binary $ makeGroupHash $ NonEmpty.toList inputGroupMigrations
  , ogCreatedAt = inputGroupCreateAd
  , ogMigrations = fmap toOutputMigration inputGroupMigrations
  }

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
