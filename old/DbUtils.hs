module Tests.Database.Trek.DbUtils (withTestDB, withDB, verifyTableExists) where

import qualified Database.Postgres.Temp as Temp
import qualified Database.PostgreSQL.Simple as Psql
import Control.Monad (void)
import Test.Hspec
import Database.PostgreSQL.Transact
import qualified Data.ByteString.Char8 as BSC
import Control.Exception
import Text.InterpolatedString.Perl6

verifyTableExists :: String -> DB Bool
verifyTableExists tableName = Psql.fromOnly . head <$> query_ [qq|
  SELECT EXISTS (
  SELECT 1
  FROM   information_schema.tables
  WHERE  table_schema = 'public'
  AND    table_name = '{tableName}'
  );
  |]

createTempConnection :: IO (Psql.Connection, Temp.DB)
createTempConnection = do
  db <- either throwIO pure =<< Temp.start
    [ ("archive_mode", "on")
    , ("wal_level", "replica")
    , ("max_wal_senders", "2")
    ]
  let connString = Temp.connectionString db
  connection <- Psql.connectPostgreSQL $ BSC.pack connString
  return (connection, db)

-- Either run the job or not
setupDB :: IO (Psql.Connection, Temp.DB)
setupDB = do
  (connection, db) <- createTempConnection
  let url = Temp.connectionString db
  print url
  return (connection, db)

withTestDB :: SpecWith (Psql.Connection, Temp.DB) -> Spec
withTestDB = beforeAll setupDB . afterAll stopDB

stopDB :: (Psql.Connection, Temp.DB) -> IO ()
stopDB (c, x) = void $ Psql.close c >> Temp.stop x

withDB :: DB a -> (Psql.Connection, Temp.DB) -> IO a
withDB x (c, _) = runDBTSerializable x c

