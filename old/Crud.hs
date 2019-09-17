module Database.Trek.Db.Crud where

import Database.Trek.Db.Types

import Database.PostgreSQL.Transact
import Data.Time
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import Data.Int
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics
import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as PS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import Crypto.Hash
import Data.ByteArray as BA
import Data.Traversable

listMigrations :: DB [HashedMigration]
listMigrations = mapSqlError $ query_ [sql|
  SELECT version, name, hash, created_at
  FROM migrations
  ORDER BY created_at ASC
  |]

-------------------------------------------------------------------------------
-- Internal types for applying migrations
-------------------------------------------------------------------------------
newtype ApplicationId = ApplicationId { unApplicationId :: Int64 }
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromField, ToField)
  deriving (ToRow) via (PS.Only ApplicationId)

instance FromRow ApplicationId where
  fromRow = fmap PS.fromOnly $ fromRow

data ApplicationRow = ApplicationRow
  { arId         :: ApplicationId
  , arCreatedAt  :: UTCTime
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (FromRow)

data ApplicationGroup = ApplicationGroup
  { aMigrations :: [MigrationRow]
  } deriving (Show, Eq, Ord, Generic)

-- UNDO
-- Probably doesn't matter. Mostly made to make testing easier
-- but easier to test the interface correctly so probably not
-- needed
data Dir = To | From

type family Direction (a :: Dir) where
  Direction To = Maybe
  Direction From = Identity

data MigrationRow dir = MigrationRow
  { mrVersion       :: Version
  , mrName          :: Text
  , mrHash          :: Hash
  , mrApplicationId :: ApplicationId
  , mrCreatedAt     :: Direction dir UTCTime
  } deriving stock (Show, Eq, Ord, Generic)
    deriving anyclass (FromRow)
-------------------------------------------------------------------------------
-- Conversion from the public to private migration type
-------------------------------------------------------------------------------
hashQuery :: BS.ByteString -> Hash
hashQuery m = BA.copyAndFreeze (hashWith SHA1 m) (const $ pure ())

createApplication :: DB ApplicationRow
createApplication = fmap head $ query_ [sql|
  INSERT INTO applications
  DEFAULT VALUES
  RETURNING id, created_at
  |]

getAllApplicationRecords :: DB (Map ApplicationId ApplicationGroup)
getAllApplicationRecords = do
  as <- query_ [sql|
      SELECT id, created_at
      FROM applications
      ORDER BY created_at
    |]
  fmap Map.fromList $ forM as $ \ApplicationRow {..} -> do
    aMigrations <- query [sql|
      SELECT version, name, hash, application_id, created_at
      FROM migrations
      WHERE application_id = ?
      |] arId
    pure (arId, ApplicationGroup {..})

insertMigration :: MigrationRow To -> DB ()
insertMigration migration = void $ execute
  [sql|
    INSERT INTO migrations
    (version, name, hash, application_id)
    VALUES
    (?, ?, ?, ?)
  |] migration

versionExists :: Version -> DB Bool
versionExists = fmap (PS.fromOnly . head) . query [sql|
  SELECT EXISTS (
    SELECT 1
    FROM migrations
    WHERE version = ?
  )
  |] . PS.Only