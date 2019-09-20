module Database.Trek.Db
  ( -- * Life cycle management
    setup
  , teardown
    -- * Migration
  , hashConflicts
  , apply
  -- * Queriesi
  , listApplications
  -- * Types
  , InputMigration (..)
  , Version
  , Hash
  , DB
  , NoSetup(..)
  , AlreadySetup (..)
  , OutputGroup (..)
  , InputGroup (..)
  , inputMigration
  , inputGroup
  , Time
  )
  where
import Database.PostgreSQL.Transact
import Data.List.NonEmpty (NonEmpty)
import Data.Time (UTCTime)
import Data.ByteString (ByteString)
import qualified Database.PostgreSQL.Simple as Psql
import qualified Database.PostgreSQL.Simple.Types as Psql
import Database.PostgreSQL.Simple.SqlQQ
import Data.String.Here.Interpolated (i)
import Control.Monad (void)
-- import Data.Text (Text)
import Control.Monad.Catch

type Version = UTCTime

type Hash = ByteString

data NoSetup = NoSetup

data AlreadySetup = AlreadySetup

type Time = UTCTime

data InputMigration = InputMigration
  { inputAction :: DB ()
  , inputVersion :: Version
  , inputHash :: Hash
  }

type GroupId = Int

data OutputGroup = OutputGroup GroupId
  deriving (Show, Eq)

data InputGroup = InputGroup
  { inputGroupMigrations :: NonEmpty InputMigration
  }

-- InputMigration constructor
inputMigration :: DB () -> Version -> Hash -> InputMigration
inputMigration = InputMigration

-- InputGroup constructor
inputGroup :: NonEmpty InputMigration -> InputGroup
inputGroup = InputGroup

-------------------------------------------------------------------------------
-- Helpers
-------------------------------------------------------------------------------
verifyTableExists :: Psql.Query -> DB Bool
verifyTableExists tableName = Psql.fromOnly . head <$> query_ [i|
  SELECT EXISTS (
  SELECT 1
  FROM   information_schema.tables
  WHERE  table_schema = ANY(regexp_split_to_array(current_setting('search_path'), '\,\s*'))
  AND    table_name = '${tableName}'
  );
  |]

setSchema :: Psql.Query -> DB ()
setSchema schemaName = void $ execute_ [i| SET search_path=${schemaName};|]

getSchema :: DB Psql.Query
getSchema = Psql.Query . Psql.fromOnly . head <$> query_ [sql|SHOW search_path|]

withMetaSchema :: DB a -> DB a
withMetaSchema action = mask $ \restore -> do
  oldSchema <- getSchema
  setSchema "meta"
  restore action `finally` setSchema oldSchema


onSetup :: (Bool -> Bool) -> DB a -> DB (Maybe a)
onSetup onF action = withMetaSchema $ do
  setupExists <- verifyTableExists "applications"
  if onF setupExists
    then Just <$> action
    else pure Nothing

withSetup :: DB a -> DB (Maybe a)
withSetup = onSetup id

withoutSetup :: DB a -> DB (Maybe a)
withoutSetup = onSetup not

setup :: DB (Maybe ())
setup = withoutSetup $ void $ execute_ [sql|
  CREATE TABLE applications
  ( id SERIAL PRIMARY KEY
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp
  );

  CREATE TABLE actions
  ( version TIMESTAMP WITH TIME ZONE PRIMARY KEY
  , name text NOT NULL
  , hash bytea NOT NULL
  , application_id int NOT NULL REFERENCES applications ON DELETE CASCADE
  , created_at TIMESTAMP WITH TIME ZONE NOT NULL DEFAULT current_timestamp
  );

  |]

teardown :: DB (Maybe ())
teardown = withSetup $ void $ execute_ [sql|
    DROP TABLE actions;
    DROP TABLE applications;
  |]

apply :: InputGroup -> DB (Maybe (Maybe OutputGroup))
apply _ = pure $ pure Nothing

listApplications :: DB (Maybe [OutputGroup])
listApplications = pure $ pure []

hashConflicts  :: [InputMigration]-> DB (Maybe [Version])
hashConflicts _ = pure $ pure []