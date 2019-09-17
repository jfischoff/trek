module Database.Trek.Db
  ( -- * Life cycle management
    setup
  , teardown
    -- * Migration
  , hashConflicts
  , migrate
  -- * Queriesi
  , listMigrations
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
import Database.PostgreSQL.Transact(DB)
import Data.List.NonEmpty (NonEmpty)
import Data.Time (UTCTime)
import Data.ByteString (ByteString)


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

setup :: DB (Either AlreadySetup ())
setup = pure $ Right ()

-- Requires setup
teardown :: DB (Either NoSetup ())
teardown = pure $ Right ()

migrate :: InputGroup -> DB (Either NoSetup (Maybe OutputGroup))
migrate _ = pure $ pure Nothing

listMigrations :: DB (Either NoSetup [OutputGroup])
listMigrations = pure $ pure []

hashConflicts  :: [InputMigration]-> DB (Either NoSetup [Version])
hashConflicts _ = pure $ pure []