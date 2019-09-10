module Database.Trek.Db.Types where
import Control.Exception

type Version = UTCTime

type Hash = ByteString

data HashedMigration = HashedMigration
  { hmVersion :: Version
  , hmName :: Text
  , hmHash :: Hash
  , hmCreatedAt :: UTCTime
  }
  deriving stock (Show, Eq, Ord, Generic)

data Migration = Migration
  { mVersion   :: Version
  , mQuery     :: ByteString
  , mExtraData :: Value
  }
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (FromRow)

data SetupResult = HadSetup | DidNotHaveSetup
  deriving (Show, Ord, Eq, Generic, Enum, Bounded)

data MigrationResult = Applied ApplicationId | NothingToApply
  deriving (Show, Ord, Eq, Generic, Enum, Bounded)

data NoSetup = NoSetup ByteString
  deriving (Show, Ord, Eq, Generic, Enum, Bounded)

data ApplicationId = ApplicationId Int

data ApplicationGroup a = ApplicationGroup
  { agExtraData      :: a
  , agHashMigrations :: NonEmpty HashedMigration
  }
