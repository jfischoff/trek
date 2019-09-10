module Database.Trek.Db.Types where
import Data.List.NonEmpty (NonEmpty)
import Data.Aeson (Value)
import Data.Time (UTCTime)
import GHC.Generics
import Data.ByteString (ByteString)
import Data.Text (Text)

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
  deriving stock (Show, Eq, Generic)

data SetupResult = HadSetup | DidNotHaveSetup
  deriving (Show, Ord, Eq, Generic, Enum, Bounded)

data NoSetup = NoSetup
  deriving (Show, Ord, Eq, Generic)

data ApplicationId = ApplicationId Int

data ApplicationGroup = ApplicationGroup
  { agExtraData      :: Value
  , agHashMigrations :: NonEmpty HashedMigration
  } deriving (Show, Eq, Generic)

toHashedMigration :: Migration -> HashedMigration
toHashedMigration = error "toHashedMigration"