module Database.Trek.Types where
import qualified Database.PostgreSQL.Simple as PS
import Data.Time
import Data.Text (Text)
import Control.Exception
import Data.Typeable
import Data.ByteString (ByteString)
import Crypto.Hash
import Data.ByteArray as BA
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics
import Data.Int

type Version = UTCTime

type PointInTime = String

type Hash = ByteString

newtype ApplicationId = ApplicationId { unApplicationId :: Int64 }
  deriving stock (Show, Eq, Ord)
  deriving newtype (FromField, ToField)
  deriving (ToRow) via (PS.Only ApplicationId)

instance FromRow ApplicationId where
  fromRow = fmap PS.fromOnly $ fromRow

data MigrationException
  = NoSetup
  | SetupRanTwice
  | FailedToParseMigratrionFilePaths [FilePath]
  | MigrationAlreadyApplied Version
  | EmptyApplication ApplicationId
  | InvalidMigrationVersion [Version]
  | InvalidDumpMigrations FilePath String
  | UnsafeApplySchemaOnProd
  | UnknownSqlException PS.SqlError
  deriving(Show, Eq, Typeable)

instance Exception MigrationException

data Migration = Migration
  { mVersion :: Version
  , mName :: Text
  , mQuery :: ByteString
  }
  deriving stock (Show, Eq, Ord, Generic)

hashQuery :: ByteString -> Hash
hashQuery m = BA.copyAndFreeze (hashWith SHA1 m) (const $ pure ())
