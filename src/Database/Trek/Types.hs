module Database.Trek.Types where
--import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Types as PS
import Database.PostgreSQL.Transact
import Data.Time
import Data.Text (Text)
import Control.Exception
import Data.Typeable
import Data.Pool
import Data.ByteString (ByteString)
import Crypto.Hash
import Data.ByteArray as BA
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Generics

type Version = UTCTime

type PointInTime = String

type Hash = ByteString

data MigrationException
  = NoMigrationTables
  | FailedToParseMigratrionFilePaths [FilePath]
  deriving(Show, Eq, Ord, Typeable)

instance Exception MigrationException

data Mode = Dev | Qa | Prod
  deriving(Show, Eq, Ord, Enum, Bounded)

data Migration = Migration
  { mVersion :: Version
  , mName :: Text
  , mQuery :: ByteString
  } 
  deriving stock (Show, Eq, Ord, Generic)

hashMigration :: Migration -> Hash
hashMigration m = BA.copyAndFreeze (hashWith SHA1 $ mQuery m) (const $ pure ())
