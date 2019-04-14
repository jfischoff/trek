module Database.Trek.Types where
--import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Transact
import Data.Time
import Data.Text (Text)
-- import Database.PostgreSQL.Simple.Options
import Data.Pool
import Data.ByteString (ByteString)

type Version = UTCTime

type PointInTime = String

type Hash = ByteString

data MigrationExceptions
  = FailedToParseMigratrionFilePaths [FilePath]

data Mode = Dev | Qa | Prod

data Migration = Migration
  { mVersion :: Version
  , mName :: Text
  , mQuery :: PS.Query
  }
