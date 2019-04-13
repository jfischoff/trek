module Database.Trek.Types where
--import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Transact
import Data.Time
import Data.Text (Text)
-- import Database.PostgreSQL.Simple.Options
import Data.Pool
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty)

type Version = UTCTime

type PointInTime = String

type Hash = ByteString

data MigrationExceptions
  = FailedToParseMigratrionFilePaths [FilePath]

data Mode = Dev | Qa | Prod

data Config = Config
  { cMigrationDirectory :: FilePath
  , cSchemaFilePath :: FilePath
  , cPool :: Pool PS.Connection
  , cRollbacker :: PointInTime -> IO ()
  , cBackerUpper :: IO PointInTime
  , cDropDb :: DB ()
  }

data Dump = Dump
  { dSchema     :: PS.Query
  , dAppliedMigrations :: NonEmpty [ProdMigration]
  }

data Migration = Migration
  { mVersion :: Version
  , mName :: Text
  , mQuery :: PS.Query
  }

data DevMigration = DevMigration
  { dmVersion :: Version
  , dmName :: Text
  , dmCreate :: UTCTime
  , dmHash :: Maybe Hash
  }

data ProdMigration = ProdMigration
  { pmVersion :: Version
  , pmName :: Text
  , pmCreate :: UTCTime
  }