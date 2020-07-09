module Database.Trek.Run where
import Database.Trek.Db
import qualified Database.PostgreSQL.Simple.Options as P
import Data.Time.Format.ISO8601
import Data.Time
import System.IO

create :: String -> IO String
create name = do
  now <- getCurrentTime
  let outputFile = name <> "_" <> iso8601Show now
  withFile outputFile WriteMode (const $ pure ())
  pure outputFile

apply :: P.Options -> FilePath -> IO (Maybe OutputGroup)
apply = undefined
