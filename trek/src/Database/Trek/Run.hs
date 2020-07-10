module Database.Trek.Run where
import Database.Trek.Db
import qualified Database.PostgreSQL.Simple.Options as P
import Data.Time.Format
import Data.Time
import System.IO
import Database.Trek.Parser

eval :: Command -> IO ()
eval = \case
  Create name -> putStrLn =<< create name
  Apply _options _filePath -> error "not implemented" -- BSL.putStrLn . Aeson.encode =<< apply options filePath

create :: String -> IO String
create name = do
  now <- getCurrentTime
  let outputFile = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S.sql" now <> "_" <> name
  withFile outputFile WriteMode (const $ pure ())
  pure outputFile

apply :: P.Options -> FilePath -> IO (Maybe OutputGroup)
apply = undefined
