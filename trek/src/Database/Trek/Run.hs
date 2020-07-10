module Database.Trek.Run where
import qualified Database.Trek.Db as Db
import qualified Database.PostgreSQL.Simple.Options as P
import Data.Time.Format
import Data.Time
import System.IO
import Database.Trek.Parser
import System.FilePath.Posix
import Data.String
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BSC
import Crypto.Hash.SHA256
import Control.Exception
import Data.Typeable
import Database.PostgreSQL.Simple
import qualified Data.Aeson as Aeson
import System.Exit
import System.IO.Error
import Control.Monad
import Data.List.NonEmpty
import System.Directory
import Data.Foldable
import qualified Database.PostgreSQL.Transact as T
import Database.PostgreSQL.Simple.Transaction

data Errors = CouldNotParseMigration FilePath
            | DirectoryDoesNotExist FilePath
  deriving (Show, Eq, Typeable)

instance Exception Errors

eval :: Command -> IO ()
eval cmd = do
  e <- case cmd of
    Create name -> putStrLn =<< create name
    Apply partialOptions filePath -> do
      options <- either throwIO pure =<< completeOptions partialOptions

      traverse_ (BSL.putStrLn . Aeson.encode) =<< apply options filePath

  try e >>= \case
    Left err -> case err of
      CouldNotParseMigration filePath -> do
        putStrLn $ "Could not parse migration: " <> filePath
        exitWith $ ExitFailure 2
      DirectoryDoesNotExist filePath -> do
        putStrLn $ "Directory does not exist: " <> filePath
        exitWith $ ExitFailure 4

create :: String -> IO String
create name = do
  now <- getCurrentTime
  let (dir, theFileName) = splitFileName name
      outputFile = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" now <> "_" <> theFileName
      outputFilePath = dir </> outputFile
  withFile outputFilePath WriteMode (const $ pure ()) >>= \case
    Left i
      | isDoesNotExistError i -> throwIO $ DirectoryDoesNotExist outputFilePath
      | otherwise             -> throwIO i
    Right () -> pure ()

  pure outputFilePath

withOptions :: P.Options -> (Connection -> IO a) -> IO a
withOptions options f = undefined

parseVersion :: FilePath -> Maybe UTCTime
parseVersion = parseTimeM True defaultTimeLocale "%Y-%m-%dT%H-%M-%S"

computeHash :: String -> Binary Db.Hash
computeHash = Binary . hash . BSC.pack

makeInputMigration :: FilePath -> IO Db.InputMigration
makeInputMigration filePath = do
  inputVersion <- maybe (throwIO $ CouldNotParseMigration filePath) pure $ parseVersion filePath

  query <- readFile filePath
  let inputAction  = void $ T.execute_ $ fromString query
      inputHash    = computeHash query

  pure Db.InputMigration {..}

apply :: P.Options -> FilePath -> IO (Maybe Db.OutputGroup)
apply options dirPath = do
  xs <- mapM makeInputMigration =<< listDirectory dirPath
  withOptions options $ \conn -> nonEmpty xs $ \theNonEmpty ->
    T.runDBT (Db.apply =<< Db.inputGroup theNonEmpty) ReadCommitted conn
