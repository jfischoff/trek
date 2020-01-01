module Database.Trek.ToInterfaceImpl where
import qualified Database.Trek.TestImpl as I
import Control.Exception
import System.Exit
import Data.Aeson
import Data.Time
import qualified Data.ByteString.Lazy.Char8 as BSLC

type DB = IO
type Version = UTCTime
type InputGroup = FilePath
type InputMigration = FilePath
type OutputGroup = Value

setup :: DB (Maybe ())
setup = do
  (exitCode, _, _) <- I.setup
  case exitCode of
    ExitSuccess -> pure $ pure ()
    ExitFailure 8 -> pure Nothing
    e -> throwIO e

teardown :: DB (Maybe ())
teardown = do
  (exitCode, _, _) <- I.setup
  case exitCode of
    ExitSuccess -> pure $ pure ()
    ExitFailure 16 -> pure Nothing
    e -> throwIO e

apply :: InputGroup -> DB (Maybe (Maybe OutputGroup))
apply filePath = do
  (exitCode, stdOut, _) <- I.migrate filePath
  case exitCode of
    ExitSuccess -> case decode (BSLC.pack stdOut) of
      Nothing -> throwIO $ userError "failed to decode json"
      Just x  -> pure $ Just $ Just x
    ExitFailure 16 -> pure Nothing
    ExitFailure 64 -> pure $ Just Nothing
    e -> throwIO e

-- hashConflicts :: [InputMigration] -> DB (Maybe [Version])
-- hashConflicts filePaths = do
--   (exitCode, stdOut, _) <- I.migrate filePath
