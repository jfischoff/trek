module Database.Trek.TestImpl where
import System.Process
import System.Exit

setup :: IO (ExitCode, String, String)
setup = readProcessWithExitCode "trek" ["setup"] ""

teardown :: IO (ExitCode, String, String)
teardown = readProcessWithExitCode "trek" ["teardown"] ""

migrate :: FilePath -> IO (ExitCode, String, String)
migrate migrationFile = readProcessWithExitCode "trek" ["migrate", "--no-warn-hash-conflicts", show migrationFile] ""

hashConflicts :: FilePath -> IO (ExitCode, String, String)
hashConflicts migrationFile = readProcessWithExitCode "trek" ["hash-conflicts", show migrationFile] ""

