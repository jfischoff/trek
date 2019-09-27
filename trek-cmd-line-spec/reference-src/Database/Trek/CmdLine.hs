module Database.Trek.CmdLine where
import System.Exit(ExitCode)

-- Main API
setup :: [String] -> IO (ExitCode, String, String)
setup = undefined
-- Requires setup
teardown :: [String] -> IO (ExitCode, String, String)
teardown = undefined

apply :: [String] -> IO (ExitCode, String, String)
apply = undefined

listApplications :: [String] -> IO (ExitCode, String, String)
listApplications = undefined

hashConflicts :: [String] -> IO (ExitCode, String, String)
hashConflicts = undefined

