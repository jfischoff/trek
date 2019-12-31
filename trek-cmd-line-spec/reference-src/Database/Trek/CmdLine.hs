module Database.Trek.CmdLine where
import System.Exit(ExitCode)

-- I should have something that can turn a core db interface into this
-- the parsers are a seperate package that is used by the intermediate package that
-- creates the command line interface

-- I don't need to depend on something else to make a reference implementation
-- I can just do it here. I don't feel like it now I just want to make the parser
-- package

-- I need to do it to make the tests pass



apply :: [String] -> IO (ExitCode, String, String)
apply = undefined

listApplications :: [String] -> IO (ExitCode, String, String)
listApplications = undefined

