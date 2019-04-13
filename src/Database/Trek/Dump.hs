module Database.Trek.Dump where

import Database.Trek.Types

loadSchema :: FilePath -> IO (Maybe Dump)
loadSchema = undefined

dumpSchema :: Config -> IO ()
dumpSchema = undefined

-- In qa mode we care about whether there is a diff between the current and new schema
-- if there is we can't just use the new one because we might not be able to load the data
-- if there is a difference we can present it
-- we can then dump the data and try to reload it
-- we can also, apply a migration, dump and reload?
applySchema :: Mode -> Dump -> IO ()
applySchema _mode _dump = undefined
