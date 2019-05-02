module Database.Trek.Dump where

import Database.Trek.Db
import Database.Trek.Types
import qualified Database.PostgreSQL.Simple as PS
import qualified Database.PostgreSQL.Simple.Types as PS
import Data.List.NonEmpty (NonEmpty)
import Data.ByteString (ByteString)
import qualified Data.Aeson as Aeson
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import System.Directory
import Control.Exception
import System.IO.Error
import System.Process
import System.Exit
import qualified Database.PostgreSQL.Simple.Options as PS
import qualified Data.ByteString.Char8 as BSC
import Data.Maybe

data Dump = Dump
  { dSchema     :: PS.Query
  , dAppliedMigrations :: [ProdMigration]
  } deriving (Show, Eq)

-- TODO I think this business of how it determines the dump does not exist
-- is not great. Should replace. Maybe an explict flag? Idk.
loadSchema :: FilePath -> IO (Maybe Dump)
loadSchema filePath = do
  e <- try $ do
        let sqlFilePath = filePath ++ "dump.sql"
            migrationsFilePath = filePath ++ "migrations.json"

        dSchema <- PS.Query <$> BS.readFile sqlFilePath
        mAppliedMigrations <- Aeson.eitherDecode <$> BSL.readFile migrationsFilePath
        dAppliedMigrations <- case mAppliedMigrations of
          Left err -> throwIO $ InvalidDumpMigrations migrationsFilePath err
          Right x  -> pure x

        pure $ Dump {..}
  pure $ case e of
    Left err
      | isDoesNotExistError err -> Nothing
    Right x -> Just x

-- loadDumpMigrations :: FilePath -> IO (Maybe [ProdMigration])

saveDump :: FilePath -> Dump -> IO ()
saveDump filePath Dump {..} = do
  createDirectory filePath
  let sqlFilePath = filePath ++ "dump.sql"
      migrationsFilePath = filePath ++ "migrations.json"
  BS.writeFile sqlFilePath $ PS.fromQuery dSchema
  BSL.writeFile migrationsFilePath $ Aeson.encode dAppliedMigrations

dumpSchema :: Runner -> String -> FilePath -> IO ()
dumpSchema (Runner runner) connString filePath = do

  let sqlFilePath = filePath ++ "dump.sql"
      migrationsFilePath = filePath ++ "migrations.json"

  system ("pg_dump '" ++ connString ++ "' --serializable-deferrable --schema-only --file=" ++ sqlFilePath) >>= \case
    ExitSuccess -> pure ()
    err -> throwIO err

  prodMigrations <- runner getAllProdMigrations
  BSL.writeFile migrationsFilePath $ Aeson.encode prodMigrations

throwExitCode :: ExitCode -> IO ()
throwExitCode = \case
  ExitSuccess -> pure ()
  err -> throwIO err


toDropDbOpts :: PS.Options -> ([(String, String)], String)
toDropDbOpts opts =
  ( catMaybes
    [ fmap ("--password",) $ PS.oPassword opts
    , fmap ("--username",) $ PS.oUser opts
    , fmap (\x -> ("--port", show x)) $ PS.oPort opts
    , fmap ("--host",)     $ PS.oHost opts
    ]
  , PS.oDbname opts
  )

dropDbOptsToCommandString :: ([(String, String)], String) -> String
dropDbOptsToCommandString (connOpts, dbName)
  =  "dropdb "
  ++ unwords (map (\(x, y) -> x ++ "=" ++ y) connOpts ++ [dbName])

createDbOptsToCommandString :: ([(String, String)], String) -> String
createDbOptsToCommandString (connOpts, dbName)
  =  "createdb "
  ++ unwords (map (\(x, y) -> x ++ "=" ++ y) connOpts ++ [dbName])

-- In qa mode we care about the data
-- We have to apply the migrations then dump the data and try to reload it
-- Err apply the migrations up to the level of prod that is
-- damn this is complex
-- We should dump the qa backup and the qa schema
-- that way we can revert if we want
-- if there is we can't just use the new one because
-- we might not be able to load the data
-- if there is a difference we can present it
-- we can then dump the data and try to reload it
-- we can also, apply a migration, dump and reload?
applySchema :: FilePath -> PS.Options -> Mode -> FilePath -> IO ()
applySchema qaBackupDir opts mode filePath = do
  let sqlFilePath = filePath ++ "dump.sql"
      migrationsFilePath = filePath ++ "migrations.json"
      connString = BSC.unpack $ PS.toConnectionString opts
  case mode of
    Qa -> error "applySchema: QA not implemented"
    Prod -> throwIO UnsafeApplySchemaOnProd
    Dev -> do
      throwExitCode =<< system (dropDbOptsToCommandString $ toDropDbOpts opts)
      throwExitCode =<< system (createDbOptsToCommandString $ toDropDbOpts opts)
      throwExitCode =<<
        system ("psql '" ++ connString ++ "' -v ON_ERROR_STOP=1 --single-transaction --file=" ++ sqlFilePath)