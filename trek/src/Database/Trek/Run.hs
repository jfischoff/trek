module Database.Trek.Run where
import qualified Database.Trek.Db as Db
import qualified Database.PostgreSQL.Simple.Options as P
import qualified Database.PostgreSQL.Simple.PartialOptions as Partial
import Data.Bool (bool)
import Data.Function (on)
import Data.Functor ((<&>))
import Data.List (groupBy, sortBy)
import Data.Time.Format
import Data.Time
import System.IO
import Database.Trek.Parser
import System.FilePath.Posix
import Data.String
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Data.ByteString.Char8 as BSC
import Crypto.Hash.SHA256
import Control.Exception
import Data.Typeable
import Data.Aeson
import System.Exit
import System.IO.Error
import Control.Monad
import Data.List.NonEmpty (nonEmpty)
import System.Directory
import Data.Foldable
import qualified Database.PostgreSQL.Transact as T
import qualified Database.PostgreSQL.Simple.Transaction as Psql
import qualified Data.ByteString.Base64 as Base64
import qualified Database.PostgreSQL.Simple as Psql
import Data.List.Split
import Data.Maybe
import Data.Aeson.Encode.Pretty

data Errors = CouldNotParseMigration FilePath
            | DirectoryDoesNotExist FilePath
  deriving (Show, Eq, Typeable)

instance Exception Errors

eval :: [(Bool, Db.InputMigration)] -> Command -> IO ()
eval extraMigrations cmd = do
  let e = case cmd of
        Create name -> putStrLn =<< create name
        Apply partialOptions filePath -> do
          let options = either (const $ error "Partial db options. Not possible") id
                      $ Partial.completeOptions partialOptions

          traverse_ (BSL.putStrLn . encodePretty) =<< apply extraMigrations options filePath

  let action = try e >>= \case
        Left err -> case err of
          CouldNotParseMigration filePath -> do
            hPutStrLn stderr $ "Could not parse migration: " <> filePath
            exitWith $ ExitFailure 2
          DirectoryDoesNotExist filePath -> do
            hPutStrLn stderr $ "Directory does not exist: " <> filePath
            exitWith $ ExitFailure 4
        Right () -> pure ()

  try action >>= \case
    Left (finalErr :: SomeException) -> do
      hPutStrLn stderr $ "Unknown error: " <> show finalErr
      exitWith $ ExitFailure 1
    Right () -> pure ()

create :: String -> IO String
create name = do
  now <- getCurrentTime
  let (dir, theFileName) = splitFileName name
      outputFile = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" now <> "_" <> theFileName
      outputFilePath = dir </> outputFile
  try (withFile outputFilePath WriteMode (const $ pure ())) >>= \case
    Left i
      | isDoesNotExistError i -> throwIO $ DirectoryDoesNotExist outputFilePath
      | otherwise             -> throwIO i
    Right () -> pure ()

  pure outputFilePath

withOptions :: P.Options -> (Psql.Connection -> IO a) -> IO a
withOptions options f =
  bracket (Psql.connectPostgreSQL $ P.toConnectionString options) Psql.close f

parseVersion :: FilePath -> Maybe UTCTime
parseVersion filePath = do
  date <- listToMaybe $ splitOn "_" $ takeFileName filePath
  parseTimeM True defaultTimeLocale "%Y-%m-%dT%H-%M-%S" date

parseUseTransaction :: FilePath -> Bool
parseUseTransaction filePath =
  let
    safeLast = \case
      (x : []) -> Just x
      (_ : xs) -> safeLast xs
      [] -> Nothing
  in
    maybe True (/= "NO-TRANSACTION") $ safeLast $ splitOn "_" $ takeBaseName filePath

computeHash :: String -> Psql.Binary Db.Hash
computeHash = Psql.Binary . hash . BSC.pack

makeInputMigration :: FilePath -> IO (Bool, Db.InputMigration)
makeInputMigration filePath = do
  inputVersion <- maybe (throwIO $ CouldNotParseMigration filePath) pure $ parseVersion filePath
  let useTransaction = parseUseTransaction filePath

  theQuery <- readFile filePath
  let inputAction  = void $ T.execute_ $ fromString theQuery
      inputHash    = computeHash theQuery

  pure (useTransaction, Db.InputMigration {..})

newtype OutputMigration = OutputMigration Db.OutputMigration

instance ToJSON OutputMigration where
  toJSON (OutputMigration (Db.OutputMigration {..})) = object
    [ "version" .= omVersion
    , "hash"    .= binaryToJSON omHash
    ]

newtype OutputGroup = OutputGroup Db.OutputGroup
  deriving (Show, Eq)

instance ToJSON OutputGroup where
  toJSON (OutputGroup (Db.OutputGroup {..})) = do
    object
      [ "id"         .= groupIdToJSON ogId
      , "created_at" .= ogCreatedAt
      , "migrations" .= fmap OutputMigration ogMigrations
      ]

binaryToJSON :: Psql.Binary BSC.ByteString -> Value
binaryToJSON (Psql.Binary x) = toJSON $ BSC.unpack $ Base64.encode x

groupIdToJSON :: Db.GroupId -> Value
groupIdToJSON (Db.GroupId x) = binaryToJSON x

apply :: [(Bool, Db.InputMigration)] -> P.Options -> FilePath -> IO [OutputGroup]
apply extraMigrations options dirPath = do
  xs <- mapM (makeInputMigration . (dirPath </>)) . filter ((==".sql") . takeExtension)
    =<< listDirectory dirPath
  let transactionGroups = groupBy' $ sortBy (compare `on` (Db.inputVersion . snd)) $ extraMigrations <> xs
  withOptions options $ \conn -> fmap (fmap OutputGroup)
    $ fmap catMaybes
    $ forM (mapMaybe (sequence . fmap nonEmpty) transactionGroups) $ \(useTransaction, theNonEmpty) -> do
        let
          withTransaction action = T.runDBT action Psql.ReadCommitted conn
          withoutTransaction action = T.runDBTNoTransaction action conn
        bool withoutTransaction withTransaction useTransaction
          $ Db.apply =<< Db.inputGroup theNonEmpty

groupBy' :: [(Bool, a)] -> [(Bool, [a])]
groupBy' xs = groupBy ((==) `on` fst) xs <&> \ys -> (all fst ys, snd <$> ys)
