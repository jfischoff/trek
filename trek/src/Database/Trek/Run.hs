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
import Data.Monoid

data Errors = CouldNotParseMigration FilePath
            | DirectoryDoesNotExist FilePath
  deriving (Show, Eq, Typeable)

instance Exception Errors

eval :: [(InTransaction, Db.InputMigration)] -> Command -> IO ()
eval extraMigrations cmd = do
  let e = case cmd of
        Create name inTransaction -> putStrLn =<< create name inTransaction
        Apply partialOptions filePath -> do
          let options = either (const $ error "Partial db options. Not possible") id
                      $ Partial.completeOptions partialOptions

          traverse_ (BSL.putStrLn . encodePretty) =<< apply extraMigrations options filePath
        SetMigrated partialOptions start end filePath -> do
          let options = either (const $ error "Partial db options. Not possible") id
                      $ Partial.completeOptions partialOptions

          traverse_ (BSL.putStrLn . encodePretty) =<< setMigrated extraMigrations options (getLast start) (getLast end) filePath
        List partialOptions -> do
          let options = either (const $ error "Partial db options. Not possible") id
                      $ Partial.completeOptions partialOptions
          traverse_ (BSL.putStrLn . encodePretty) =<< list options

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

noTransactionSigil :: String
noTransactionSigil = "NO-TRANSACTION"

create :: String -> InTransaction -> IO String
create name inTransaction = do
  now <- getCurrentTime
  let (dir, theFileName) = takeBaseName <$> splitFileName name
      transactionModifier = case inTransaction of
                              InTransaction -> ""
                              NoTransaction -> "_" <> noTransactionSigil
      outputFile = formatTime defaultTimeLocale "%Y-%m-%dT%H-%M-%S" now <> "_" <> theFileName <> transactionModifier <> ".sql"
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

parseUseTransaction :: FilePath -> InTransaction
parseUseTransaction filePath =
  let
    safeLast = \case
      (x : []) -> Just x
      (_ : xs) -> safeLast xs
      [] -> Nothing
  in
    maybe InTransaction (bool InTransaction NoTransaction . (== noTransactionSigil)) $ safeLast $ splitOn "_" $ takeBaseName filePath

computeHash :: String -> Psql.Binary Db.Hash
computeHash = Psql.Binary . hash . BSC.pack

makeInputMigration :: FilePath -> IO (InTransaction, Db.InputMigration)
makeInputMigration filePath = do
  inputVersion <- maybe (throwIO $ CouldNotParseMigration filePath) pure $ parseVersion filePath
  let useTransaction = parseUseTransaction filePath

  theQuery <- readFile filePath

  let inputAction  = void $ T.execute_ $ fromString theQuery
      inputHash    = computeHash theQuery

  seq (length theQuery) $ pure (useTransaction, Db.InputMigration {..})

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

filterRange :: Maybe UTCTime -> Maybe UTCTime -> [(InTransaction, Db.InputMigration)] -> [(InTransaction, Db.InputMigration)]
filterRange start end migrations =
  let startCondition = maybe (const True) (<=) start
      endCondition   = maybe (const True) (>=) end
  in filter (\(_, x) -> startCondition (Db.inputVersion x) && endCondition (Db.inputVersion x)) migrations

buildMigrations :: FilePath -> IO [(InTransaction, Db.InputMigration)]
buildMigrations dirPath
  = mapM (makeInputMigration . (dirPath </>)) . filter ((==".sql") . takeExtension)
  =<< listDirectory dirPath

setMigrated :: [(InTransaction, Db.InputMigration)] -> P.Options -> Maybe UTCTime -> Maybe UTCTime -> FilePath -> IO [OutputGroup]
setMigrated extraMigrations options start end dirPath = do
  xs <- buildMigrations dirPath

  applyWith Db.SetMigrate (filterRange start end $ extraMigrations <> xs) options

apply :: [(InTransaction, Db.InputMigration)] -> P.Options -> FilePath -> IO [OutputGroup]
apply extraMigrations options dirPath = do
  xs <- buildMigrations dirPath
  applyWith Db.RunMigrations (extraMigrations <> xs) options

applyWith :: Db.ApplyType -> [(InTransaction, Db.InputMigration)] -> P.Options -> IO [OutputGroup]
applyWith applyType allMigrations options = do
  let transactionGroups = groupBy' $ sortBy (compare `on` (Db.inputVersion . snd)) allMigrations
  withOptions options $ \conn -> fmap (fmap OutputGroup)
    $ fmap catMaybes
    $ forM (mapMaybe (sequence . fmap nonEmpty) transactionGroups) $ \(useTransaction, theNonEmpty) -> do
        let
          runner action = case useTransaction of
                     InTransaction -> T.runDBT action Psql.ReadCommitted conn
                     NoTransaction -> T.runDBTNoTransaction action conn
        runner $ Db.apply applyType =<< Db.inputGroup theNonEmpty

groupBy' :: [(InTransaction, a)] -> [(InTransaction, [a])]
groupBy' xs = groupBy ((==) `on` fst) xs <&> \ys ->
  ( fromMaybe InTransaction (listToMaybe $ fmap fst ys)
  , snd <$> ys
  )

list :: P.Options -> IO [OutputGroup]
list options = fmap (map OutputGroup) $
  withOptions options $ \conn -> T.runDBT Db.listApplications Psql.ReadCommitted conn
