module Tests.Database.Trek.RunSpec where
import Database.Trek.Parser
import Database.Trek.Run
import Test.Hspec
import Data.Maybe
import Data.List.Split
import Data.Time.Format
import System.IO.Temp
import System.Directory
import Control.Exception
import Data.Time
import System.FilePath.Posix
import Data.List.NonEmpty (NonEmpty(..))
import qualified Database.Postgres.Temp as Temp
import Control.Concurrent
import Control.Concurrent.Async
import Data.IORef
import Data.Foldable
import qualified Database.PostgreSQL.Simple.Options as P
import qualified Database.Trek.Db as Db
import Database.PostgreSQL.Simple.Types
import Data.Time.QQ
import qualified Database.PostgreSQL.Simple as Psql
import qualified Database.PostgreSQL.Simple.SqlQQ as Psql
import Paths_trek_app (getDataDir)
import qualified Database.PostgreSQL.Transact as T
import Control.Monad (void)

aroundAll :: forall a. ((a -> IO ()) -> IO ()) -> SpecWith a -> Spec
aroundAll withFunc specWith = do
  (var, stopper, asyncer) <- runIO $
    (,,) <$> newEmptyMVar <*> newEmptyMVar <*> newIORef Nothing
  let theStart :: IO a
      theStart = do

        thread <- async $ do
          withFunc $ \x -> do
            putMVar var x
            takeMVar stopper
          pure $ error "Don't evaluate this"

        writeIORef asyncer $ Just thread

        either pure pure =<< (wait thread `race` takeMVar var)

      theStop :: a -> IO ()
      theStop _ = do
        putMVar stopper ()
        traverse_ cancel =<< readIORef asyncer

  beforeAll theStart $ afterAll theStop $ specWith

foo :: String
foo = "CREATE SCHEMA IF NOT EXISTS test; CREATE TABLE test.foo (id SERIAL PRIMARY KEY)"

bar :: String
bar = "CREATE SCHEMA IF NOT EXISTS test; CREATE TABLE test.bar (id SERIAL PRIMARY KEY)"

quux :: String
quux = "CREATE SCHEMA IF NOT EXISTS test; CREATE TABLE test.quux (id SERIAL PRIMARY KEY)"

inputGroup :: NonEmpty FilePath -> IO FilePath
inputGroup = error "inputGroup"

withSetup :: (P.Options -> IO a) -> IO a
withSetup f = do
  -- Helper to throw exceptions
  let throwE x = either throwIO pure =<< x

  throwE $ Temp.withDbCache $ \dbCache -> do
    let combinedConfig = Temp.defaultConfig <> Temp.cacheConfig dbCache
    Temp.withConfig combinedConfig $ \db -> f $ Temp.toConnectionOptions db

checkTables :: Psql.Connection -> IO [String]
checkTables conn = fmap Psql.fromOnly <$> Psql.query_ conn
  "SELECT CAST(table_name AS varchar) FROM information_schema.tables where table_schema = 'test' ORDER BY table_name"

spec :: Spec
spec = do
  describe "Database.Trek.Run" $ do
    it "creates a file" $ do
      withSystemTempDirectory "trek-test" $ \tmp -> do
        old <- getCurrentDirectory
        bracket_ (setCurrentDirectory tmp) (setCurrentDirectory old) $ do
          createDirectory "path"
          let name = "path/migration.sql"
          output <- create name InTransaction
          let (dir, theFileName) = splitFileName output
              [date, actualName] = splitOn "_" theFileName
          dir `shouldBe` "path/"
          actualName `shouldBe` "migration.sql"
          isJust (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H-%M-%S" date :: Maybe UTCTime) `shouldBe` True
          doesFileExist output `shouldReturn` True
    it "creates a file with NO-TRANSACTION" $ do
      withSystemTempDirectory "trek-test" $ \tmp -> do
        old <- getCurrentDirectory
        bracket_ (setCurrentDirectory tmp) (setCurrentDirectory old) $ do
          createDirectory "path"
          let name = "path/migration.sql"
          output <- create name NoTransaction
          let (dir, theFileName) = splitFileName output
              [date, actualName, noTransaction] = splitOn "_" theFileName
          dir `shouldBe` "path/"
          actualName `shouldBe` "migration"
          noTransaction `shouldBe` "NO-TRANSACTION.sql"
          isJust (parseTimeM True defaultTimeLocale "%Y-%m-%dT%H-%M-%S" date :: Maybe UTCTime) `shouldBe` True
          doesFileExist output `shouldReturn` True
    it "parses 'no transaction' indications" $ do
      parseUseTransaction "migrations/2020-11-16T12:00:00.0000_do-things_NO-TRANSACTION.sql"
        `shouldBe` NoTransaction
      parseUseTransaction "migrations/2020-11-16T12:00:00.0000_do-things.sql"
        `shouldBe` InTransaction

  aroundAll withSetup $ describe "Database.Trek.Run.apply" $ do
    it "empty directory does nothing" $ \options -> withSystemTempDirectory "trek-test" $ \tmp -> do
      apply [] options tmp `shouldReturn` []
      -- Doing it twice should be the same
      apply [] options tmp `shouldReturn` []

    it "list of nothing returns nothing" $ \options -> do
      list options `shouldReturn` []

    it "standard migrations succeed" $ \options -> do
      dataDir <- fmap (</> "data") getDataDir

      [OutputGroup (Db.OutputGroup {ogMigrations})] <- apply [] options dataDir
      let fooM :| [barM, quuxM] = ogMigrations
      fooM  `shouldBe` Db.OutputMigration
        { Db.omVersion = [utcIso8601ms|2020-07-12T06:21:21.00000|]
        , Db.omHash = Binary
          { fromBinary = "L\DLE\137\195\169\&0\163o!I\189\253`\250\203\147\215\200\224\137S\160m{\179\227\240\ESC\194P-I" }
        , Db.omName = "2020-07-12T06-21-21_foo"
        }
      barM  `shouldBe` Db.OutputMigration
        { Db.omVersion = [utcIso8601ms| 2020-07-12T06:21:27.00000 |]
        , Db.omHash = Binary
          { fromBinary = "\ETX\225\155\215\184\144\147\DLEn\SO\195\175\&4\167\208~-\244S\146\&9\215K\223i\173\EOT\209A'Z7" }
        , Db.omName = "2020-07-12T06-21-27_bar"
        }
      quuxM `shouldBe` Db.OutputMigration
        { Db.omVersion = [utcIso8601ms| 2020-07-12T06:21:32.00000 |]
        , Db.omHash = Binary
          { fromBinary = "\DLE*\221\")\SO\204\207\EMdmn\b\197\233a\212-NA\133;\255\167/\t\133\139\163\222Tz" }
        , Db.omName = "2020-07-12T06-21-32_quux"
        }

      withOptions options checkTables `shouldReturn` ["bar", "foo", "quux"]

    it "list of standard migrations succeeds" $ \options -> do
      [OutputGroup (Db.OutputGroup {ogMigrations})] <- list options
      let fooM :| [barM, quuxM] = ogMigrations
      fooM  `shouldBe` Db.OutputMigration
        { Db.omVersion = [utcIso8601ms|2020-07-12T06:21:21.00000|]
        , Db.omHash = Binary
          { fromBinary = "L\DLE\137\195\169\&0\163o!I\189\253`\250\203\147\215\200\224\137S\160m{\179\227\240\ESC\194P-I" }
        , Db.omName = "2020-07-12T06-21-21_foo"
        }
      barM  `shouldBe` Db.OutputMigration
        { Db.omVersion = [utcIso8601ms| 2020-07-12T06:21:27.00000 |]
        , Db.omHash = Binary
          { fromBinary = "\ETX\225\155\215\184\144\147\DLEn\SO\195\175\&4\167\208~-\244S\146\&9\215K\223i\173\EOT\209A'Z7" }
        , Db.omName = "2020-07-12T06-21-27_bar"
        }
      quuxM `shouldBe` Db.OutputMigration
        { Db.omVersion = [utcIso8601ms| 2020-07-12T06:21:32.00000 |]
        , Db.omHash = Binary
          { fromBinary = "\DLE*\221\")\SO\204\207\EMdmn\b\197\233a\212-NA\133;\255\167/\t\133\139\163\222Tz" }
        , Db.omName = "2020-07-12T06-21-32_quux"
        }

    it "reapplying does nothing" $ \options -> do
      (apply [] options . (</> "data") =<< getDataDir) `shouldReturn` []

    it "reapplying with an extra migration applies the migration" $ \options -> do
      let extraMigration = Db.InputMigration
            { Db.inputAction = void $ T.execute_ "CREATE TABLE test.extra();"
            , Db.inputVersion = [utcIso8601ms| 2020-07-12T06:21:33.00000 |]
            , Db.inputHash = Binary "hash"
            , Db.inputName = "extra"
            }
      [OutputGroup (Db.OutputGroup {ogMigrations})] <- apply [(InTransaction, extraMigration)] options . (</> "data") =<< getDataDir
      ogMigrations `shouldBe` Db.OutputMigration
        { Db.omVersion = [utcIso8601ms| 2020-07-12T06:21:33.00000 |]
        , Db.omHash = Binary "hash"
        , Db.omName = "extra"
        } :| []

      withOptions options checkTables `shouldReturn` ["bar", "extra", "foo", "quux"]

    it "setMigrate adds the migrations but doesn't run it " $ \options -> do
      let extraMigration = Db.InputMigration
            { Db.inputAction = void $ T.execute_ "CREATE TABLE test.extra1();"
            , Db.inputVersion = [utcIso8601ms| 2020-07-12T06:27:33.00000 |]
            , Db.inputHash = Binary "hash1"
            , Db.inputName = "extra1"
            }
      [OutputGroup (Db.OutputGroup {ogMigrations})] <- setMigrated [(InTransaction, extraMigration)] options Nothing Nothing . (</> "data")
        =<< getDataDir
      ogMigrations `shouldBe` Db.OutputMigration
        { Db.omVersion = [utcIso8601ms| 2020-07-12T06:27:33.00000 |]
        , Db.omHash = Binary "hash1"
        , Db.omName = "extra1"
        } :| []

      withOptions options checkTables `shouldReturn` ["bar", "extra", "foo", "quux"]

    it "setMigrate doesn't add the migration if it is out of bounds by start" $ \options -> do
      let extraMigration = Db.InputMigration
            { Db.inputAction = void $ T.execute_ "CREATE TABLE test.extra1();"
            , Db.inputVersion = [utcIso8601ms| 2020-07-12T06:27:33.00000 |]
            , Db.inputHash = Binary "hash1"
            , Db.inputName = "extra1"
            }

      (setMigrated [(InTransaction, extraMigration)] options (pure [utcIso8601ms| 2020-07-12T06:27:34.00000 |]) Nothing . (</> "data")
          =<< getDataDir) `shouldReturn` []

      withOptions options checkTables `shouldReturn` ["bar", "extra", "foo", "quux"]

    it "setMigrate doesn't add the migration if it is out of bounds by end" $ \options -> do
      let extraMigration = Db.InputMigration
            { Db.inputAction = void $ T.execute_ "CREATE TABLE test.extra1();"
            , Db.inputVersion = [utcIso8601ms| 2020-07-12T06:27:33.00000 |]
            , Db.inputHash = Binary "hash1"
            , Db.inputName = "extra1"
            }

      (setMigrated [(InTransaction, extraMigration)] options Nothing (pure [utcIso8601ms| 2020-07-12T06:27:32.00000 |]) . (</> "data")
          =<< getDataDir) `shouldReturn` []

      withOptions options checkTables `shouldReturn` ["bar", "extra", "foo", "quux"]

    it "reapplying a list of mixed transaction migrations" $ \options -> do
      let
        transactionQuery = do
          tid :: Only Int <- head <$> T.query_ [Psql.sql| select txid_current(); |]
          void $ T.execute
            [Psql.sql| insert into test.in_transaction
              (trans)
              (select txid_status(?));
            |]
            tid
        extraMigrations =
            [ ( InTransaction
              , Db.InputMigration {
                  Db.inputAction = void $ T.execute_ "create table test.in_transaction (id serial primary key, trans text);"
                , Db.inputVersion = [utcIso8601ms| 2020-11-16T00:00:00.0000 |]
                , Db.inputHash = Binary "hash_1"
                , Db.inputName = "hash_1"
                }
              )
            , ( InTransaction
              , Db.InputMigration {
                  Db.inputAction = transactionQuery
                , Db.inputVersion = [utcIso8601ms| 2020-11-16T00:00:10.0000 |]
                , Db.inputHash = Binary "hash_2"
                , Db.inputName = "hash_2"
                }
              )
            , ( InTransaction
              , Db.InputMigration {
                  Db.inputAction = transactionQuery
                , Db.inputVersion = [utcIso8601ms| 2020-11-16T00:00:20.0000 |]
                , Db.inputHash = Binary "hash_3"
                , Db.inputName = "hash_3"
                }
              )
            , ( NoTransaction
              , Db.InputMigration {
                  Db.inputAction = transactionQuery
                , Db.inputVersion = [utcIso8601ms| 2020-11-16T00:00:30.0000 |]
                , Db.inputHash = Binary "hash_4"
                , Db.inputName = "hash_4"
                }
              )
            , ( NoTransaction
              , Db.InputMigration {
                  Db.inputAction = transactionQuery
                , Db.inputVersion = [utcIso8601ms| 2020-11-16T00:00:40.0000 |]
                , Db.inputHash = Binary "hash_5"
                , Db.inputName = "hash_5"
                }
              )
            , ( InTransaction
              , Db.InputMigration {
                  Db.inputAction = transactionQuery
                , Db.inputVersion = [utcIso8601ms| 2020-11-16T00:00:50.0000 |]
                , Db.inputHash = Binary "hash_6"
                , Db.inputName = "hash_6"
                }
              )
            , ( InTransaction
              , Db.InputMigration {
                  Db.inputAction = transactionQuery
                , Db.inputVersion = [utcIso8601ms| 2020-11-16T00:00:60.0000 |]
                , Db.inputHash = Binary "hash_7"
                , Db.inputName = "hash_7"
                }
              )
            ]
        checkTransactions conn = fmap Psql.fromOnly <$> Psql.query_ conn
          [Psql.sql|
            SELECT trans
            from test.in_transaction
            order by id;
          |]

      void $ apply extraMigrations options . (</> "data") =<< getDataDir
      withOptions options checkTransactions `shouldReturn`
        (fmap ((\case
                  NoTransaction -> "committed"
                  InTransaction -> "in progress"
              ) :: InTransaction -> String)
              $ fmap fst $ drop 1 extraMigrations
        )
