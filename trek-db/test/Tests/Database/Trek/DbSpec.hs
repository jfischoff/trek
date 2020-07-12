module Tests.Database.Trek.DbSpec (spec) where
import Database.Trek.Db
import Data.Time.QQ
import qualified Database.Postgres.Temp as Temp
import qualified Database.PostgreSQL.Transact as T
import qualified Database.PostgreSQL.Simple as Psql
import Database.PostgreSQL.Simple.SqlQQ
import Control.Exception
import Control.Monad (void)
import Data.Pool
import qualified Data.List.NonEmpty as NonEmpty
import Test.Hspec hiding (shouldReturn, shouldBe)
import Data.Foldable
import Control.Monad ((<=<))
import Test.Hspec.Expectations.Lifted (shouldReturn, shouldBe)
import Data.List.NonEmpty (NonEmpty(..), fromList)
import Control.Concurrent
import Control.Concurrent.Async
import Data.IORef

type WorldState = String

-------------------------------------------------------------------------------
-- Functions that should live somewhere else
-------------------------------------------------------------------------------
-- This is sequential subsets!
{-
nonEmptySubsetsOf :: NonEmpty a -> NonEmpty (NonEmpty a)
nonEmptySubsetsOf = \case
  xs@(_ :| []) -> xs :| []
  x :| y:ys ->
    let subs = nonEmptySubsetsOf (y :| ys)
    in fmap (cons x) subs <> subs
-}

nonEmptyPartitionsOf :: NonEmpty a -> NonEmpty (NonEmpty (NonEmpty a))
nonEmptyPartitionsOf = fromList . fmap fromList . fmap (fmap fromList) . partitions . toList

partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x:xs) = [[x]:p | p <- partitions xs]
                  ++ [(x:ys):yss | (ys:yss) <- partitions xs]
-------------------------------------------------------------------------------
-- Hspec helper
-------------------------------------------------------------------------------
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

clear :: DB ()
clear = void $ T.execute_ [sql|
  DROP SCHEMA IF EXISTS meta CASCADE;
  DROP SCHEMA IF EXISTS test CASCADE;
 |]

worldState :: DB WorldState
worldState = do
  xs :: [String] <- fmap Psql.fromOnly <$> T.query_
    "SELECT CAST(table_name AS varchar) FROM information_schema.tables where table_schema = 'test' ORDER BY table_name"
  pure $ concat xs

createFoo :: DB ()
createFoo = void $ T.execute_ [sql| CREATE SCHEMA IF NOT EXISTS test; CREATE TABLE test.foo (id SERIAL PRIMARY KEY)|]

createBar :: DB ()
createBar = void $ T.execute_ [sql| CREATE SCHEMA IF NOT EXISTS test; CREATE TABLE test.bar (id SERIAL PRIMARY KEY)|]

createQuux :: DB ()
createQuux = void $ T.execute_ [sql| CREATE SCHEMA IF NOT EXISTS test; CREATE TABLE test.quux (id SERIAL PRIMARY KEY)|]

foo :: InputMigration
foo = InputMigration createFoo [utcIso8601| 2048-12-01 |] (Psql.Binary "extra")

bar :: InputMigration
bar = InputMigration createBar  [utcIso8601| 2025-12-01 |] (Psql.Binary "migration-2025-12-01")

quux :: InputMigration
quux = InputMigration createQuux [utcIso8601| 2025-12-02 |] (Psql.Binary "migration-2025-01-01")

toOutputMigration :: InputMigration -> OutputMigration
toOutputMigration InputMigration {..} = OutputMigration
  { omVersion = inputVersion
  , omHash    = inputHash
  }

toOutput :: InputGroup -> DB OutputGroup
toOutput InputGroup {..} = pure OutputGroup
  { ogId = GroupId $ Psql.Binary $ makeGroupHash $ NonEmpty.toList inputGroupMigrations
  , ogCreatedAt = inputGroupCreateAd
  , ogMigrations = fmap toOutputMigration inputGroupMigrations
  }

rollback :: DB a -> DB a
rollback = T.rollback
--
withSetup :: (Pool Psql.Connection -> IO a) -> IO a
withSetup f = do
  -- Helper to throw exceptions
  let throwE x = either throwIO pure =<< x

  throwE $ Temp.withDbCache $ \dbCache -> do
    let combinedConfig = Temp.defaultConfig <> Temp.cacheConfig dbCache
    Temp.withConfig combinedConfig $ \db ->
      f =<< createPool (Psql.connectPostgreSQL $ Temp.toConnectionString db) Psql.close 2 60 10

withPool :: DB a -> Pool Psql.Connection -> IO a
withPool action pool = withResource pool $ \conn -> T.runDBTSerializable action conn

spec :: Spec
spec = do
  let rollbackIt msg action = it msg $ \pool -> flip withPool pool $ rollback action

  aroundAll withSetup $ describe "Tests.Database.Trek.Db.Interface" $ do
    rollbackIt "input migration on clean setup give output group" $ do
      initial <- inputGroup $ pure foo
      expected <- toOutput initial
      apply initial `shouldReturn` Just expected

    it "for s ⊆ x. apply x >> apply s = Nothing" $ withPool $ rollback $ do
      twoMigrations <- inputGroup (foo NonEmpty.:| [bar])
      expectedTwoOutput <- toOutput twoMigrations
      apply twoMigrations `shouldReturn` Just expectedTwoOutput

      rollback $ (apply =<< inputGroup (pure foo)) `shouldReturn` Nothing
      rollback $ (apply =<< inputGroup (pure bar)) `shouldReturn` Nothing

    rollbackIt "for s ⊆ x and y st. z = y / x and z ≠ ∅. apply s >> apply y = Just z" $ do
      twoMigrations <- inputGroup (foo NonEmpty.:| [bar])
      expectedTwoOutput <- toOutput twoMigrations
      apply twoMigrations `shouldReturn` Just expectedTwoOutput

      someAlreadyApplied <- inputGroup (foo NonEmpty.:| [quux])
      onlyQuux <- toOutput =<< inputGroup (quux NonEmpty.:| [])

      apply someAlreadyApplied `shouldReturn` Just onlyQuux

        -- TODO make exception safe
    it "actions are preserved during migration : all partitions apply the same effects" $ \pool -> do
      let migrations = quux NonEmpty.:| [foo, bar]
      forM_ (nonEmptyPartitionsOf migrations) $ \parts -> do
        _ <- flip withPool pool clear

        expectedWorldState <- mapM_ (flip withPool pool . sequenceA_ . fmap inputAction) parts
          >> flip withPool pool worldState

        _ <- flip withPool pool clear

        actual <- flip withPool pool
          (mapM_ (apply <=< inputGroup) parts >> worldState)

        actual `shouldBe` expectedWorldState
