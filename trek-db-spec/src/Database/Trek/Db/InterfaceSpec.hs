module Database.Trek.Db.InterfaceSpec where
import Test.Hspec (Spec, it, describe, SpecWith, beforeAll, afterAll, Arg, Example)
import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Database.PostgreSQL.Transact
import Test.Hspec.Expectations.MonadThrow (shouldReturn)
import qualified Database.PostgreSQL.Simple as PS
import Data.String.Here (i)
import qualified Database.Postgres.Temp as Temp
import Control.Exception
import Data.Monoid
import qualified Database.PostgreSQL.Simple.Options as POptions
import Data.Foldable
import Database.Trek.Db.TestInterface
import Data.Bifunctor

-------------------------------------------------------------------------------
-- General db setup that I copy around
-------------------------------------------------------------------------------
createTempConnection :: IO (PS.Connection, Temp.DB)
createTempConnection = do
  db <- either throwIO pure =<< Temp.start Temp.defaultOptions
  let connString = POptions.toConnectionString $ Temp.options db
  connection <- PS.connectPostgreSQL connString
  return (connection, db)

-- Either run the job or not
setupDB :: IO (PS.Connection, Temp.DB)
setupDB = do
  (connection, db) <- createTempConnection
  let url = POptions.toConnectionString $ Temp.options db
  print url
  return (connection, db)

withTestDB :: SpecWith (PS.Connection, Temp.DB) -> Spec
withTestDB = beforeAll setupDB . afterAll stopDB

stopDB :: (PS.Connection, Temp.DB) -> IO ()
stopDB (c, x) = void $PS.close c >> Temp.stop x

-- this should rollback
withDB :: DB a -> (PS.Connection, Temp.DB) -> IO a
withDB x (c, _) = runDBTSerializable x c
-------------------------------------------------------------------------------
-- Verification Helpers
-------------------------------------------------------------------------------
verifyTableExists :: String -> DB Bool
verifyTableExists tableName = PS.fromOnly . head <$> query_ [i|
  SELECT EXISTS (
  SELECT 1
  FROM   information_schema.tables
  WHERE  table_schema = 'public'
  AND    table_name = '${tableName}'
  );
  |]

-------------------------------------------------------------------------------
-- Functions that should live somewhere else
-------------------------------------------------------------------------------
subsetsOf :: [a] -> [[a]]
subsetsOf = \case
  [] -> []
  x:xs -> map (x:) (subsets xs) ++ subset xs

unitRight :: Functor f => f (Either a b) -> f (Either a b)
unitRight = fmap (first (const ()))

-------------------------------------------------------------------------------
-- Test data
-------------------------------------------------------------------------------
extraMigrationsAndValidations :: NonEmpty (Migration, DB Bool)
extraMigrationsAndValidations = error "extraMigrations"

extraMigrations :: NonEmpty Migration
extraMigrations = fmap fst extraMigrationsAndValidations

extraValidations :: NonEmpty (DB Bool)
extraValidations = fmap snd extraMigrationsAndValidations

migrationsAndValidations :: NonEmpty (Migration, DBT IO Bool)
migrationsAndValidations = error "migrations"

migrations :: NonEmpty (Migration, DBT IO Bool)
migrations = map fst migrationsAndValidations

-------------------------------------------------------------------------------
-- Schema clearing
-------------------------------------------------------------------------------
-- Drop the 'test' schema to reset
clearSchema :: DB ()
clearSchema = void $ execute_ [i|
    DROP SCHEMA IF EXISTS test CASCADE;
    CREATE SCHEMA test;
  |]

withClearSchema :: DB a -> (PS.Connection, Temp.DB) -> IO a
withClearSchema action (conn, db) =  (conn, db)

-- A replacement for 'it' that also cleans the schema
clearIt
  :: Example ((PS.Connection, Temp.DB) -> IO a)
  => String
  -> DB a
  -> SpecWith
     (Arg ((PS.Connection, Temp.DB) -> IO a))
clearIt msg = it msg . withClearSchema

-------------------------------------------------------------------------------
-- Pre-call setup on the db
-------------------------------------------------------------------------------
withSetup :: DB a -> (PS.Connection, Temp.DB) -> IO a
withSetup action = withClearSchema (setup >> action)

setupIt
  :: Example ((PS.Connection, Temp.DB) -> IO a)
  => String
  -> DB a
  -> SpecWith (Arg ((PS.Connection, Temp.DB) -> IO a))
setupIt msg = it msg . withSetup

-------------------------------------------------------------------------------
-- Setup the migrations
-------------------------------------------------------------------------------
applyAllMigrations :: DBT IO ApplicationGroup
applyAllMigrations = do
  let (theMigrations, theValidators) = NonEmpty.unzip migrations
      input = applicationGroup theMigrations
  unitLeft (migrate input) `shouldReturn` Right (Just $ hashApplicationGroup input)

  mconcat (toList $ fmap (fmap All) theValidators) `shouldReturn` All True

  pure input

migrateIt
  :: Example ((PS.Connection, Temp.DB) -> IO a)
  => String
  -> (ApplicationGroup -> DB a)
  -> SpecWith (Arg ((PS.Connection, Temp.DB) -> IO a))
migrateIt msg action = setupIt msg $
  applyAllMigrations >>= action

-------------------------------------------------------------------------------
--
--                        'setup' and 'teardown' specs
--
-- The setup and teardown functions have the following rules
-- With a clear schema the following is true
--  setup                -> Right ()
--  teardown             -> Left  ()
--
--  One can think of Right as Success and Left as Failure
--
--  where a is either setup, teardown or something that doesn't affect the
--  the migration tables
--
--  both setup and teardown have fixpoints after two application
--  a >> setup    >> setup    -> Left  ()
--  a >> teardown >> teardown -> Left  ()
--
--  Both setup and teardown succeed if they fall each other
--  a >> setup    >> teardown -> Right ()
--  a >> teardown >> setup    -> Right ()
--
-- Additionally all functions but 'setup', require that 'setup' is called first
-- to not get a 'Left'
-------------------------------------------------------------------------------

-- | The interface includes error types but the spec
--   does not examine them so set them to '()' with unitRight and
--   unitLeft
setupTeardownSpecs
  :: SpecWith (PS.Connection, Temp.DB)
setupTeardownSpecs = describe "setup teardown" $ do
  describe "On a clear schema" $ do
    clearIt "setup succeeds" $ setup    `shouldReturn` Right ()
    clearIt "teardown fails" $ teardown `shouldReturn` Left  ()

  let beforeActions =
        [ ("nothing" , pure ()      )
        , ("setup"   , void setup   )
        , ("teardown", void teardown)
        ]

  forM_ beforeActions $ \(name, action) -> describe ("before " ++ name) $ do

    describe "fixpoints" $ do
      clearIt "setup" $
        action >> setup >> setup       `shouldReturn` Left  ()
      clearIt "teardown" $
        action >> teardown >> teardown `shouldReturn` Left  ()

    describe "alternation" $ do
      clearIt "teardown >> setup" $
        action >> teardown >> setup `shouldReturn` Right ()
      clearIt "setup >> teardown" $
        action >> setup >> teardown `shouldReturn` Right ()

-------------------------------------------------------------------------------
--
--                        setup is required specs
--
-- teardown, migrate, listMigrations, hashConflicts all require that setup
-- is called first
--
-------------------------------------------------------------------------------
requireSetupSpecs :: SpecWith (PS.Connection, Temp.DB)
requireSetupSpecs = do
  let  = fmap fst migrations
  describe "Clean schema gives NoSetup for" $ do
    clearIt "migrate" $
      unitRight (migrate $ applicationGroup justMigrations) `shouldReturn` Left ()
    clearIt "listMigrations" $
      unitRight listMigrations `shouldReturn` Left ()
    clearIt "hashConflicts with nonempty migrations" $
      hashConflicts (toList justMigrations) `shouldReturn` Left ()

-------------------------------------------------------------------------------
--
--                        migrate specs
--
-- All assume a clear schema
--
-- migrate x >> listMigrations = [x]
-- migrate x >> for s ⊆ x. migrate s >> listMigrations = [x]
-- migrate x >> for s ⊆ x and y st. z = y / x and z ≠ ∅.
--   migrate (s ∪ y) >> listMigrations = [x, y]
--
-- in addition
-- for all partitions of [[[InputMigration]]] of an InputGroup
-- apply all the sub groups in the partition through `migrate` is equivalent to any
-- apply the actions directly

-- equivalent in the sense that one must provide a way represent the state of the world
-- DB a -> WorldState and WorldStates can be compared.
-- We are testing the side effects of DB actions.
--
-- We need to test the idepotency property
-- manually that any migration that is run twice does affect the world.
-- regardless of what other migrations are run
-- in between
--
-- what else?
-- if migrate with a nothing does not modify the world
-------------------------------------------------------------------------------
migrateListMigrationSpecs :: SpecWith (PS.Connection, Temp.DB)
migrateListMigrationSpecs = describe "migration and listMigrations" $ do
  describe "listMigrations" $ setupIt "gives []" $
    listMigrations `shouldReturn` Right []

  let listMigrationsAssert expected =
        fmap (fmap (fmap clearCreatedAt)) listMigrations
          `shouldReturn` Right [clearCreatedAt $ hashApplicationGroup expected]

  migrateIt "a single migration on a clean setup succeeds" $ listMigrationsAssert

  let (theMigrations, _) = unzip $ toList migrations

      verifyUnionWithSubset :: String -> [(Migration, DB Bool)] -> SpecWith (PS.Connection, Temp.DB)
      verifyUnionWithSubset msg newList = migrateIt msg $ \appliedMigration -> do
        case NonEmpty.nonEmpty newList of
          Nothing -> forM_ (subsetsOf theMigrations) $ \subset -> for_ (NonEmpty.nonEmpty subset) $ \neSubset -> rollback $ do
            migrate (applicationGroup neSubset) `shouldReturn` Right Nothing
            listMigrationsAssert appliedMigration
          Just new -> forM_ (subsetsOf theMigrations) $ \subset -> for_ (NonEmpty.nonEmpty subset) $ \neSubset -> rollback $ do
            let (theNewMigrations, theNewValidators) = NonEmpty.unzip new
            -- Should fail for now
            migrate (applicationGroup $ neSubset <> theNewMigrations) `shouldReturn` Right Nothing

            mconcat (toList $ fmap (fmap All) theNewValidators) `shouldReturn` All True

            -- Should fail
            listMigrationsAssert appliedMigration

  verifyUnionWithSubset "subsets of the already applied migrations return Nothing and listMigrations returns the same as before" []

  verifyUnionWithSubset "subset with union of a new versions return the original plus the new versions" (toList extraMigrations)
-------------------------------------------------------------------------------
--
--                        hashConflict spec
--
-- hashConflicts [] = Right []
-- On a clean schema
-- hashConflicts x = Right []
-- migrate x >> st. y and x are disjoint. hashConflicts (x ∪ y) = Right x
-------------------------------------------------------------------------------
hashConflictSpecs
  :: SpecWith (PS.Connection, Temp.DB)
hashConflictSpecs = do
  setupIt "hashConflicts [] = Right []" $
    hashConflict [] `shouldReturn` Right []
  setupIt "On a clear schema hashConflicts x = Right []" $
    hashConflict migrations `shouldReturn` Right []
  migrateIt "migrate x >> st. y and x are disjoint. hashConflicts (x ∪ y) = Right x" $ \_ -> do
    hashConflicts (toList $ migrations <> extraMigrations)
      `shouldReturn` Right (fmap migrationVersion migrations)

specs :: Spec
specs = withTestDB $ describe "Tests.Database.Trek.Db.Interface" $ do
  setupTeardownSpecs
  requireSetupSpecs
  migrateListMigrationSpecs
  hashConflictSpecs
