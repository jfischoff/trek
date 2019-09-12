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
import Database.Trek.Db.Interface
import Data.Bifunctor

verifyTableExists :: String -> DB Bool
verifyTableExists tableName = PS.fromOnly . head <$> query_ [i|
  SELECT EXISTS (
  SELECT 1
  FROM   information_schema.tables
  WHERE  table_schema = 'public'
  AND    table_name = '${tableName}'
  );
  |]

createTempConnection :: IO (PS.Connection, Temp.DB)
createTempConnection = do
  db <- either throwIO pure =<< Temp.start Temp.defaultOptions
          { Temp.tmpCmdLineOptions =
              [ ("archive_mode", "on")
              , ("wal_level", "replica")
              , ("max_wal_senders", "2")
              ]
          }

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

clearSchema :: DB ()
clearSchema = void $ execute_ [i|
    DROP SCHEMA IF EXISTS test CASCADE;
    CREATE SCHEMA test;
  |]

drop3 :: (a, b, c) -> (a, b)
drop3 = undefined

withClearSchema :: DB a -> (PS.Connection, Temp.DB) -> IO a
withClearSchema action (conn, db) = withDB (clearSchema >> action) (conn, db)

migrations :: NonEmpty (Migration, DBT IO Bool)
migrations = error "migrations"

{-
all with a clean schema

setup                 -> DidNotHaveSetup
setup    >> setup     -> HadSetup

teardown             -> DidNotHaveSetup
teardown >> teardown -> DidNotHaveSetup

teardown >> setup    -> DidNotHaveSetup
setup    >> teardown -> HadSetup

-}
mapSpec :: (a -> b) -> SpecWith a -> SpecWith b
mapSpec = error "mapSpec"

onAClearSchema
  :: Example ((PS.Connection, Temp.DB) -> IO a)
  => String
  -> DB a
  -> SpecWith
     (Arg ((PS.Connection, Temp.DB) -> IO a))
onAClearSchema msg = it msg . withClearSchema


withSetup :: DB a -> (PS.Connection, Temp.DB) -> IO a
withSetup action = withClearSchema (setup >> action)

itWithSetup
  :: Example ((PS.Connection, Temp.DB) -> IO a)
  => String
  -> DB a
  -> SpecWith (Arg ((PS.Connection, Temp.DB) -> IO a))
itWithSetup msg = it msg . withSetup

subsetsOf :: [a] -> [[a]]
subsetsOf = error "forSubsetsOf"

unitLeft :: Functor f => f (Either a b) -> f (Either () b)
unitLeft = fmap (first (const ()))

applyAllMigrations :: DBT IO ApplicationGroup
applyAllMigrations = do
  let (theMigrations, theValidators) = NonEmpty.unzip migrations
      input = applicationGroup theMigrations
  unitLeft (migrate input) `shouldReturn` Right (Just $ hashApplicationGroup input)

  mconcat (toList $ fmap (fmap All) theValidators) `shouldReturn` All True

  pure input

itWithAllMigrations
  :: Example ((PS.Connection, Temp.DB) -> IO a)
  => String
  -> (ApplicationGroup -> DB a)
  -> SpecWith (Arg ((PS.Connection, Temp.DB) -> IO a))
itWithAllMigrations msg action = itWithSetup msg $
  applyAllMigrations >>= action

unitEither :: Functor f => f (Either a b) -> f (Either () ())
unitEither = fmap (bimap (const ()) (const ()))

setupTeardownSpecs
  :: SpecWith (PS.Connection, Temp.DB)
setupTeardownSpecs = describe "setup teardown" $ do
  onAClearSchema "setup succeeds on a clean schema" $
    unitEither setup `shouldReturn` Right ()
  onAClearSchema "teardown then setup succeeds" $
    unitEither (teardown >> setup) `shouldReturn` Right ()
  onAClearSchema "setup then teardown gives" $
    unitEither (setup >> teardown) `shouldReturn` Right ()
  onAClearSchema "teardown fails on clean schema" $
    unitEither teardown `shouldReturn` Left ()

  let beforeActions =
        [ ("nothing" , pure () )
        , ("setup"   , void $ setup   )
        , ("teardown", void $ teardown)
        ]

  forM_ beforeActions $ \(actionName, beforeAction) -> describe ("before " ++ actionName) $ do
    onAClearSchema "setup projection" $
      unitEither (beforeAction >> setup >> setup) `shouldReturn` Left ()
    onAClearSchema "teardown projection" $
      unitEither (beforeAction >> teardown >> teardown) `shouldReturn` Left ()

{-
assuming: cleanSchema >> setup >> runExcept

>> wraps in ExceptT

listMigrations = pure []

migrate (a, xs) >> listMigrations = pure [(a, xs)]
migrate xs >> forall subset xs. migrate subset >> Except listMigrations = pure xs
migrate xs >> forall subset xs. migrate (subset `union` y) >> listMigrations = pure (xs `union` y)
-}

extraMigrations :: NonEmpty (Migration, DB Bool)
extraMigrations = error "extraMigrations"

migrateSpecs
  :: SpecWith (PS.Connection, Temp.DB)
migrateSpecs = describe "migration" $ do
  describe "Clean schema gives NoSetup" $ do
    onAClearSchema "for migrate" $
      unitEither (migrate $ applicationGroup $ fmap fst migrations) `shouldReturn` Left ()
    onAClearSchema "for listMigrations" $
      unitEither listMigrations `shouldReturn` Left ()
    onAClearSchema "for hashConflicts with nonempty migrations" $
      unitLeft (hashConflicts (toList $ fmap fst migrations)) `shouldReturn` Left ()

  describe "listMigrations" $ itWithSetup "gives []" $
    unitLeft listMigrations `shouldReturn` Right []

  let listMigrationsAssert expected =
        unitLeft (fmap (fmap (fmap clearCreatedAt)) listMigrations)
          `shouldReturn` Right [clearCreatedAt $ hashApplicationGroup expected]

  itWithAllMigrations "a single migration on a clean setup succeeds" $ listMigrationsAssert

  let (theMigrations, _) = unzip $ toList migrations

      verifyUnionWithSubset :: String -> [(Migration, DB Bool)] -> SpecWith (PS.Connection, Temp.DB)
      verifyUnionWithSubset msg newList = itWithAllMigrations msg $ \appliedMigration -> do
        case NonEmpty.nonEmpty newList of
          Nothing -> forM_ (subsetsOf theMigrations) $ \subset -> for_ (NonEmpty.nonEmpty subset) $ \neSubset -> rollback $ do
            unitLeft (migrate (applicationGroup neSubset)) `shouldReturn` Right Nothing
            listMigrationsAssert appliedMigration
          Just new -> forM_ (subsetsOf theMigrations) $ \subset -> for_ (NonEmpty.nonEmpty subset) $ \neSubset -> rollback $ do
            let (theNewMigrations, theNewValidators) = NonEmpty.unzip new
            -- Should fail for now
            unitLeft (migrate (applicationGroup $ neSubset <> theNewMigrations)) `shouldReturn` Right Nothing

            mconcat (toList $ fmap (fmap All) theNewValidators) `shouldReturn` All True

            -- Should fail
            listMigrationsAssert appliedMigration

  verifyUnionWithSubset "subsets of the already applied migrations return Nothing and listMigrations returns the same as before" []

  verifyUnionWithSubset "subset with union of a new versions return the original plus the new versions" (toList extraMigrations)

conflictingMigrations :: [Migration]
conflictingMigrations = error "conflictingMigrations"

hashConflictSpecs
  :: SpecWith (PS.Connection, Temp.DB)
hashConflictSpecs = do
  itWithAllMigrations "no hash conflicts on empty" $ \_ ->
    unitLeft (hashConflicts conflictingMigrations) `shouldReturn` Right []
  itWithAllMigrations "any subset of the conflictingMigrations union with extraMigrations and the conflicts are only the subset" $ \_ -> do
    forM_ (subsetsOf conflictingMigrations) $ \subset -> do
      unitLeft (hashConflicts (subset ++ toList (fmap fst extraMigrations)))
        `shouldReturn` Right (fmap migrationVersion subset)
      unitLeft (hashConflicts subset)
        `shouldReturn` Right (fmap migrationVersion subset)

specs :: Spec
specs = withTestDB $ describe "Tests.Database.Trek.Db.Interface" $ do
  setupTeardownSpecs
  migrateSpecs
  hashConflictSpecs
