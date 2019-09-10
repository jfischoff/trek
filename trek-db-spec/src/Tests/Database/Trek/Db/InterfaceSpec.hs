module Tests.Database.Trek.Db.InterfaceSpec where
import Database.Trek.Db.Interface
import Database.Trek.Db.Types
import Test.Hspec (Spec, it, describe, SpecWith, beforeAll, afterAll, Arg, Example)
import Control.Monad (void)
import qualified Data.Aeson as Aeson
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Database.PostgreSQL.Transact
import Test.Hspec.Expectations.MonadThrow (shouldBe, shouldReturn)
import qualified Database.PostgreSQL.Simple as PS
import Data.String.Here (i)
import qualified Database.Postgres.Temp as Temp
import Control.Exception
import Data.Monoid
import qualified Database.PostgreSQL.Simple.Options as POptions
import Data.Foldable

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

withClearSchema :: (Interface -> DB a) -> (PS.Connection, Temp.DB, Interface) -> IO a
withClearSchema action (conn, db, interface) = withDB (clearSchema >> action interface) (conn, db)

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
  :: Example ((PS.Connection, Temp.DB, Interface) -> IO a)
  => String
  -> (Interface -> DB a)
  -> SpecWith
     (Arg ((PS.Connection, Temp.DB, Interface) -> IO a))
onAClearSchema msg = it msg . withClearSchema

withSetup :: (Interface -> DB a) -> (PS.Connection, Temp.DB, Interface) -> IO a
withSetup action = withClearSchema (\interface -> iSetup interface >> action interface)

itWithSetup
  :: Example ((PS.Connection, Temp.DB, Interface) -> IO a)
  => String
  -> (Interface -> DB a)
  -> SpecWith (Arg ((PS.Connection, Temp.DB, Interface) -> IO a))
itWithSetup msg = it msg . withSetup

subsetsOf :: [a] -> [[a]]
subsetsOf = error "forSubsetsOf"

applyAllMigrations :: Interface
                            -> NonEmpty (Migration, DBT IO Bool) -> DBT IO ApplicationGroup
applyAllMigrations Interface {..} migrations = do
  let (theMigrations, theValidators) = NonEmpty.unzip migrations

  Right (Just appliedMigrations) <- iMigrate Aeson.Null theMigrations
  appliedMigrations `shouldBe` ApplicationGroup Aeson.Null (fmap toHashedMigration theMigrations)

  mconcat (toList $ fmap (fmap All) theValidators) `shouldReturn` All True

  pure appliedMigrations

initialMigrations :: NonEmpty (Migration, DB Bool)
initialMigrations = error "migrations"

itWithAllMigrations
  :: Example ((PS.Connection, Temp.DB, Interface) -> IO a)
  => String
  -> (Interface -> ApplicationGroup -> DB a)
  -> SpecWith (Arg ((PS.Connection, Temp.DB, Interface) -> IO a))
itWithAllMigrations msg action = itWithSetup msg
  $ \interface -> applyAllMigrations interface initialMigrations >>= action interface

setupTeardownSpecs
  :: Interface
  -> SpecWith (PS.Connection, Temp.DB, Interface)
setupTeardownSpecs Interface {..} = describe "setup teardown" $ do
  let beforeActions =
        [ ("nothing" , pure () )
        , ("setup"   , void $ iSetup   )
        , ("teardown", void $ iTeardown)
        ]
  forM_ beforeActions $ \(actionName, beforeAction) -> describe ("before " ++ actionName) $ do
    onAClearSchema "setup succeeds on a clean schema" $ \_ ->
      iSetup `shouldReturn` DidNotHaveSetup
    onAClearSchema "teardown then setup succeeds" $ \_ ->
      (beforeAction >> iTeardown >> iSetup) `shouldReturn` DidNotHaveSetup
    onAClearSchema "setup projection" $ \_ ->
      (beforeAction >> iSetup >> iSetup) `shouldReturn` HadSetup
    onAClearSchema "setup then teardown gives" $ \_ ->
      (beforeAction >> iSetup >> iTeardown) `shouldReturn` HadSetup
    onAClearSchema "teardown fails on clean schema" $ \_ ->
      iTeardown `shouldReturn` DidNotHaveSetup
    onAClearSchema "teardown projection" $ \_ ->
      (beforeAction >> iTeardown >> iTeardown) `shouldReturn` DidNotHaveSetup

{-
assuming: cleanSchema >> setup >> runExcept

>> wraps in ExceptT

listMigrations = pure []

migrate (a, xs) >> listMigrations = pure [(a, xs)]
migrate xs >> forall subset xs. migrate subset >> Except listMigrations = pure xs
migrate xs >> forall subset xs. migrate (subset `union` y) >> listMigrations = pure (xs `union` y)
-}

clearCreatedAt :: ApplicationGroup -> ApplicationGroup
clearCreatedAt = error "clearCreatedAt"


migrateSpecs :: NonEmpty (Migration, DB Bool) -> NonEmpty (Migration, DB Bool)-> SpecWith (PS.Connection, Temp.DB, Interface)
migrateSpecs migrations extraMigrations = describe "migration" $ do
  let expectNoSetup action = do
        onAClearSchema "for migrate" $ \interface@(Interface {..}) -> do
          void $ action interface
          iMigrate Aeson.Null (fmap fst migrations) `shouldReturn` Left NoSetup
        onAClearSchema "for listMigrations" $ \interface@(Interface {..}) -> do
          void $ action interface
          iListMigrations `shouldReturn` Left NoSetup
        onAClearSchema "for hashConflicts with nonempty migrations" $ \interface@(Interface {..}) -> do
          void $ action interface
          iHashConflicts (toList $ fmap fst migrations) `shouldReturn` Left NoSetup

  describe "Clean schema gives NoSetup" $ expectNoSetup (const $ pure ())
  describe "setup >> teardown gives NoSetup" $ expectNoSetup (\(Interface {..}) -> iSetup >> iTeardown)

  describe "listMigrations" $ itWithSetup "gives []" $ \(Interface {..}) ->
    iListMigrations `shouldReturn` Right []

  itWithAllMigrations "a single migration on a clean setup succeeds" $ \Interface {..} appliedMigration -> do
    fmap (fmap (fmap clearCreatedAt)) iListMigrations `shouldReturn` Right [clearCreatedAt appliedMigration]

  let (theMigrations, _) = unzip $ toList migrations

      verifyUnionWithSubset :: String -> [(Migration, DB Bool)] -> SpecWith (PS.Connection, Temp.DB, Interface)
      verifyUnionWithSubset msg newList = itWithAllMigrations msg $ \Interface {..} appliedMigration -> do
        case NonEmpty.nonEmpty newList of
          Nothing -> forM_ (subsetsOf theMigrations) $ \subset -> for_ (NonEmpty.nonEmpty subset) $ \neSubset -> rollback $ do
            iMigrate Aeson.Null neSubset`shouldReturn` Right Nothing
            iListMigrations `shouldReturn` Right [appliedMigration]
          Just new -> forM_ (subsetsOf theMigrations) $ \subset -> for_ (NonEmpty.nonEmpty subset) $ \neSubset -> rollback $ do
            let (theNewMigrations, theNewValidators) = NonEmpty.unzip new
            -- Should fail for now
            iMigrate Aeson.Null (neSubset <> theNewMigrations) `shouldReturn` Right Nothing

            mconcat (toList $ fmap (fmap All) theNewValidators) `shouldReturn` All True

            -- Should fail
            iListMigrations `shouldReturn` Right [appliedMigration]

  verifyUnionWithSubset "subsets of the already applied migrations return Nothing and listMigrations returns the same as before" []

  verifyUnionWithSubset "subset with union of a new versions return the original plus the new versions" (toList extraMigrations)

hashConflictSpecs
  :: [Migration]
  -> [Migration]
  -> SpecWith (PS.Connection, Temp.DB, Interface)
hashConflictSpecs conflictingMigrations extraMigrations = do
  itWithAllMigrations "no hash conflicts on empty" $ \Interface {..} _ ->
    iHashConflicts conflictingMigrations `shouldReturn` Right []
  itWithAllMigrations "any subset of the conflictingMigrations union with extraMigrations and the conflicts are only the subset" $ \Interface {..} _ ->
    forM_ (subsetsOf conflictingMigrations) $ \subset -> do
      iHashConflicts (subset ++ extraMigrations) `shouldReturn` Right (fmap mVersion subset)
      iHashConflicts subset `shouldReturn` Right (fmap mVersion subset)

specs :: Interface -> Spec
specs interface@Interface {..} = describe "Tests.Database.Trek.Db.Interface" $ do
  -- TODO beforeAll and afterAll with the db and interface
  setupTeardownSpecs interface
{-
  migrateSpecs interface
  hashConflictSpecs iHashConflicts
-}