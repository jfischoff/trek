module Database.Trek.Db.InterfaceSpec where
import Test.Hspec.Expectations.Lifted (shouldReturn)
import Test.Hspec (Arg, Example, Spec, SpecWith, afterAll, beforeAll, describe, it)
import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty(..), fromList, cons)
import Data.Foldable
import Database.Trek.Db.Interface
import Database.Trek.Db.TestInterface
import Database.Trek.Db.TestInterface.Types
import Data.Maybe
import Control.Monad (join)

{-
TOOD I need to make a negative implementation that is somewhat reasonable.

Some ideas
need to make the job runner next

data Job = Job
  { batch :: LastRow -> DB [Row]
  , rowKey :: Row -> LastRow
  , action :: Row -> DB ()
  }

And the coordinator that can take a migration and turn it into a sequence of
migrations and jobs followed by code deployments.

These might all be migrators

The job system is sort of like a migration system that has a status
The job system updates the migration until it is finished.

The code deployment doesn't push something that is already out there

Every thing is idepotent. It is basically a system for ensure idepotency

but they are idepotent in different ways.
The migration system does something or not.
The job is incremental and updates state
The code deployment is based on the output hash.

Not clear how to have a single interface yet ... if at all.

Animation deep learning dancing music visualizer. If the user is better at
reinforcing the spectrum they make the visualization go crazy and the AI
learns

an interesting question is whether the system can handle concurrency. It should
be able to

A table lock sounds reasonable

Some thoughts about the bigger picture. This is not just a migrator.
It is a way to store actions to help achieve idempotency. The fact
that it can do that easily for DB actions is a special case. Because
we can lift into the DB (or perhaps it should be abstracted to a different
monad) we can embed arbitrary IO.

In this way the migrator can orchanstrate the steps to a zero down time
deployment.

-}

type SpecState = SpecStateM DB

-------------------------------------------------------------------------------
-- Functions that should live somewhere else
-------------------------------------------------------------------------------
-- This is sequential subsets!
nonEmptySubsetsOf :: NonEmpty a -> NonEmpty (NonEmpty a)
nonEmptySubsetsOf = \case
  xs@(_ :| []) -> xs :| []
  x :| y:ys ->
    let subs = nonEmptySubsetsOf (y :| ys)
    in fmap (cons x) subs <> subs

nonEmptyPartitionsOf :: NonEmpty a -> NonEmpty (NonEmpty (NonEmpty a))
nonEmptyPartitionsOf = fromList . fmap fromList . fmap (fmap fromList) . partitions . toList

partitions :: [a] -> [[[a]]]
partitions [] = [[]]
partitions (x:xs) = [[x]:p | p <- partitions xs]
                  ++ [(x:ys):yss | (ys:yss) <- partitions xs]
-------------------------------------------------------------------------------
-- Hspec helper
-------------------------------------------------------------------------------
withTestDB :: SpecWith SpecState -> Spec
withTestDB = beforeAll dbRunner . afterAll ssShutdown
-------------------------------------------------------------------------------
-- Schema clearing
-------------------------------------------------------------------------------
withClear :: DB a -> SpecState -> IO a
withClear action SpecState { ssRunner } = ssRunner (clear >> action)

-- A replacement for 'it' that also clears
clearIt
  :: Example (SpecState -> IO a)
  => String
  -> DB a
  -> SpecWith
     (Arg (SpecState -> IO a))
clearIt msg = it msg . withClear
-------------------------------------------------------------------------------
-- Pre-call setup on the db
-------------------------------------------------------------------------------
withSetup :: DB a -> SpecState -> IO a
withSetup action = withClear (setup >> action)

-- A replacement for 'it' that also clears and calls setup
setupIt
  :: Example (SpecState -> IO a)
  => String
  -> DB a
  -> SpecWith (Arg (SpecState -> IO a))
setupIt msg = it msg . withSetup
-------------------------------------------------------------------------------
-- Setup the migrations
-------------------------------------------------------------------------------
applyAllMigrations :: DB OutputGroup
applyAllMigrations = fmap (fromMaybe (error "migrations could not be applied") . join) $
  apply $ inputGroup migrations

-- A replacement for 'it' that applies all the migrations
applyIt
  :: Example (SpecState -> IO a)
  => String
  -> (OutputGroup -> DB a)
  -> SpecWith (Arg (SpecState -> IO a))
applyIt msg action = setupIt msg $ applyAllMigrations >>= action
-------------------------------------------------------------------------------
--
--                        Preconditions specs
--
-- Here we ensure that the test interface is setup correctly.
--
-- We need to ensure that 'migrations' and 'extraMigrations' are disjoint
-- Additionally we need to ensure that 'clear' returns use to the same
-- 'WorldState'
-- Additionally 'rollback' and 'runDB' should be such that the world is not
-- affected.
-- 'runDB' needs to persist the world.

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
setupTeardownSpecs
  :: SpecWith SpecState
setupTeardownSpecs = describe "setup teardown" $ do
  describe "On a clear schema" $ do
    clearIt "setup succeeds" $ setup    `shouldReturn` Just ()
    clearIt "teardown fails" $ teardown `shouldReturn` Nothing

  let beforeActions =
        [ ("nothing" , pure ()      )
        , ("setup"   , void setup   )
        , ("teardown", void teardown)
        ]

  forM_ beforeActions $ \(name, action) -> describe ("before " ++ name) $ do

    describe "fixpoints" $ do
      clearIt "setup" $
        (action >> setup    >> setup)    `shouldReturn` Nothing
      clearIt "teardown" $
        (action >> teardown >> teardown) `shouldReturn` Nothing

    describe "alternation" $ do
      clearIt "teardown >> setup" $
        (action >> teardown >> setup) `shouldReturn` Just ()
      clearIt "setup >> teardown" $
        (action >> setup >> teardown) `shouldReturn` Just ()
-------------------------------------------------------------------------------
--
--                        setup is required specs
--
-- teardown, apply, listApplication, hashConflicts all require that setup
-- is called first
--
-------------------------------------------------------------------------------
requireSetupSpecs :: SpecWith SpecState
requireSetupSpecs = do
  describe "Clean schema gives NoSetup for" $ do
    clearIt "apply" $
      apply (inputGroup migrations) `shouldReturn` Nothing
    clearIt "listApplication" $
      listApplications `shouldReturn` Nothing
    clearIt "hashConflicts with nonempty migrations" $
      hashConflicts (toList migrations) `shouldReturn` Nothing
-------------------------------------------------------------------------------
--
--                        apply specs
--
-- All assume a clear schema
--
-- apply x >> listApplication = [x]
-- apply x >> for s ⊆ x. apply s >> listApplication = [x]
-- apply x >> for s ⊆ x and y st. z = y / x and z ≠ ∅.
--   apply (s ∪ y) >> listApplication = [x, y]
--
-- in addition
-- for all partitions of [[[InputMigration]]] of an InputGroup
-- apply all the sub groups in the partition through `apply` is equivalent to any
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
-- if apply with a nothing does not modify the world
-------------------------------------------------------------------------------
applyListMigrationSpecs :: SpecWith SpecState
applyListMigrationSpecs = describe "migration and listApplication" $ do
  setupIt "listApplication gives []" $ listApplications `shouldReturn` Just []

  describe "apply x >>" $ do
    applyIt "listApplication = [x]" $ \a ->
      listApplications `shouldReturn` Just [a]

    applyIt "for s ⊆ x. apply s >> listApplication = [x]" $ \a ->
      forM_ (nonEmptySubsetsOf migrations) $ \subset -> do
        apply (inputGroup subset) `shouldReturn` Just Nothing
        listApplications `shouldReturn` Just [a]

    applyIt ("for s ⊆ x and y st. z = y / x and z ≠ ∅. apply (s ∪ y)" <>
      ">> listApplication = [x, y]") $ \a ->
        forM_ (nonEmptySubsetsOf extraMigrations) $ \subset -> rollback $ do
          output <- fromMaybe (error "migrations could not be applied") . join
            <$> apply (inputGroup subset)
          listApplications `shouldReturn` Just [a, output]

  describe "actions are preserved during migration" $ it "all partitions run the same" $ \SpecState {ssRunner} ->
    forM_ (nonEmptyPartitionsOf migrations) $ \parts -> do
      void $ ssRunner $ clear >> setup
      expectedWorldState <- mapM_ (ssRunner . sequenceA_ . fmap inputAction) parts >> ssRunner worldState
      void $ ssRunner $ clear >> setup
      ssRunner
        (mapM_ (apply . inputGroup) parts >> worldState) `shouldReturn`
          expectedWorldState
-------------------------------------------------------------------------------
--
--                        hashConflict spec
--
-- hashConflicts [] = Right []
-- On a clean schema
-- hashConflicts x = Right []
-- apply x >> st. y and x are disjoint. hashConflicts (x ∪ y) = Right x
-------------------------------------------------------------------------------
hashConflictSpecs :: SpecWith SpecState
hashConflictSpecs = do
  setupIt "hashConflicts [] = Right []" $
    hashConflicts [] `shouldReturn` Just []
  setupIt "On a clear schema hashConflicts x = Right []" $
    hashConflicts (toList migrations) `shouldReturn` Just []
  applyIt "apply x >> st. y and x are disjoint. hashConflicts (x ∪ y) = Right x" $ \_ -> do
    hashConflicts (toList $ conflictingMigrations)
      `shouldReturn` Just (toList $ fmap inputVersion migrations)

spec :: Spec
spec = withTestDB $ describe "Tests.Database.Trek.Db.Interface" $ do
  setupTeardownSpecs
  requireSetupSpecs
  applyListMigrationSpecs
  hashConflictSpecs
