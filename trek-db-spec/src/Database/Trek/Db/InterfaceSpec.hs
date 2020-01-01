module Database.Trek.Db.InterfaceSpec where
import Test.Hspec.Expectations.Lifted (shouldReturn)
import Test.Hspec hiding (shouldReturn)
import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty(..), fromList, cons)
import Data.Foldable
import Database.Trek.Db.Interface
import Database.Trek.Db.TestInterface
import Database.Trek.Db.TestInterface.Types
import Data.Maybe
import Control.Monad (join)
import Control.Concurrent
import Control.Concurrent.Async
import Data.IORef

{-

There is another interface
The public interface which implements
Everything with functions that return IO (ExitCode, String, String)

and take in [String]

If I have this impl and a bunch of parsers I can make the DB interface

all these functions are implemented by a helper that catches all exceptions and starts by reading all of
I'm going to test at the function level and the executable level

new idea
start with a typed interface and command line parsers for each command.
The parsers are tested individually in unit tests and the app tests them together.

With the parsers/pretty printer/to (ExitCode, String, String) we can make the less typed interface

There is a default app that uses the interface
It creates the parser and then does a dispatch
it pretty prints the result

The tests need the output parser
The interface does not

TODO need to test that setup >> setup >> migrate works!

Here is the question is there a way to make the parsers dynmanic
use dynamic
the parser makes
type CommandArgs = Parser (First (Key, Dynamic))

commandArgs = plugins

type Dispatch = Map Key (Dynamic -> IO ())

dispatch

for now it can just hardcode the functions

that easy

still a win

the executable is indeterminate package with a main. It is used with another impl to make a exe
that interface is type
this exe merely combines the parser and does the dispatch and pretty printing.
well that is not the whole story
there is are tests of just the parsing running and pretty printing of the commands.

These are tests against an untyped interface. And both the exe and the individuals functions are tested ... thus justifying the
second less typed interface .. nope

This is really just an impl of going from the test stuff (parser, func, pretty) to the less typed interface
This is just a helper module. The tests are tests specification, e.g. that it preserves the migration interface
It also tests that all the point tests work so the formatting is one point

the interface I have identified is one that adds parsers and pretty printers
and for testing I need the output parser

additionally I do need an interface that produces just (ExitCode, String, String) and takes in [String]
this is because I have to wrap the exe's and produce the wrapped functions with the parser etc for testing.
the reason the parsers things is an interface is because it is the interface used by the main executable.





the migrate filePath -> dispatches based type of file
if it is a directory it tries to load each file
  -- it dispatches of the file type
if it is a file it loads it as a newline manifest
if it is a sql file it runs it and using the name to determine the migration
if it is a sh it runs it

Crazy idea. I can test the migrate filePath implementations with the same test interface

Crazy idea more the exe can shelf test the extension scripts

The really important discovery is reusing the same interface is useful

I might want the action interface to be `ReaderT DbConfig IO a` so I can predicably read files

The thing that is confusing me is the interface for the actual migration

The main thing I am trying to figure out is

what is the external interface

I need to work back from the cmd line

.



> trek setup

> trek setup
exitcode: 8
stderr: Setup Already! >:(

> trek migrate FILEPATH
exitcode: 16
stderr: Not Setup! Execute `trek setup` to setup.
exitcode: 32
stderr: The following versions have hash conflicts [VERSION]
> trek migrate FILEPATH --warn-hash-conflicts
stderr: The following versions have hash conflicts [VERSION]
{ name           : foo
, version        : 12/12/1980
, hash           : xofdshagnosfdasngs
, rollback       : fdsqfg12
, application_id : 1
, created_at     : 12/12/2020
}
> trek migrate FILEPATH --no-warn-hash-conflicts
{ name           : foo
, version        : 12/12/1980
, hash           : xofdshagnosfdasngs
, rollback       : fdsqfg12
, application_id : 1
, created_at     : 12/12/2020
}
> trek migrate FILEPATH
{ rollback   : fdsqfg12
, id         : 1
, migrations :
  [ { name           : foo
    , version        : 12/12/1980
    , hash           : xofdshagnosfdasngs
    , application_id : 1
    , created_at     : 12/12/2020
    }
  , { name           : foo
    , version        : 12/12/1981
    , hash           : xofdshagnosfdasngs
    , application_id : 1
    , created_at     : 12/12/2021
    }
  ]
}
> trek migrate FILEPATH
exitcode: 64
stderr: Nothing to migrate!
> trek migrate FILEPATH --warn-empty-migration
stderr: Nothing to migrate!
> trek migrate FILEPATH --no-warn-empty-migration

trek hash-conflicts FILEPATH
exitcode: 16
stderr: Not Setup! Execute `trek setup` to setup.

trek add-hashes [VERSION]
exitcode: 16
stderr: Not Setup! Execute `trek setup` to setup.

trek remove-hashes [VERSION]
exitcode: 16
stderr: Not Setup! Execute `trek setup` to setup.

trek teardown
exitcode: 16

In the case that we don't care about hashes

trek migrate FILEPATH
trek list
trek teardown

the idea is that I can make a Impl out of this for the spec.
Also I can test that somethings are exactly as I expect them.

-- On failure it outputs the state to rollback to
trek migrate --error-on-conflict FILEPATH
trek list
trek teardown

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
