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
withTestDB = aroundAll dbRunner
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

spec :: Spec
spec = withTestDB $ describe "Tests.Database.Trek.Db.Interface" $
  applyListMigrationSpecs
