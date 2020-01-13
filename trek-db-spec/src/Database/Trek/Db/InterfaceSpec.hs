module Database.Trek.Db.InterfaceSpec where
import Test.Hspec.Expectations.Lifted (shouldReturn)
import Test.Hspec hiding (shouldReturn)
-- import Control.Monad (void)
import Data.List.NonEmpty (NonEmpty(..), fromList, cons)
import Data.Foldable
import Database.Trek.Db.Interface
import Database.Trek.Db.TestInterface
import Database.Trek.Db.TestInterface.Types
-- import Data.Maybe
-- import Control.Monad (join)
import Control.Concurrent
import Control.Concurrent.Async
import Data.IORef
import qualified Data.List.NonEmpty as NonEmpty

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

withRunner :: DB a -> SpecState -> IO a
withRunner action SpecState { ssRunner } = ssRunner action

runIt :: Example (SpecState -> IO a)
      => String
      -> DB a
      -> SpecWith (Arg (SpecState -> IO a))
runIt msg = it msg . withRunner

rollbackIt
  :: Example (SpecState -> IO a)
  => String
  -> DB a
  -> SpecWith (Arg (SpecState -> IO a))
rollbackIt msg = it msg . withRunner . rollback
-------------------------------------------------------------------------------
-- Setup the migrations
-------------------------------------------------------------------------------
{-
applyAllMigrations :: DB OutputGroup
applyAllMigrations = fmap (fromMaybe (error "migrations could not be applied") . join) $
  apply $ inputGroup migrations

-- A replacement for 'it' that applies all the migrations
applyIt
  :: Example (SpecState -> IO a)
  => String
  -> (OutputGroup -> DB a)
  -> SpecWith (Arg (SpecState -> IO a))
applyIt msg action = applyAllMigrations >>= action
-}
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
{-
applyMigrationSpecs :: SpecWith SpecState
applyMigrationSpecs = describe "migration and listApplication" $ do

  describe "actions are preserved during migration" $ it "all partitions run the same" $ \SpecState {ssRunner} ->
    forM_ (nonEmptyPartitionsOf migrations) $ \parts -> do
      void $ ssRunner $ clear >> setup
      expectedWorldState <- mapM_ (ssRunner . sequenceA_ . fmap inputAction) parts >> ssRunner worldState
      void $ ssRunner $ clear >> setup
      ssRunner
        (mapM_ (apply . inputGroup) parts >> worldState) `shouldReturn`
          expectedWorldState
-}

spec :: Spec
spec = withTestDB $ describe "Tests.Database.Trek.Db.Interface" $ do
  rollbackIt "input migration on clean setup give output group" $ do
    initial <- inputGroup $ pure foo
    expected <- toOutput initial
    apply initial `shouldReturn` Just expected

  rollbackIt "for s ⊆ x. apply x >> apply s = Nothing" $ do
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
