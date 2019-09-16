module Database.Trek.Db.Reference where
import Database.Trek.Db.TestInterface.Types
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty, fromList, nonEmpty)
import Data.Foldable

data WorldState = WorldState
  { isVersions :: [NonEmpty (Version, Int)]
  , isSetup    :: Bool
  , isCounter  :: Int
  } deriving (Show, Eq, Ord)

initialWorldState :: WorldState
initialWorldState = WorldState [] False 0

type DB = StateT WorldState IO

type Version = Int
data InputMigration = InputMigration
  { inputAction :: DB ()
  , inputVersion :: Version
  , inputHash :: Int
  }

type InputGroup = NonEmpty InputMigration

inputGroup :: NonEmpty InputMigration -> InputGroup
inputGroup = id

type OutputGroup = NonEmpty Version

add :: Int -> DB ()
add = error "add"

extraMigrations :: NonEmpty InputMigration
extraMigrations = pure $ InputMigration (add 6) 6 6

migrations :: NonEmpty InputMigration
migrations = fromList $ map (\x -> InputMigration (add x) x x) $ [1 .. 5]

conflictingMigrations :: NonEmpty InputMigration
conflictingMigrations = fromList $ map (\x -> InputMigration (add x) x $ x + 1) $ [1 .. 5]

worldState :: DB WorldState
worldState = get

clear :: DB ()
clear = put initialWorldState

rollback :: DB a -> DB a
rollback action = do
  oldState <- get
  r <- action
  put oldState
  pure r

dbRunner :: IO (SpecStateM DB)
dbRunner = pure $ SpecState
  { ssRunner   = flip evalStateT initialWorldState
  , ssShutdown = pure ()
  }

setup :: DB (Maybe ())
setup = gets isSetup >>= \case
  True -> pure Nothing
  False -> Just <$> modify (\x -> x { isSetup = True })

teardown :: DB (Maybe ())
teardown = gets isSetup >>= \case
  False -> pure Nothing
  True -> Just <$> modify (\x -> x { isSetup = False })

flattenVersions =  concat . fmap toList

apply :: InputGroup -> DB (Maybe (Maybe OutputGroup))
apply xs = do
  WorldState {isVersions, isSetup} <- get
  if not isSetup
    then pure Nothing
    else Just <$> do
      let allVersions = map fst $ flattenVersions isVersions
          migrationsToApply = filter (not . flip elem allVersions . inputVersion) $ toList xs
      sequence_ $ map inputAction migrationsToApply

      (newVersions, res) <- case nonEmpty migrationsToApply of
        Nothing -> pure (isVersions, Nothing)
        Just ys -> pure (isVersions <> [fmap (\x -> (inputVersion x, inputHash x)) ys], Just $ fmap inputVersion ys)

      put . WorldState newVersions isSetup =<< gets isCounter

      pure res

listApplications :: DB (Maybe [OutputGroup])
listApplications = do
  WorldState {isVersions, isSetup} <- get
  if not isSetup
    then pure Nothing
    else pure $ Just $ map (fmap fst) isVersions

hashConflicts :: [InputMigration] -> DB (Maybe [Version])
hashConflicts [] = pure $ Just []
hashConflicts _ = error "hashConflicts"
{-
hashConflicts xs = do
  WorldState {isVersions, isSetup} <- get
  if not isSetup
    then pure Nothing
    else do
      let allVersions = flattenVersions isVersions
          theTest InputMigration {..} =
            if
      pure $ Just $ map inputVersion $ filter theTest xs
-}