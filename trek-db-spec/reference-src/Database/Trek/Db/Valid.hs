module Database.Trek.Db.Valid where
import Database.Trek.Db.TestInterface.Types
import Control.Monad.State
import Data.List.NonEmpty (NonEmpty, fromList, nonEmpty)
import Data.Foldable

type WorldState = [NonEmpty (Version, Int)]

data InternalState = InternalState
  { isVersions :: WorldState
  , isSetup    :: Bool
  , isCounter  :: Int
  } deriving (Show, Eq, Ord)

initialInternalState :: InternalState
initialInternalState = InternalState [] False 0

type DB = StateT InternalState IO

type Version = Int

data InputMigration = InputMigration
  { inputAction :: DB ()
  , inputVersion :: Version
  , inputHash :: Int
  }

clear :: DB ()
clear = put initialInternalState

worldState :: DB WorldState
worldState = isVersions <$> get

type InputGroup = NonEmpty InputMigration

inputGroup :: NonEmpty InputMigration -> DB InputGroup
inputGroup = pure

type OutputGroup = NonEmpty Version

toOutput :: InputGroup -> DB OutputGroup
toOutput = pure . fmap inputVersion

setup :: DB (Maybe ())
setup = gets isSetup >>= \case
  True -> pure Nothing
  False -> Just <$> modify (\x -> x { isSetup = True })

teardown :: DB (Maybe ())
teardown = gets isSetup >>= \case
  False -> pure Nothing
  True -> Just <$> modify (\x -> x { isSetup = False })

flattenVersions =  concat . fmap toList

apply :: InputGroup -> DB (Maybe OutputGroup)
apply xs = do
  setup
  InternalState {isVersions, isSetup} <- get

  let allVersions = map fst $ flattenVersions isVersions
      migrationsToApply = filter (not . flip elem allVersions . inputVersion) $ toList xs
  sequence_ $ map inputAction migrationsToApply

  (newVersions, res) <- case nonEmpty migrationsToApply of
    Nothing -> pure (isVersions, Nothing)
    Just ys -> pure (isVersions <> [fmap (\x -> (inputVersion x, inputHash x)) ys], Just $ fmap inputVersion ys)

  put . InternalState newVersions isSetup =<< gets isCounter

  pure res

listApplications :: DB (Maybe [OutputGroup])
listApplications = do
  InternalState {isVersions, isSetup} <- get
  if not isSetup
    then pure Nothing
    else pure $ Just $ map (fmap fst) isVersions

hashConflicts :: [InputMigration] -> DB (Maybe [Version])
hashConflicts [] = pure $ Just []
hashConflicts xs = do
  InternalState {isVersions, isSetup} <- get
  if not isSetup
    then pure Nothing
    else do
      let allVersions = flattenVersions isVersions
          theTest InputMigration {..} =
            case lookup inputVersion allVersions of
              Nothing -> False
              Just theHash -> theHash /= inputHash
      pure $ Just $ map inputVersion $ filter theTest xs
