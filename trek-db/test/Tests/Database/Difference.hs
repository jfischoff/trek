module Tests.Database.Difference where

import Data.Text (Text)
import Database.PostgreSQL.Transact
import qualified Data.Set as Set
import qualified Database.PostgreSQL.Simple.Types as PS
import Text.InterpolatedString.Perl6
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Foldable


data TableDiff = TableDiff
  { tdMissingColumns :: [Text]
  , tdExtraColumns   :: [Text]
  } deriving(Show, Eq, Ord)

instance Semigroup TableDiff where
  x <> y = TableDiff
    (tdMissingColumns x <> tdMissingColumns y)
    (tdExtraColumns x <> tdExtraColumns y)

instance Monoid TableDiff where
  mempty = TableDiff mempty mempty
  mappend = (<>)

validateColumns :: Text -> [Text] -> DB TableDiff
validateColumns tableName expectedList = do
  columnNames <- Set.fromList . map PS.fromOnly <$> query_ [qq|
          SELECT
            COLUMN_NAME
          FROM
            information_schema.COLUMNS
          WHERE
            TABLE_NAME = '{tableName}';
        |]

  let expected = Set.fromList expectedList
      tdMissingColumns = toList $ expected Set.\\ columnNames
      tdExtraColumns   = toList $ columnNames Set.\\ expected
  pure TableDiff {..}

-- Extra tables are fine in these situations
data SchemaDiff = SchemaDiff
  { sdMissingTables   :: [Text]
  , sdDifferentTables :: Map Text TableDiff
  } deriving (Show, Eq)

addDifferentTables :: SchemaDiff -> Text -> TableDiff -> SchemaDiff
addDifferentTables x n d
  | d == mempty = x
  | otherwise = x { sdDifferentTables = Map.insert n d $ sdDifferentTables x }

instance Semigroup SchemaDiff where
  x <> y = SchemaDiff
    { sdMissingTables   = sdMissingTables x <> sdMissingTables y
    , sdDifferentTables = sdDifferentTables x <> sdDifferentTables y
    }

instance Monoid SchemaDiff where
  mempty = SchemaDiff mempty mempty
  mappend = (<>)

tableExists :: Text -> DB Bool
tableExists tableName = PS.fromOnly . head <$> query_ [qq| SELECT EXISTS (
  SELECT 1
  FROM   information_schema.tables
  WHERE  table_schema = 'public'
  AND    table_name = '{tableName}'
  )|]

diffTable :: Text -> [Text] -> DB SchemaDiff
diffTable tableName columns = tableExists tableName >>= \case
  True ->  addDifferentTables mempty tableName <$> validateColumns tableName columns
  False -> pure $ mempty { sdMissingTables = [tableName]}

diffApplications :: DB SchemaDiff
diffApplications = diffTable "applications"  [ "id", "created_at"]

diffMigrations :: DB SchemaDiff
diffMigrations = diffTable "migrations"
  [ "version"
  , "name"
  , "created_at"
  , "application_id"
  , "hash"
  ]

diffSetup :: DB SchemaDiff
diffSetup = (<>) <$> diffApplications <*> diffMigrations

cleanSchemaDiff :: SchemaDiff
cleanSchemaDiff =
  mempty { sdMissingTables = ["applications", "migrations"]}