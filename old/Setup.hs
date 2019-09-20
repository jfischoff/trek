module Database.Trek.Db.Setup where
import Database.Trek.Db.MigrationException

import Database.PostgreSQL.Simple.SqlQQ
import qualified Database.PostgreSQL.Simple as PS
import Database.PostgreSQL.Simple.FromRow
import Database.PostgreSQL.Simple.ToRow
import Database.PostgreSQL.Transact
import Control.Monad (void)
import Data.Time
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Data.Foldable
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Text.InterpolatedString.Perl6
import Control.Monad.Catch
import GHC.Generics
import Data.Traversable
import qualified Data.ByteString as BS

-- TODO make this use Either
