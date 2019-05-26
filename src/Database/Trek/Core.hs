module Database.Trek.Core where

import Control.Exception
import Database.PostgreSQL.Transact

import Database.Trek.Types
import qualified Database.Trek.Db as Db
import Data.Typeable

setup :: DB ()
setup = Db.mapSqlError $ Db.setup Prod

diffSetup :: DB Db.SchemaDiff
diffSetup = Db.diffSetup Prod

teardown :: DB ()
teardown = Db.mapSqlError $ Db.teardown

data HashConflict = HashConflict
  { hcConflictingVersions :: [Version]
  , hcContinuation :: DB ()
  } deriving (Typeable)

instance Show HashConflict where
  show HashConflict {..}
    =  "Hash conflict with the following versions "
    ++ unwords (map show hcConflictingVersions)

instance Exception HashConflict

-- Throws HashConflict
migrate :: [Migration] -> DB ()
migrate _ = Db.mapSqlError $ pure ()