module Database.Trek.Db.NoSetup where
import Database.Trek.Db.Types

toMigrationException :: PS.SqlError -> Either NoSetup PS.SqlError
toMigrationException = \case
  PS.SqlError
    { sqlExecStatus = PS.FatalError
    , sqlErrorMsg = msg
    } ->  case msg of
      "relation \"applications\" already exists" -> Just $ NoSetup msg
      "relation \"applications\" does not exist" -> Just $ NoSetup msg
      "relation \"migrations\" already exists"   -> Just $ NoSetup msg
      "relation \"migrations\" does not exist"   -> Just $ NoSetup msg
      _ -> Nothing
  _ -> Nothing

mapSqlError :: MonadCatch m => m a -> m a
mapSqlError = handle (either throwM throwM . toMigrationException)
