-- TODO make better not head
-- Also only used by the test code ... maybe move there ...
getMigrationRow :: Version -> DB (MigrationRow From)
getMigrationRow version = fmap head $ query [sql|
    SELECT version, name, hash, application_id, created_at
    FROM migrations
    WHERE version = ?
  |] $ PS.Only version


-- Also only used by the test code
clearMigrations :: DB ()
clearMigrations = void $ execute_ [sql| TRUNCATE applications CASCADE |]
