module Database.Trek.Parser where
import Options.Generic
import Database.PostgreSQL.Simple.PartialOptions
import qualified Options.Applicative.Builder as B
import Control.Applicative
import Data.Time

data Command
  = Apply PartialOptions FilePath
  | Create String
  | SetMigrated PartialOptions (Last UTCTime) (Last UTCTime) FilePath
  deriving (Show, Eq, Generic)

utcReader :: B.ReadM UTCTime
utcReader = B.maybeReader $ parseTimeM True defaultTimeLocale "%Y-%m-%dT%H:%M:%S"

instance ParseRecord Command where
  parseRecord = do
    let applyParser
           =  Apply
          <$> parseRecord
          <*> B.strArgument (B.metavar "FILEPATH")

        createParser
          = Create <$> B.strArgument (B.metavar "NAME")

        setMigratedParser
           =  SetMigrated
          <$> parseRecord
          <*> fmap Last
              ( optional
              $ B.option utcReader
              $ B.long "start-version"
              )
          <*> fmap Last
              ( optional
              $ B.option utcReader
              $ B.long "end-version"
              )
          <*> B.strArgument (B.metavar "FILEPATH")

    B.subparser
     (  B.command "apply"
          (B.info applyParser (B.progDesc "Apply migrations"))
     <> B.command "create"
          (B.info createParser (B.progDesc "Create migrations"))
     <> B.command "set-migrated"
          (B.info setMigratedParser (B.progDesc "Set migrations as applied in the data without running them"))
     )
