module Database.Trek.Parser where
import Options.Generic
import Database.PostgreSQL.Simple.PartialOptions
import qualified Options.Applicative.Builder as B

data Command = Apply PartialOptions FilePath | Create String
  deriving (Show, Eq, Generic)

instance ParseRecord Command where
  parseRecord = do
    let applyParser
           =  Apply
          <$> parseRecord
          <*> B.strArgument (B.metavar "FILEPATH")

        createParser
          = Create <$> B.strArgument (B.metavar "NAME")

    B.subparser
     (  B.command "apply"
          (B.info applyParser (B.progDesc "Apply migrations"))
     <> B.command "create"
          (B.info createParser (B.progDesc "Create migrations"))
     )
