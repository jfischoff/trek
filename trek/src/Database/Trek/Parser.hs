module Database.Trek.Parser where
import Options.Generic

data Command = Apply FilePath | Create String
  deriving (Show, Eq, Generic)

instance ParseRecord Command
