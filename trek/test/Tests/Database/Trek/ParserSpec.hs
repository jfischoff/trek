module Tests.Database.Trek.ParserSpec where
import Database.Trek.Parser
import Test.Hspec
import System.Environment
import Options.Generic
import Database.PostgreSQL.Simple.PartialOptions

spec :: Spec
spec = describe "Database.Trek.Parser" $ do
  it "parses create" $
    withArgs ["create", "foo"] $
     getRecord "trek" `shouldReturn` Create "foo"
  it "parses apply" $
    withArgs ["apply", "/path/to/migrations"] $
      getRecord "trek" `shouldReturn` Apply "/path/to/migrations" mempty
  it "parses apply" $
    withArgs ["apply", "/path/to/migrations", "--dbname=db"] $
      getRecord "trek" `shouldReturn` Apply "/path/to/migrations" (mempty { dbname = pure "db"})
