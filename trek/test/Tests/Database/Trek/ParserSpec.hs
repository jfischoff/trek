module Tests.Database.Trek.ParserSpec where
import Database.Trek.Parser
import Test.Hspec
import System.Environment
import Options.Generic

spec :: Spec
spec = describe "Database.Trek.Parser" $ do
  it "parses create" $
    withArgs ["create", "foo"] $
     getRecord "trek" `shouldReturn` Create "foo"
  it "parses create" $
    withArgs ["apply", "/path/to/migrations"] $
      getRecord "trek" `shouldReturn` Apply "/path/to/migrations"
