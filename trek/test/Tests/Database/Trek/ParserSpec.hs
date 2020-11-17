module Tests.Database.Trek.ParserSpec where
import Database.Trek.Parser
import Test.Hspec
import System.Environment
import Options.Generic
import Database.PostgreSQL.Simple.PartialOptions
import Data.Time.QQ


spec :: Spec
spec = describe "Database.Trek.Parser" $ do
  it "parses create" $
    withArgs ["create", "foo"] $
     getRecord "trek" `shouldReturn` Create "foo" InTransaction
  it "parses create with no-transaction" $
    withArgs ["create", "--no-transaction", "foo"] $
      getRecord "trek" `shouldReturn` Create "foo" NoTransaction
  it "parses apply" $
    withArgs ["apply", "/path/to/migrations"] $
      getRecord "trek" `shouldReturn` Apply mempty "/path/to/migrations"
  it "parses apply" $
    withArgs ["apply", "/path/to/migrations", "--dbname=db"] $
      getRecord "trek" `shouldReturn` Apply (mempty { dbname = pure "db"}) "/path/to/migrations"
  it "parses set-migration" $
    withArgs [ "set-migrated"
             , "--start-version=2020-07-12T05:00:00"
             , "--end-version=2020-07-12T07:00:00"
             , "/path/to/migrations"
             ] $
      getRecord "trek" `shouldReturn`
        SetMigrated
          mempty
          (pure [utcIso8601ms| 2020-07-12T05:00:00.00000 |])
          (pure [utcIso8601ms| 2020-07-12T07:00:00.00000 |])
          "/path/to/migrations"

  it "parses set-migration with no range" $
    withArgs [ "set-migrated"
             , "/path/to/migrations"
             ] $
      getRecord "trek" `shouldReturn`
        SetMigrated
          mempty
          mempty
          mempty
          "/path/to/migrations"
