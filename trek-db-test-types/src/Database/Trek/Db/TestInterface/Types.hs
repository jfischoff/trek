module Database.Trek.Db.TestInterface.Types where

-- Necessary because we don't have impredicative types
newtype SpecStateM m = SpecState { ssRunner :: forall a. m a -> IO a }
