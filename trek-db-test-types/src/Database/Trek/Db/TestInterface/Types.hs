module Database.Trek.Db.TestInterface.Types where

-- Necessary because we don't have impredicative types
data SpecStateM m = SpecState
  { ssRunner   :: forall a. m a -> IO a
  , ssShutdown :: IO ()
  }