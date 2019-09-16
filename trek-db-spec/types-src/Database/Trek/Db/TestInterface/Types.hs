module Database.Trek.Db.TestInterface.Types where

data SpecStateM m = SpecState
  { ssRunner   :: forall a. m a -> IO a
  , ssShutdown :: IO ()
  }