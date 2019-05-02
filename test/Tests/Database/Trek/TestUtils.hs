module Tests.Database.Trek.TestUtils where

import Test.Hspec
import qualified Control.Concurrent.Async as Async
import Control.Concurrent.Async (Async)
import Control.Concurrent
import Control.Monad (forever)
import System.IO.Temp

withTempDir :: String -> SpecWith FilePath -> Spec
withTempDir name = aroundAll (withSystemTempDirectory name)

aroundAll :: forall a. ((a -> IO ()) -> IO ()) -> SpecWith a -> Spec
aroundAll withFunc specWith = do
  let start :: IO (a, Async ())
      start = do
        var <- newEmptyMVar

        thread <- Async.async $ withFunc $ \value -> do
          putMVar var value
          forever $ threadDelay 100000000

        theA <- takeMVar var
        pure (theA, thread)

      stop :: (a, Async ()) -> IO ()
      stop = Async.cancel . snd

  beforeAll start $ afterAll stop $ beforeWith (pure . fst) specWith