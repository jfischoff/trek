module System.Posix.Process.Read where
import System.Posix.Process (forkProcess)
import System.Posix.IO      (createFile, dupTo, stdOutput, stdError)
import System.Posix.Files   (accessModes)
import Control.Monad        (forever, void)
import Control.Concurrent   (threadDelay)
import System.Posix.Signals
import Control.Exception

readProcess :: IO () -> IO (ExitCode, String, String)
readProcess process = do
  processFinished <- newEmptyMVar
  bracket
      (forkProcess $ do
          (newStdoutIn, newStdoutOut) <- createPipe
          dupTo newStdoutIn stdOutput

          (newStderrIn  newStderrOut) <- createPipe
          dupTo newStderrIn stdError

          process

        cancel stdErrReader
        cancel stdOutReader

          putMVar processFinished ()
      )
      (signalProcess sigKILL)
      $ \_ -> takeMVar processFinished
