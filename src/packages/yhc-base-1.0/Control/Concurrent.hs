-- Yhc concurrency implementation

module Control.Concurrent(ThreadId, killProcess, forkIO, throwTo, myThreadId) where

import Prelude hiding (catch)
import YHC.Internal(_mkIOok1, unsafePerformIO)
import YHC.Primitive
import System.IO hiding (catch)
import Control.Exception

-- kill the current process
killProcess :: IO ()
killProcess = primKillProcess

-- spin lock the current process (for testing)
spinLockProcess :: Int -> IO Int
spinLockProcess n = primSpinLockProcess n

-- fork an IO action, return it's thread
forkIO :: IO () -> IO ThreadId
forkIO act = primSpawnProcess closure
  where
  closure = _E (runThread act)

-- return the id of this thread
myThreadId :: IO ThreadId
myThreadId = primMyThreadId ()

-- this code is almost identical to YHC._Driver._driver however it's very difficult to factor the difference out
-- because of the restriction that drivers musn't RETURN_EVAL.
runThread :: IO () -> a
runThread act =
    case doit of                    -- drivers can't RETURN_EVAL since they are the first thing on the stack
        () -> undefined
    where
    doit = unsafePerformIO (do
            catch act (\e -> do
                    hPutStrLn stderr ("Thread: "++show e)
                    killProcess
                )
            killProcess
        )
