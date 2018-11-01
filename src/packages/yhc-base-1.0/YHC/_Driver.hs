#ifndef __HADDOCK__
module YHC._Driver where

import YHC.Internal (IO,World(..))
import YHC.Primitive

import Prelude hiding (catch)
import System.Exit
import System.IO.Unsafe(unsafePerformIO)
import System.IO hiding (catch)
import Control.Exception
import Control.Concurrent

_toplevel :: World
_toplevel = World

_driver :: World -> IO () -> b
_driver _ main =
    case doit of                    -- drivers can't RETURN_EVAL since they are the first thing on the stack
        () -> undefined
    where
    doit = unsafePerformIO (do
            catch main (\e -> do
                    hPutStrLn stderr (show e)
                    reallyExit (ExitFailure (negate 1))
                )
            reallyExit ExitSuccess
        )

reallyExit :: ExitCode -> a
reallyExit ExitSuccess     = primExitWith 0
reallyExit (ExitFailure n) = primExitWith n

#endif

