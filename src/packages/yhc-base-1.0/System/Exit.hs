module System.Exit
    (
      ExitCode(..)

     ,exitWith
     ,exitFailure
    )
    where

import Prelude
import YHC.Primitive
import YHC.Exception

-- Originally From ExitFailure.hs

exitFailure     :: IO a
exitFailure      = exitWith (ExitFailure 255)   -- value 255 is arbitrary

-- Originally From ExitWith.hs

exitWith                :: ExitCode -> IO a
exitWith code = throw (ExitException code)
