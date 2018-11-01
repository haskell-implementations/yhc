module Debug.Trace where

import System.IO
import YHC.Internal(unsafePerformIO)

trace :: String -> a -> a
trace []     a = a
trace (x:xs) a = unsafePerformIO (hPutChar stderr x) `seq` trace xs a
