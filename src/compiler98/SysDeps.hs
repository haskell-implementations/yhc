module SysDeps (
   module PackedString, trace, openBinaryFileWrite, osName
) where

import Data.PackedString as PackedString
import System.IO
import Debug.Trace (trace)
import System.Info

openBinaryFileWrite :: FilePath -> IO Handle
openBinaryFileWrite f = openBinaryFile f WriteMode

osName :: String
osName = if compilerName == "yhc" || os /= "mingw32"
         then os
         else "windows"
