
module Yhc.Core.Serialise(saveCore, loadCore) where

import Yhc.Core.Type
import Yhc.Core.Binary
import Yhc.Core.Internal.Binary
import System.IO


coreStr :: String
coreStr = "YHC-CORE"
coreVer :: Int
coreVer = 4

saveCore :: FilePath -> Core -> IO ()
saveCore file core = writeBinary file (coreStr,coreVer,core)

loadCore :: FilePath -> IO Core
loadCore file = do
    hndl <- openBinaryFile file ReadMode
    a <- get hndl
    b <- get hndl
    if a /= coreStr && b /= coreVer
        then hClose hndl >> error ("Incompatible Core file, " ++ file)
        else do c <- get hndl; hClose hndl; return c
