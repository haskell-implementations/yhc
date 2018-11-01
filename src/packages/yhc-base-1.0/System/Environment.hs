module System.Environment where

import Foreign
import Foreign.C
import YHC.Primitive

getProgName :: IO String
getProgName =
  do name <- primGetProgName
     peekCString name

getArgs :: IO [String]
getArgs = getArgs' 0
  where
  getArgs' n = do
     arg <- primGetArg n
     let CString aptr = arg
     if aptr == nullPtr then
        return []
      else do
        args <- getArgs' (n+1)
        s <- peekCString arg
        return (s : args)

getEnv :: String -> IO String
getEnv s =
  do cs <- withCString s (\cs -> primGetEnv cs)
     let CString csptr = cs
     if csptr == nullPtr then
       return ""
      else
       peekCString cs


