module System.Directory where

import YHC.Primitive
import Foreign.C
import Foreign.Ptr

createDirectory :: FilePath -> IO ()
createDirectory fp = wrapIO (withCString fp primCreateDir)

createDirectoryIfMissing :: Bool -> FilePath -> IO ()
createDirectoryIfMissing = error "createDirectoryIfMissing: not implemented yet"

removeDirectory :: FilePath -> IO ()
removeDirectory fp = wrapIO (withCString fp primRemoveDir)

removeDirectoryRecursive :: FilePath -> IO ()
removeDirectoryRecursive = error "removeDirectoryRecursive: not implemented yet"

renameDirectory :: FilePath -> FilePath -> IO ()
renameDirectory from to = wrapIO $
  withCString from $ \ from' ->
  withCString to   $ \ to' ->
  primRenameDir from' to'

getDirectoryContents :: FilePath -> IO [FilePath]
getDirectoryContents fp = do
       dir <- withCString fp primOpenDir
       fs <- list dir
       wrapIO (primCloseDir dir)
       return fs
  where
  list dir = do
    dirent <- primNextDir dir
    if dirent == nullPtr then
      return []
     else do
      cname <- primGetDirName dirent
      name <- peekCString cname
      names <- list dir
      return (name:names)

getCurrentDirectory :: IO FilePath
getCurrentDirectory = primGetCurrentDir >>= peekCString

setCurrentDirectory :: FilePath -> IO ()
setCurrentDirectory fp = wrapIO $ withCString fp primSetCurrentDir

getHomeDirectory :: IO FilePath
getHomeDirectory = primGetHomeDir >>= peekCString

getAppUserDataDirectory :: String -> IO FilePath
getAppUserDataDirectory = error "getAppUserDataDirectory: not implemented yet"

getUserDocumentsDirectory :: IO FilePath
getUserDocumentsDirectory = error "getUserDocumentsDirectory: not implemented yet"

getTemporaryDirectory :: IO FilePath
getTemporaryDirectory = error "getTemporaryDirectory: not implemented yet"

removeFile :: FilePath -> IO ()
removeFile fp = wrapIO $ withCString fp primRemoveFile

renameFile :: FilePath -> FilePath -> IO ()
renameFile from to = wrapIO $
  withCString from $ \ from' ->
  withCString to   $ \ to' ->
  primRenameFile from' to'

copyFile :: FilePath -> FilePath -> IO ()
copyFile src dst = wrapIO $
  withCString src $ \ src' ->
  withCString dst $ \ dst' ->
  primCopyFile src' dst'

canonicalizePath :: FilePath -> IO FilePath
canonicalizePath = error "canonicalizePath: not implemented yet"

findExecutable :: String -> IO (Maybe FilePath)
findExecutable = error "findExecutable: not implemented yet"

doesFileExist :: FilePath -> IO Bool
doesFileExist fp = withCString fp primFileExists

doesDirectoryExist :: FilePath -> IO Bool
doesDirectoryExist fp = withCString fp primDirExists

data Permissions = Permissions {
  readable, writable, executable, searchable :: Bool
}

getPermissions :: FilePath -> IO Permissions
getPermissions = error "getPermissions: not implemented yet"

setPermissions :: FilePath -> Permissions -> IO ()
setPermissions = error "setPermissions: not implemented yet"

--getModificationTime :: FilePath -> IO ClockTime
--getModificationTime = error "get modification time

wrapIO :: IO Int -> IO ()
wrapIO f =
  do v <- f -- FIXME: throw error and stuff!
     return ()


