
module Make.Useful(
    module System.Directory,
    module System.Environment,
    module System.Exit,
    module Control.Monad,
    module Data.Char,
    module Data.List,
    module Data.Maybe,
    module Make.Profile,
    module Make.Useful
    ) where

import System.Directory hiding (copyFile, removeDirectoryRecursive)
import qualified System.Directory
import System.Environment
import qualified System.Cmd
import System.Exit
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Make.Profile(ask)

-- for private use only
import System.Info(os)

abort :: String -> IO ()
abort x = putStrLn x >> exitFailure


windows :: Bool
windows = os == "mingw32"


-- occasionally Windows demands the correct slash direction
slashes :: String -> String
slashes = if windows then map f else id
    where f x = if x == '/' then '\\' else x


copyFile :: FilePath -> FilePath -> IO ()
copyFile from to = do
    verbose <- ask "verbose"
    when (not $ null verbose) $ putStrLn $ "copyFile: " ++ from ++ " -> " ++ to
    System.Directory.copyFile from to


system :: String -> IO ExitCode
system cmd = do
    verbose <- ask "verbose"
    when (not $ null verbose) $ putStrLn $ "system: " ++ cmd
    System.Cmd.system cmd


systemBool :: String -> IO Bool
systemBool cmd = do
    res <- system cmd
    return $ res == ExitSuccess


systemSuccess :: String -> String -> IO ()
systemSuccess msg cmd = systemSuccessCont cmd (abort msg) (return ())


systemSuccessCont :: String -> IO a -> IO a -> IO a
systemSuccessCont cmd failed success = do
    b <- systemBool cmd
    if b then success else failed


systemPipe :: String -> String -> String -> IO String
systemPipe msg cmd out = do
    systemSuccess msg (cmd ++ " > " ++ out)
    readFile out


systemPipeCont :: String -> String -> IO a -> (String -> IO a) -> IO a
systemPipeCont cmd out failed success =
    systemSuccessCont (cmd ++ " > " ++ out) failed (readFile out >>= success)


systemSequence :: [String] -> IO Bool
systemSequence = ioSequence . map systemBool

ioSequence :: [IO Bool] -> IO Bool
ioSequence [] = return True
ioSequence (x:xs) = do
    b <- x
    if b then ioSequence xs else return False


ensureDirectory :: FilePath -> IO ()
ensureDirectory x = do
    b <- doesDirectoryExist x
    when (not b) $ createDirectory x


exe_ = if windows then (++ ".exe") else id


demand :: [String] -> IO [String]
demand [] = return []
demand (x:xs) = do
    y <- ask x
    when (null y) $ abort $ "ERROR: " ++ x ++ ", required component missing"
    ys <- demand xs
    return (y:ys)

request :: [String] -> IO [String]
request [] = return []
request (x:xs) = do
    y <- ask x
    when (null y) $ putStrLn $ "WARNING: " ++ x ++ ", optional component missing -- some features missing"
    ys <- request xs
    return (y:ys)


replace :: Char -> Char -> String -> String
replace a b = map (\x -> if x == a then b else x)


snub x = sort (nub x)

(</>) :: String -> String -> String
x </> y = x ++ ['/' | not $ "/" `isSuffixOf` x] ++ y

-- (folders, files)
getContentsRec :: FilePath -> IO ([FilePath], [FilePath])
getContentsRec dir = do
    items <- getDirectoryContents dir
    dirs  <- filterM (doesDirectoryExist . full) $ filter (not . ("." `isPrefixOf`)) items
    files <- filterM (doesFileExist . full) items
    
    (ds,fs) <- mapAndUnzipM cont dirs
    return (dirs ++ concat ds, files ++ concat fs)
    where
        full x = dir </> x
        
        cont x = do
            (fs,ds) <- getContentsRec $ full x
            return (map (x </>) fs, map (x </>) ds)


-- should you build the second file from the first
shouldBuild :: FilePath -> FilePath -> IO Bool
shouldBuild src dest = do
    exist <- doesFileExist dest
    if not exist then return True else do
        dtime <- getModificationTime dest
        stime <- getModificationTime src
        return $ dtime < stime



concatMapM f = liftM concat . mapM f


removeDirectoryRecursive dir = do
    b <- doesDirectoryExist dir
    when b $ System.Directory.removeDirectoryRecursive dir


ltrim = dropWhile isSpace
rtrim = reverse . ltrim . reverse
trim = ltrim . rtrim


takeFileName :: FilePath -> String
takeFileName = reverse . takeWhile (/= '/') . reverse

dropFileName :: FilePath -> String
dropFileName = reverse . drop 1 . dropWhile (/= '/') . reverse


takeExtension :: FilePath -> String
takeExtension x = case break (== '.') $ reverse $ takeFileName x of
                       (_, []) -> ""
                       (x, _ ) -> '.' : reverse x

dropExtension :: FilePath -> String
dropExtension x = take (length x - length (takeExtension x)) x
