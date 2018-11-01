
module Make.Profile(loadProfile, saveProfile, ask, setProfile, argsProfiles) where

import qualified Data.Map as Map
import Data.IORef
import System.Directory
import Control.Monad
import Data.List
import Data.Char
import Foreign


i :: IORef Profile
i = unsafePerformIO (newIORef Map.empty)


loadProfile x = readProfile x >>= writeIORef i
saveProfile = readIORef i >>= writeProfile ""


type Profile = Map.Map String String


fileProfile :: String -> FilePath
fileProfile x = "profiles/" ++ (if null x then "default" else x) ++ ".txt"


readProfile :: String -> IO Profile
readProfile x = do
    let file = fileProfile x
    b <- doesFileExist file
    if b then liftM parseProfile $ readFile file
     else if null x then return Map.empty
     else error $ "Could not find the specified profile, " ++ x


writeProfile :: String -> Profile -> IO ()
writeProfile x p = do
    b <- doesDirectoryExist "profiles"
    when (not b) $ createDirectory "profiles"
    let file = fileProfile x
        f (a,b) = [a ++ "=" ++ b | not $ "!" `isPrefixOf` a]
    writeFile file $ unlines $ concatMap f $ Map.toList p


ltrim = dropWhile isSpace
trim = reverse . ltrim . reverse . ltrim
unquote x = if nx >= 2 && head x == '\"' && last x == '\"' then take (nx-2) (tail x) else x
    where nx = length x


parseProfile :: String -> Map.Map String String
parseProfile = Map.fromList . concatMap (parseSetting . dropComment . ltrim) . lines
    where dropComment x = if "--" `isPrefixOf` x then "" else x


parseSetting :: String -> [(String,String)]
parseSetting = f . unquote . trim
    where
        f x | null value = []
            | otherwise = [(unquote $ trim key, unquote $ trim $ tail value)]
            where (key,value) = break (== '=') x


ask :: String -> IO String
ask x = do
    p <- readIORef i
    return $ Map.findWithDefault "" x p


argsProfiles :: [String] -> IO ()
argsProfiles = mapM_ (uncurry setProfile) . concatMap (force . parseSetting)
    where force x = x ++ [('!':a,b) | (a,b) <- x]


setProfile :: String -> String -> IO ()
setProfile k v = modifyIORef i (Map.insert k v)
