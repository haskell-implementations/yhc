
module General where

import System.Time
import System
import Directory
import Control.Exception


data TestResult = FailCompile
                | FailStdOut
                | FailStdErr 
                | FailExpected 
                | Pass {compile :: Integer, run :: Integer}                
                deriving (Show, Read, Eq)

isPass :: TestResult -> Bool
isPass (Pass{}) = True
isPass _ = False

isBadFail :: TestResult -> Bool
isBadFail (FailExpected{}) = False
isBadFail x = not (isPass x)


        
timeSystem :: String -> IO Integer
timeSystem cmd = do x <- getClockTime
                    -- fix for GHC 6.4.1 bug
                    Control.Exception.catch (system cmd) (\_ -> return ExitSuccess)
                    y <- getClockTime
                    return $ diffToInteger $ diffClockTimes y x

-- in milliseconds
diffToInteger :: TimeDiff -> Integer
diffToInteger (TimeDiff{tdPicosec=ps, tdSec=sec, tdMin=min, tdHour=hour, tdDay=day,
                        tdMonth=month, tdYear=year}) =
    (ps `div` 1000000000) +
    toInteger sec   * 1000 +
    toInteger min   * 1000 * 60 +
    toInteger hour  * 1000 * 60 * 60 +
    toInteger day   * 1000 * 60 * 60 * 24 +
    toInteger month * 1000 * 60 * 60 * 24 * 30 +
    toInteger year  * 1000 * 60 * 60 * 24 * 365
    


readFileExist :: FilePath -> IO String
readFileExist x = do res <- doesFileExist x
                     if res then readFile x else return ""


removeFileSafe :: FilePath -> IO ()
removeFileSafe x = do res <- doesFileExist x
                      if res then removeFile x else return ()
                      


isFakeDir :: FilePath -> Bool
isFakeDir x = x `elem` [".",".."]


concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM f x = do xs <- mapM f x
                    return $ concat xs
                    
filterM :: Monad m => (a -> m Bool) -> [a] -> m [a]
filterM f [] = return []
filterM f (x:xs) = do res <- f x
                      rest <- filterM f xs
                      return $ if res then x : rest else rest


padding :: Show a => Int -> a -> String
padding width val = replicate (width - length x) ' ' ++ x
    where x = show val
