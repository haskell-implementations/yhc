
{-|
    Command Line Options
-}


module Main where

import System
import Directory
import Monad
import Char
import List
import System.Time


import Report
import Run
import General



main :: IO ()
main = do x <- getArgs
          let logging = "-log" `elem` x
              report  = "-report" `elem` x
              norun   = "-norun" `elem` x
              dead    = "-dead" `elem` x
              dirs    = filter (not . isArg) x
              
          if null dirs then helpMsg
           else do x <- concatMapM (f dead norun report logging) dirs
                   when (not $ null x) $
                        do failures <- writeSummary x
                           when (failures > 0) $ exitWith (ExitFailure failures)

                   when report $ putStrLn "Reports written"
    where
        f dead norun report logging x = do
            tests <- findTests dead x
            let tests2 = sortCaseInsensitive $ map (drop (length x+1)) tests
            res <- if norun then return [] else runTests x tests2
            if logging then writeLog x res else return ()
            if report then writeReport x else return ()
            return $ map snd res


isArg :: String -> Bool
isArg ('-':_) = True
isArg _ = False


helpMsg :: IO ()
helpMsg = putStr $ unlines [
    "Yhc Test Suite",
    "test [-log] [-report] <dir>",
    "   Where <dir> is the name of a directory containing test files",
    "   -log, turn on performance logging",
    "   -report, write out a report in html (called report.html)",
    "   -norun, do not run the tests (just gen a report, if -report is on)"
    ]



sortCaseInsensitive :: [String] -> [String]
sortCaseInsensitive xs = sortBy f xs
    where f a b = map toLower a `compare` map toLower b


-- the return value should be the stem to which you need to combine the result
findTests :: Bool -> FilePath -> IO [FilePath]
findTests dead x =
    do
        q <- doesDirectoryExist x
        if not q
            then return []
            else do
                children <- getDirectoryContents x
                let rchildren = filter (not . isFakeDir) children
                    fchildren = map (\a -> x ++ "/" ++ a) rchildren
                    hasFile = ("Main.hs" `elem` children) || ("Main.lhs" `elem` children)
                    exists = hasFile && (dead || not ("dead.txt" `elem` children))
                rest <- concatMapM (findTests dead) fchildren
                return $ if exists then x : rest else rest
                 
                 
runTests :: FilePath -> [FilePath] -> IO [(FilePath, TestResult)]
runTests path tests = do
        basepath <- getEnv "YHC_BASE_PATH"
        let bindir = basepath ++ "/bin/"
        res <- zipWithM (f bindir) [1..] tests
        return res
    where
        count = length tests
        width = length $ show count
    
        f bindir val test =
            do cur <- getCurrentDirectory
               setCurrentDirectory $ path ++ "/" ++ test
               putStrLn $ "-- TEST (" ++ padding width val ++ "/" ++ show count ++ ") - " ++ test
               x <- runTest (bindir ++ "yhc") (bindir ++ "yhi")
               setCurrentDirectory cur
               return (test, x)


writeLog :: String -> [(FilePath, TestResult)] -> IO ()
writeLog x res = do
        clockt <- getClockTime
        ct <- toCalendarTime clockt
        appendFile (x ++ "/report.log") (show (ct, res) ++ "\n")
    where


writeSummary :: [TestResult] -> IO Int
writeSummary res = do
        let count = length res
            width = length $ show count
            pass = length $ filter isPass res
            bad_fail = length $ filter isBadFail res
            failed_all = count - pass
        putStr $ unlines [
             "-- SUMMARY",
             "Passed:  " ++ padding width pass,
             "Failed:  " ++ padding width failed_all,
             "Painful: " ++ padding width bad_fail,
             "Total :  " ++ show count
             ]
        return bad_fail
