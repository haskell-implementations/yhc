
module Run(runTest) where

import General
import Directory
import System.Directory
--import Diff

-- must be run from the correct directory
runTest :: String -> String -> IO TestResult
runTest yhc yhi = do
        removeFileSafe "Main.hbc"
        bug $ removeFileSafe "Main.hbc"
        
        flags <- do b <- doesFileExist "flags.txt"
                    if not b then return "" else do
                        s <- readFile "flags.txt"
                        return $ unwords $ lines s
        
        compile <- timeSystem $
            yhc ++ " Main " ++ flags ++
            " 2> stderr.compiler " ++
            " >  stdout.compiler"
                  
        succWrong <- bug $ doesFileExist "Main.hbc"

        flagsExists <- doesFileExist $ "expected.yhiflags"
        flags <- if flagsExists then do
                    txt <- readFile "expected.yhiflags"
                    return $ head $ lines txt
                  else
                    return ""
        success <- doesFileExist "Main.hbc"
        makeerr <- doesFileExist "expected.makerr"
        may_fail <- doesFileExist "expected.fail"
        input <- doesFileExist "expected.stdin"        
        

        r <- if success then do
            e1 <- errOn makeerr "Expected make error"
            run <- timeSystem $
                yhi ++ " " ++ flags ++ " Main" ++
                " 2> actual.stderr" ++
                " >  actual.stdout" ++
                if input then " < expected.stdin" else ""
                
            e2 <- errDiff "stdout" "expected.stdout" "actual.stdout"
            e3 <- errDiff "stderr" "expected.stderr" "actual.stderr"
            
            return $ if not e1 then FailCompile
                          else if not e2 then FailStdOut
                          else if not e3 then FailStdErr
                          else Pass compile run
         else do
            e1 <- errOn (not makeerr) "Unexpected make error"
            return $ if makeerr then Pass 0 0 else FailCompile

            
        if may_fail && not (isPass r) then do
             putStrLn "Failure was expected"
             return FailExpected
         else return r
    where
        -- only for bugs, should be removed when Yhc is fixed
        bug x = x

        


-- return True if you pass
errOn :: Bool -> String -> IO Bool
errOn x msg = do if x then (putStrLn $ "ERROR: " ++ msg) else return ()
                 return (not x)


-- return True if you pass
errDiff :: String -> FilePath -> FilePath -> IO Bool
errDiff msg f1 f2 = do c1 <- readFileExist f1
                       c2 <- readFileExist f2
                       let res = c1 /= c2 -- isDiff c1 c2
                       if res then
                           do putStrLn $ "ERROR: different " ++ msg
                              putStrLn $ "EXPECTED:\n" ++ c1 ++ "\nACTUAL:\n" ++ c2
                              return False
                        else
                           return True

