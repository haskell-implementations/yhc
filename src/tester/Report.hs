
module Report(writeReport) where

import System.Time
import General
import List


writeReport :: FilePath -> IO ()
writeReport path = do xs <- getReportData (path ++ "/report.log")
                      res <- formatReportData path xs
                      writeFile "report.html" res




formatReportData :: FilePath -> [(CalendarTime, [(FilePath, TestResult)])] -> IO String
formatReportData path x = do
    errTxt <- if null errMsgs
              then return ["<p>No errors occurred</p>"]
              else mapM (uncurry showError) errMsgs
    return $ unlines $ 
        ["<html>"
        ,"  <head>"
        ,"    <title>Yhc Test Results</title>"
        ,"    <style type='text/css'>"
        ,unlines $ dateStyles
        ,"      .pass {background-color: green;}"
        ,"      .fail {background-color: red;}"
        ,"      table {border-collapse: collapse;}"
        ,"      .graph td {padding:0px;margin:0px;vertical-align:bottom;}"
        ,"      .graph {border:1px solid gray; height:100px;}"
        ,"    </style>"
        ,"  <body>"
        ,"    <h1>Yhc Test Report</h1>"
        ,"    <h2>Pass/Fail matrix</h2>"
        ,"    <table>"
        ,"      <tr><td><b>Test name</b></td>" ++ dateHeader ++ "</tr>"
        ,unlines $ zipWith passFail [1..] tests
        ,"    </table>"
        ,"    <h2>Pass/Fail graph</h2>"
        ,"    <table class='graph'><tr>"
        ,unlines $ zipWith graphBar [1..] dates
        ,"    </tr></table>"
        ,"    <h2>Error Messages</h2>"
        ,unlines $ errTxt
        ,"  </body>"
        ,"</html>"
        ]
    where
        errMsgs = filter (lastFail . snd) $ zip [1..] tests
    
        dateStyles = [".t" ++ show i ++ " {background-color:rgb(" ++ j ++ "," ++ j ++ ",255);}" |
            (i,d) <- zip [1..] ddates,
            j <- [show $ 200 - (d*2)]]
        
        dateHeader = concat $ ["<td class='t" ++ show i ++ "'>&nbsp;</td>" | i <- [1..length dates]]
    
        tests = sort $ nub $ map (fst . fst) items
        dates = sort $ nub $ map (snd . fst) items
        
        hdate = toClockTime (head dates)
        ldate = last dates
        ddates = distribute [diffToInteger (toClockTime x `diffClockTimes` hdate) | x <- dates]
    
        items = [((file,date),res) | (date,fileres) <- x, (file,res) <- fileres]
        
        
        lastFail test = case lookup (test, ldate) items of
                          Just x | not (isPass x) -> True
                          _ -> False
        
        passFail n test = "<tr><td>" ++ test ++ "</td>" ++ concatMap f dates ++ lst ++ "</tr>"
            where
                lst = if lastFail test
                      then "<td><a href='#t" ++ show n ++ "'>(view)</a></td>"
                      else "<td>&nbsp;</td>"
            
                f date = case lookup (test,date) items of
                             Nothing -> "<td></td>"
                             Just (Pass a b) -> "<td class='pass'>&nbsp;</td>"
                             Just _ -> "<td class='fail'>&nbsp;</td>"
                             
        graphBar n date = "<td><div class='t" ++ show n ++ "' style='height:" ++ show perc ++ "px'>&nbsp;</div></td>"
            where
                perc = floor (pass * 100 / total)
                x = [c | ((a,b),c) <- items, b == date]
                total = (genericLength x) :: Float
                pass = (genericLength $ filter isPass x) :: Float
                
                
        showError n test = do
                err <- case lookup (test, ldate) items of
                    Just FailCompile -> do a <- readFileExist (f "stderr.compiler")
                                           b <- readFileExist (f "stdout.compiler")
                                           return $ "COMPILE ERROR:\n" ++ a ++ "\n" ++ b
                    Just FailStdErr -> g "stderr"
                    Just FailStdOut -> g "stdout"
                    _ -> return "TODO"
                return $
                    "<h3><a name='t" ++ show n ++ "'> </a>" ++
                    "Failure for " ++ test ++ "</h3>" ++
                    "<pre>" ++ err ++ "</pre>"
            where
                f x = path ++ "/" ++ test ++ "/" ++ x
                
                g x = do a <- readFileExist (f $ "expected." ++ x)
                         b <- readFileExist (f $ "actual." ++ x)
                         return $ "EXPECTED:\n" ++ a ++ "\nFOUND:\n" ++ b
        


getReportData :: FilePath -> IO [(CalendarTime, [(FilePath, TestResult)])]
getReportData xs = do x <- readFile xs
                      return $ map read $ lines x


-- take a list of integers
-- and over any possible range
-- convert them to Int's, in the range 0..100
-- lowest item must be 0, highest must be 100
distribute :: [Integer] -> [Int]
distribute xs = if mn == mx
                then replicate (length xs) 100
                else map (round . f) vals
    where
        f x = (x-c) * m
    
        c = mn
        m = 100 / (mx - mn)
    
        vals = (map fromInteger xs) :: [Float]
        mn = fromInteger (minimum xs) :: Float
        mx = fromInteger (maximum xs) :: Float
