
module Main where

import System
import List
import Char


main = do (x:_) <- getArgs
          y <- readFile x
          writeFile (x ++ ".htm") (process y)
          


type Date = (Int, Int, Int) -- year, month, day


type Chart = [(Int, Int)]


process :: String -> String
process x = drawChart chart
    where
        chart = map g items
        items = accumCount 0 $ map f $ group $ sort $ map getDate $ filter (not.isBlank) $ lines x
        
        isBlank "" = True
        isBlank (' ':_) = True
        isBlank x = False
        
        f xs = (length xs, head xs)
        minv = snd $ head items
        maxv = snd $ last items
        maxc = fst $ last items
        
        accumCount n [] = []
        accumCount n ((a,b):c) = (a+n,getValue b) : accumCount (a+n) c
        
        g (count, val) = (x,y)
            where
                x = ((val - minv) * 300) `div` (maxv - minv)
                y = ((maxc - count) * 150) `div` maxc
                
getDate :: String -> Date
getDate x = (read (xs !! 7), getMonth (xs !! 1), read (xs !! 2))
    where xs = words x


months = ["jan","feb","mar","apr","may","jun","jul","aug","sep","oct","nov","dec"]

getMonth x = case elemIndex (map toLower x) months of
                    Nothing -> error $ "Unrecognised month: " ++ x
                    Just x -> x

getDays x | x == 1 = 29
          | x `elem` [0,2,4,6,7,9,11] = 31
          | otherwise = 30


getValue :: Date -> Int
getValue (year,month,day) = (year*366) + f (month-1) + day
    where
        f (-1) = 0
        f n = getDays n + f (n-1)



drawChart :: Chart -> String
drawChart x = showChart res
    where
        res = allX $ floatY $ nubX x -- allX $ floatY $ nubX x
    
        maxHeight = 150
    
        showChart xs = prefix ++ unlines (map f xs) ++ suffix
        f (x,y) = concat $ div1 : [div2 | fromInteger yfloor /= y]
            where
                div1 = "<div style=\"position:absolute;background-color:blue;" ++
                    "height:" ++ show (maxHeight - yfloor) ++ "px;" ++
                    "width:1px;" ++
                    "left:" ++ show x ++ "px;" ++
                    "top:" ++ show yfloor ++ "px;" ++
                    "\">&nbsp;</div>"
                div2 = "<div style=\"position:absolute;background-color:" ++ color ++ ";" ++
                    "height:1px;" ++
                    "width:1px;" ++
                    "left:" ++ show x ++ "px;" ++
                    "top:" ++ show (yfloor-1) ++ "px;" ++
                    "\">&nbsp;</div>"
            
                color = "rgb(" ++ perc ++ "%," ++ perc ++ "%,100%)"
                perc = show (100 - (100 * (fromInteger yfloor - y)))
                yfloor = toInteger (ceiling y)

        nubX = map head . groupBy (\a b -> fst a == fst b)
        
        floatY = map (\(a,b) -> (a, toFloat b))
        toFloat x = (fromInteger (toInteger x)) :: Float
        
        allX ((x1,y1):(x2,y2):res) = (x1,y1) : [(x,y2 + (diff * toFloat (x-x2))) | x <- [x1+1..x2-1]] ++ allX ((x2,y2):res)
            where diff = (y1-y2)/(toFloat (x1-x2))
        allX x = x


prefix = "<html><head><title>Darcs Statistics for Yhc</title></head><body><h1 style=\"padding-bottom:25px;\">Darcs Statistics for Yhc</h1>\n" ++
         "<div style=\"position:absolute;border:1px solid red;width:300px;height:150px;\">\n" 

suffix = "</div>\n</html>\n"
