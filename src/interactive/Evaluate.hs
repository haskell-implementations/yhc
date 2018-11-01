-- 

module Evaluate where 

import Interact
import System
import Directory
import IO 
import GenUtil

removeFileSafe :: FilePath -> IO ()
removeFileSafe x = do b <- doesFileExist x
                      if b then removeFile x else return ()

readFileStrict :: FilePath -> IO String
readFileStrict x = do h <- openFile x ReadMode 
                      s <- hGetContents h
                      ss <- length s `seq` return s
                      hClose h
                      return ss

evalYhc :: (String -> IO ()) -> (String -> IO ()) -> String -> IO ()
evalYhc out err cmd = do 
    writeFile "_temp.hs" contents
    cres <- system "yhc _temp.hs 2> _temp.err"
    case cres of 
      ExitFailure code -> do errtext <- readFileStrict "_temp.err"
                             err errtext
      ExitSuccess -> do
        eres <- system "yhi YheTempModule > _temp.out 2> _temp.err"
        case eres of
          ExitFailure code -> do errtext <- readFileStrict "_temp.err"
                                 err errtext
          ExitSuccess -> do
            res <- readFileStrict "_temp.out"
            out res
    removeFileSafe "_temp.hi"
    removeFileSafe "_temp.hbc"
    removeFileSafe "_temp.out"
    removeFileSafe "_temp.hs"
    return () 
  where
    -- modules = map (\x->"import "++x++"\n") (interactModules act)
    contents = unlines 
       ["module YheTempModule where"
       ,""
       --,(unlines modules)
       ,"main = putStrLn $ show $ " ++ cmd
       ]

