-----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Dmitry Golubovsky, 2007       
-- License     :  BSD-style
-- 
-- Maintainer  :  Dmitry Golubovsky <golubovsky@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- YHC Web Service: Initialize the databases
--
-------------------------------------------------------------------------------

module Main where

import Data.Maybe
import Database.CouchDB
import Text.JSON
import Control.Monad
import System.Environment
import System.FilePath
import System.Posix.Files
import System.Exit
import System.IO
import Network.URI

dbases = ["inspool", "pastebin", "examples", "static"]

main = do
  prog <- getProgName
  args <- getArgs
  when (null args) $ do
    hPutStrLn stderr $ "Usage: " ++ prog ++ " couchdb_uri"
    exitWith (ExitFailure 2)
  let puris = fromMaybe [] $ do
        baseuri <- parseAbsoluteURI (head args)
        pu <- mapM (\pb -> parseURIReference ("/" ++ pb ++ "/")) dbases
        mapM (flip relativeTo baseuri) pu
  when (null puris) $ do
    hPutStrLn stderr $ prog ++ ": could not construct URI for databases"
    exitWith (ExitFailure 3)
  hPutStr stdout $ "WARNING!!! " ++ prog ++ " will initialize the CouchDB instance " ++
                   "running at " ++ head args ++ ".\n" ++
                   "Databases: " ++ show dbases ++ " will be emptied\n" ++
                   "Proceed (y/n)? "
  hFlush stdout
  rsp <- hGetLine stdin
  when ((null rsp) || ((head rsp) `notElem` ['y', 'Y'])) $ exitWith ExitSuccess
  mapM_ initdb puris

initdb :: URI -> IO ()

initdb uri = do
  let prtRes rsp = 
        if (hasValueWithName "ok" rsp) 
          then hPutStrLn stdout "OK"
          else if (hasValueWithName "error" rsp) 
                 then do err <- getValueByName "error" rsp >>= getString
                         hPutStrLn stdout err
                 else hPutStrLn stdout $ show rsp
  hPutStrLn stdout $ "Initializing database at " ++ show uri
  hPutStr stdout "Deleting... "
  rsp <- cDBDel uri "" >>= return . rsp2CDB >>= cdb2JSON
  prtRes rsp
  hPutStr stdout "Creating empty... "
  rsp <- cDBPutStr uri "" >>= return . rsp2CDB >>= cdb2JSON
  prtRes rsp


