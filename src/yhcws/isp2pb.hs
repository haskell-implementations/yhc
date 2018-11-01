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
-- YHC Web Service: Move a document from inspool to pastebin
--
-- This program polls the "inspool" database for new documents.
-- It grabs one document at a time, counts number of documents
-- in the "pastebin" database, and creates new entry in the pastebin
-- deleting the entry in the inspool.
--
-----------------------------------------------------------------------------

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
import Data.Time
import System.Locale

import Text.PrettyPrint.HughesPJ

isp = "inspool"

pb = "pastebin"

main = do
  prog <- getProgName
  args <- getArgs
  when (length args == 0) $ do
    hPutStrLn stderr $ "Usage: " ++ prog ++ " couchdb_uri"
    exitWith (ExitFailure 2)
  let (iuri, puri) = fromMaybe (nullURI, nullURI) $ do
        baseuri <- parseAbsoluteURI (head args)
        ispuri <- parseURIReference ("/" ++ isp ++ "/")
        pburi <- parseURIReference ("/" ++ pb ++ "/")
        iu <- ispuri `relativeTo` baseuri
        pu <- pburi `relativeTo` baseuri
        return (iu, pu)
  mapM (\(s, u) -> when (u == nullURI) $ do
    hPutStrLn stderr $ prog ++ ": could not parse the database uri " ++ s
    exitWith (ExitFailure 3)) $ zip [head args ++ "/" ++ isp, head args ++ "/" ++ pb] 
                                    [iuri, puri]
  ijsn <- cDBGet iuri >>= return . rsp2CDB >>= cdb2JSON
  dbname <- getValueByName "db_name" ijsn >>= getString
  when (dbname /= isp) $ do
    hPutStrLn stderr $ prog ++ ": database name " ++ dbname ++ " was not expected for inspool"
    exitWith (ExitFailure 4)
  idcnt <- getValueByName "doc_count" ijsn >>= getDouble >>= return . floor
  when (idcnt == 0) $ exitWith (ExitFailure 99)
  let onedoc = dbListCountDocsURI 1 iuri
  when (isNothing onedoc) $ do
    hPutStrLn stderr $ prog ++ ": could not build URI for documents list"
    exitWith (ExitFailure 5)
  od <- cDBGet (fromJust onedoc) >>= return . rsp2CDB >>= cdb2JSON
  dids <- getValueByName "rows" od >>= getColumn "id" >>= mapM getString
  when (length dids == 0) $ exitWith ExitSuccess
  pjsn <- cDBGet puri >>= return . rsp2CDB >>= cdb2JSON
  dbname <- getValueByName "db_name" pjsn >>= getString
  when (dbname /= pb) $ do
    hPutStrLn stderr $ prog ++ ": database name " ++ dbname ++ " was not expected for pastebin"
    exitWith (ExitFailure 6)
  let tvrq = parseRelativeReference "?count=1"
  maxpdid <- cDBTempView puri tvrq "function(doc) { map(-doc._id, doc);}"   
           >>= return . rsp2CDB >>= cdb2JSON
           >>= getValueByName "rows" >>= getColumn "id" >>= mapM getString
  let pdidstr | maxpdid == [] = "0"
              | otherwise = head maxpdid
      pbid = show $ ((read pdidstr) :: Int) + 1
      idocuri = dbDocURI iuri (head dids)
      pdocuri = dbDocURI puri pbid
  when (any isNothing [idocuri, pdocuri]) $ do
    hPutStrLn stderr $ prog ++ ": could not build URI for document"
    exitWith (ExitFailure 7)
  utc <- getCurrentTime
  idoc <- cDBGet (fromJust idocuri) >>= return . rsp2CDB >>= cdb2JSON
  idocrev <- getValueByName "_rev" idoc >>= getString
  tz <- getCurrentTimeZone
  let udiff = diffUTCTime utc (UTCTime (ModifiedJulianDay 0) 0)
      ltm = formatTime defaultTimeLocale "%c" utc
      idoc' = idoc `delNode` "_id" `delNode` "_rev" 
                   `mergeL` ("ispid", String $ head dids)
                   `mergeL` ("utcnum", (Number . fromRational . toRational) udiff)
                   `mergeL` ("utctxt", String ltm)
                   `mergeL` ("status", String "New")
  let tvrq = parseRelativeReference "?count=1"
  rsp <- cDBPutJSON (fromJust pdocuri) idoc' >>= return . rsp2CDB >>= cdb2JSON
  when (hasValueWithName "error" rsp) $ do
    hPutStrLn stderr $ prog 
                    ++ ": could not store document in pastebin, name conflict, retry later"
    exitWith (ExitFailure 127)
  rdl <- cDBDel (fromJust idocuri) idocrev >>= return . rsp2CDB >>= cdb2JSON
  when (hasValueWithName "error" rdl) $ do
    hPutStrLn stderr $ prog 
                    ++ ": could not delete document in inspool"
    exitWith (ExitFailure 8)
  exitWith ExitSuccess


