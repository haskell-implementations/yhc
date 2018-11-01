-----------------------------------------------------------------------------
-- |
-- Module      :  NewEntry
-- Copyright   :  (c) Dmitry Golubovsky, 2007       
-- License     :  BSD-style
-- 
-- Maintainer  :  Dmitry Golubovsky <golubovsky@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (needs Yhc Javascript backend)
--
-- YHC Web Service: Application to create a new codebase entry.
--
-- This program displays a web page which allows an user to create
-- a new entry in the codebase. The codebase is located under the
-- "code" database. Entries are documents (initially empty)
-- with server-generated identifiers. Upon user's action, such
-- document will be generated, and its URI will be presented to user
-- for entry access and bookmarking.
--
-------------------------------------------------------------------------------

module NewEntry where

import CPS
import UnsafeJS
import YWSWidgets
import Control.Concurrent.JSThreads
import Network.XMLHTTP
import Data.JsonNode
import Data.Maybe

import Control.Monad
import CDOM.Level2.Events

-- Main function.

main = docBodyC mainW

title = "Create a New Pastebin Entry"

-- Main widget.

mainW = gapP
    +++ shellC title newentry

data PBEntry = PBEntry {
  pbeTitle :: String
 ,pbeAuthor :: String
 ,pbeSource :: String
} deriving (Show)

ifEmpty Nothing x = Just x
ifEmpty (Just "") x = Just x
ifEmpty (Just z)  x = Just z

-- Main widget presentation.

newentry :: Widget

newentry = 
  getLocation $ \loc ->
  msgBox $ \dummy ->
  msgBox $ \ttlin ->
  msgBox $ \autin ->
  msgBox $ \srcin ->
  let  mbs = [ttlin, autin, srcin]
       clrall = evtBCastA "click" (evt2ConstU $ FwdUpdSet "") mbs
       jmpbrowse = jmpBrowseA "click" "MainGUI.html"
       enter par = waitFor par "click" $ \e -> 
         evt2ConstU (FwdUpdSet "") e $ \fwups ->
         askValueU ttlin $ \mbttl ->
         askValueU autin $ \mbaut ->
         askValueU srcin $ \mbsrc ->
         broadCastMsg_ fwups mbs $ \_ ->
         let obj = return PBEntry `ap` (ifEmpty mbttl "No title") 
                                  `ap` (ifEmpty mbaut "Unknown")
                                  `ap` mbsrc
         in  case obj of
               Nothing -> enter par
               Just o -> writeObj loc o $ \s ->
                         sendMsg_ srcin (FwdUpdSet s) $ \_ -> enter par
       loadobj loc ttlin autin srcin par =
         let uri = do
               buri <- splitURI loc
               prot <- getValueByName "protocol" buri >>= getString
               auth <- getValueByName "authority" buri >>= getString
               anchor <- getValueByName "anchor" buri >>= getString
               let uri = prot ++ "://" ++ auth ++ "/" ++ anchor
               return uri
         in  case uri of
               Nothing -> True 
               Just du -> loadObj du $ \pbe ->
                 sendMsg_ ttlin (FwdUpdSet $ pbeTitle pbe) $ \_ ->
                 sendMsg_ autin (FwdUpdSet $ pbeAuthor pbe) $ \_ ->
                 sendMsg_ srcin (FwdUpdSet $ pbeSource pbe) $ const True
  in   gapP
   +++ lblinC "Title:"  << active (fwdValueA ttlin dummy)
   +++ dividerP
   +++ gapP
   +++ lblinC "Author:" << active (fwdValueA autin dummy)
   +++ dividerP
   +++ gapP
   +++ labelP "Source:" 
   +++ txtAreaI `withClass` "w85p entry" `withRows` 20 
                       |<< active (fwdValueA srcin dummy)
   +++ dividerP
   +++ gapP
   +++ blacklnP
   +++ gapP
   +++ mkDiv |<< active (loadobj loc ttlin autin srcin)
   +++ mkDiv `withClass` "centered" |<<
       (buttonI `withClass` "w20p" 
            |<< (textP "Submit" +++ active enter)
      +++ hspace
      +++ buttonI `withClass` "w20p" 
            |<< (textP "Browse" +++ active jmpbrowse)
      +++ hspace
      +++ buttonI `withClass` "w20p" 
            |<< (textP "Clear All" +++ active clrall)
      +++ dividerP
      +++ gapP)
                  
-- Items repeatedly used.

labelP :: String -> Widget

labelP s = mkSpan `withClass` "w10p label" |<< textP s

lblinC :: String -> Widget -> Widget

lblinC s w = labelP s +++ inputI `withClass` "w85p entry" |<< w

hspace :: Widget

hspace = mkSpan |<< textP " "

-- Load a document from the storage.

loadObj :: String -> CPS Bool PBEntry

loadObj uri k = 
  runHTTP (genericGET  uri `withHeader` ("Accept", "application/json, *")) $ \rs ->
  let empty = PBEntry {pbeTitle = "", pbeAuthor = "", pbeSource = ""}
      damaged = empty {pbeSource = ("-- Document at " ++ 
                                        uri ++ 
                                    " is damaged: one or more fields missing")}
      nontext = empty {pbeSource = "-- Non-text response from XML HTTP layer"}
      errcode rs = empty {pbeSource = "-- " ++ show (rspCode rs) ++ ": " ++ rspReason rs}
      r = case rspCode rs of
        200 -> case rspBody rs of
                 StringBody s -> fromMaybe damaged $ do
                   j <- parseJSON s
                   ttl <- getValueByName "title" j >>= getString
                   aut <- getValueByName "author" j >>= getString
                   src <- getValueByName "source" j >>= getString
                   return PBEntry `ap` (Just ttl) `ap` (Just aut) `ap` (Just src)
                 _ -> nontext
        _ -> errcode rs
  in  k r

-- Write an object to the storage. Entries are POSTed into the /inspool
-- database.

writeObj :: String -> PBEntry -> CPS Bool String

writeObj loc pbe k =
  let jpbe = withNewJSON $ \nj -> do
        buri <- splitURI loc
        prot <- getValueByName "protocol" buri >>= getString
        auth <- getValueByName "authority" buri >>= getString
        let uri = prot ++ "://" ++ auth ++ "/inspool"
        js <- setString "title" (pbeTitle pbe) nj >>=
              setString "author" (pbeAuthor pbe) >>=
              setString "source" (pbeSource pbe) >>=
              toJSONString
        return (js, uri)
  in  case jpbe of
        Nothing -> k "-- * ERROR: Could not derive inspool database URI"
        Just (js, uri) ->
          runHTTP (genericPOST uri `withHeader` ("Accept", "application/json, *")
                                   `withHeader` ("Content-type", "application/json")
                                   `withBody` (StringBody js)) $ \rs ->
          case rspCode rs of
            201 -> case rspBody rs of
                     StringBody jrs | rspContType rs == "application/json" ->
                       k $ fromMaybe "-- *ERROR: incorrect JSON response" $ do
                         j <- parseJSON jrs
                         ok <- getValueByName "ok" j >>= getBool
                         did <- getValueByName "id" j >>= getString
                         return $ "-- Posted new document ID = " ++ did
                                    | otherwise ->
                       k $ "-- *ERROR: incorrect content type: " ++ rspContType rs
                     _ -> k $ "-- *ERROR: incorrect response body"
            _ -> k $ "-- *ERROR: " ++ show (rspCode rs) ++ " " ++ rspReason rs ++
                     "\n" ++ showBody (rspBody rs)


showBody (XHException s) = s
showBody _ = ""
