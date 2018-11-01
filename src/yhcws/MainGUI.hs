-----------------------------------------------------------------------------
-- |
-- Module      :  MainGUI
-- Copyright   :  (c) Dmitry Golubovsky, 2007       
-- License     :  BSD-style
-- 
-- Maintainer  :  Dmitry Golubovsky <golubovsky@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (needs Yhc Javascript backend)
--
-- YHC Web Service: Main interface to the pastebin browser.
--
-- This program allows to view and change contents of the pastebin. It lists  
-- certain number of entries at a time, showing their submission and compilation
-- status, etc.
--
------------------------------------------------------------------------------

module MainGUI where

import CPS
import UnsafeJS
import YWSWidgets
import Control.Concurrent.JSThreads
import Network.XMLHTTP
import Data.JsonNode
import Data.Maybe
import Data.List

import CDOM.Level2.Events

import DOM.Level2.CSS2Properties

-- Main function.

main = docBodyC mainW

title = "Browse Pastebin Entries"

dbase = "pastebin"

pgsize = 15

-- Viewer/browser control command

data VBRCTL = 
  BROWSE
 |VIEWFLD String (String, String, String)
 |VIEWATT String (String, String, String)
 deriving (Eq)
 

-- Main widget.

mainW = gapP
    +++ maingui

-- Main widget presentation.

maingui :: Widget

maingui = getLocation $ \loc ->
          msgBox $ \lmbx ->
          msgBox $ \vmbx ->
          msgBox $ \lbbx ->
          msgBox $ \vbbx ->
          msgBox $ \vcbx ->
          mkDiv `withClass` "vstrut" |<< active (deMuxA vmbx [vbbx, lbbx, vcbx])
      +++ mkDiv |<< (listbody lmbx vmbx loc +++ active (hideParentA (== BROWSE) True lbbx))
      +++ mkDiv |<< (viewbody vmbx vcbx loc +++ active (hideParentA (/= BROWSE) False vbbx))
      +++ dividerP

deMuxA :: MSGBOX Bool b -> [MSGBOX Bool b] -> ACTF

deMuxA ibx obxs par = recvMsg ibx $ \[m] ->
  broadCastMsg_ m obxs $ \_ -> deMuxA ibx obxs par

hideParentA :: (VBRCTL -> Bool) -> Bool -> MSGBOX Bool VBRCTL -> ACTF

hideParentA cond initv mbx par =
  let ivis = if initv then "block" else "none"
  in  inlineStyle par (set'display ivis) $ \_ ->
      hpa cond mbx par where
        hpa cnd mb p = 
          let vis v = if v then "block" else "none"
          in  recvMsg mb $ \[m] ->
              inlineStyle par (set'display $ vis (cnd m)) $ \_ ->
              hpa cnd mb p

viewbody vmbx vcbx loc = mkDiv |<< active (loadItemA loc vmbx vcbx)


loadItemA :: String -> MSGBOX Bool VBRCTL -> MSGBOX Bool VBRCTL -> ACTF

loadItemA loc vmbx mbx par = recvMsg mbx $ \[m] ->
  let (txtrd, ttl) = case m of
        VIEWFLD t (db, did, fld) -> (loadField loc db did fld, t ++ " [" ++ fld ++ "]")
        VIEWATT t (db, did, att) -> (loadAtt   loc db did att, t ++ " (" ++ att ++ ")")
        _ -> (toCPS [], "")
  in  txtrd $ \txt -> updContainerU par (shellC ttl $ vtxt txt) |>>| 
      loadItemA loc vmbx mbx par where
        vtxt txt = mkDiv `withClass` "viewbody" |<< mkPre |<< textP txt
               +++ dividerP
               +++ blacklnP
               +++ mkDiv `withClass` "centered" |<<
                     (buttonI `withClass` "w20p" |<< 
                       (textP "Close" 
                    +++ active (evtBCastA "click" (evt2ConstU BROWSE) [vmbx]))
               +++ dividerP)

loadField :: String -> String -> String -> String -> CPS Bool String

loadField loc db did fld k = 
  let uri = fromMaybe "" $ do
        buri <- splitURI loc
        prot <- getValueByName "protocol" buri >>= getString
        auth <- getValueByName "authority" buri >>= getString
        let uri = prot ++ "://" ++ auth ++ "/" ++ db ++ "/" ++ did
        return uri
  in  runHTTP (genericGET uri `withHeader` ("Accept", "application/json, *")) $ \rs ->
      let r = case rspCode rs of
            200 -> case rspBody rs of
                     StringBody s -> fromMaybe ("no " ++ fld ++ " field\n") $ do
                       j <- parseJSON s
                       ftx <- getValueByName fld j >>= getString
                       return ftx
                     _ -> "Non-text response"
            _ -> show (rspCode rs) ++ ": " ++ rspReason rs
      in  k r

loadAtt :: String -> String -> String -> String -> CPS Bool String
  
loadAtt loc db did att k = 
  let uri = fromMaybe "" $ do
        buri <- splitURI loc
        prot <- getValueByName "protocol" buri >>= getString
        auth <- getValueByName "authority" buri >>= getString 
        let uri = prot ++ "://" ++ auth ++ "/" ++ db ++ "/" ++ did ++ "/" ++ att
        return uri
  in  runHTTP (genericGET uri) $ \rs ->
      let r = case rspCode rs of
            200 -> case rspBody rs of
                     StringBody s -> s
                     _ -> "Non-text response"
            _ -> show (rspCode rs) ++ ": " ++ rspReason rs
      in  k r



jmpbrowse :: ACTF

jmpbrowse = jmpBrowseA "click" "NewEntry.html"

whspace :: Int -> Widget

whspace n = mkSpan |<< textP (replicate n ' ')

pgctl n mbx = evtBCastA "click" (evt2ConstU n) [mbx]

listbody lmbx vmbx loc = shellC title lb where
  lb = mkDiv `withClass` "listbody" 
         |<< (listhead +++ mkDiv |<< active (dbpager loc lmbx vmbx pgsize 0))
   +++ dividerP
   +++ blacklnP
   +++ mkDiv `withClass` "centered" |<<
         (buttonI `withClass` "w20p" |<< (textP "<< Prev" +++ active (pgctl (-1) lmbx))
      +++ whspace 5
      +++ buttonI `withClass` "w20p" |<< (textP "New Entry" +++ active jmpbrowse)
      +++ whspace 5
      +++ buttonI `withClass` "w20p" |<< (textP "Next >>" +++ active (pgctl 1 lmbx))
      +++ dividerP)



dbpager :: String -> MSGBOX Bool Int -> MSGBOX Bool VBRCTL -> Int -> Int -> ACTF

dbpager loc mbx vmbx ps off par = 
  let [uri] = loc2uri loc ps off
  in  getrows vmbx uri $ \rows ->
      updContainerU par (mkDiv |<< foldr (+++) nowidget rows) |>>|
      recvMsg mbx $ \[msg] ->
      let off' = off + signum msg
          off'' | off' < 0 = 0
                | otherwise = off'
      in  dbpager loc mbx vmbx ps off'' par
  
loc2uri loc ps off = do
  buri <- splitURI loc
  prot <- getValueByName "protocol" buri >>= getString
  auth <- getValueByName "authority" buri >>= getString
  let uri = prot ++ "://" ++ auth ++ "/" ++ dbase ++ "/_temp_view"
                 ++ "?count=" ++ show ps 
                 ++ if off > 0 then "&skip=" ++ show (off * pgsize) else ""
--                 ++ if off > 1 then "&update=true" else ""
  return uri


getrows vmbx uri k = 
  let dblexcl s = s ++ ":!!doc." ++ s ++ "?doc." ++ s ++ ":\"\""
      viewfun = 
        "function(doc) {map(0-doc._id, {" ++
        foldr1 (\a b -> a ++ ", " ++ b) 
               (map dblexcl ["_id", "author", "title", "status", "utctxt"]) ++
        ",_att:!!doc._attachments?doc._attachments:{}});}"
  in  runHTTP (genericPOST uri `withHeader` ("Accept", "application/json, *")
                               `withHeader` ("Content-type", "text/javascript")
                               `withBody` (StringBody viewfun)) $ \rs ->
      case rspCode rs of
        200 -> case rspBody rs of
                 StringBody jrs | rspContType rs == "application/json" ->
                   k $ fromMaybe [] $ decodebody vmbx jrs
                 NullBody -> k [bannerP "Null body was returned"]
                 XHException s -> k [bannerP $ "Exception: " ++ s]
                 XMLBody _ -> k [bannerP $ "XML Document was returned but not expected"]
                 _ -> k [bannerP "Incorrect response body format"]
        _   -> k [bannerP $ show (rspCode rs) ++ ": " ++ rspReason rs]


nrows jrs = fromMaybe "no rows" $ do
  j <- parseJSON jrs
  getValueByName "rows" j >>= getString >>= return

decodebody vmbx jrs = do
  j <- parseJSON jrs
  rows <- getValueByName "rows" j
  values <- getColumn "value" rows
  lks <- mapM (getValueByName "_att") values
  ids <- mapM (getValueByName "_id") values >>= mapM getString
  aus <- mapM (getValueByName "author") values >>= mapM getString
  tts <- mapM (getValueByName "title") values >>= mapM getString
  sts <- mapM (getValueByName "status") values >>= mapM getString
  utc <- mapM (getValueByName "utctxt") values >>= mapM getString
  return $ zipWith6 (listrow vmbx) lks ids aus utc tts sts

listhead = mkDiv `withClass` "h4p centered" |<< (
             mkSpan `withClass` "w15p listhd centered"   |<< textP "links"
         +++ mkSpan `withClass` "w15p listhd leftalign"  |<< textP "author"
         +++ mkSpan `withClass` "w20p listhd centered"   |<< textP "time"
         +++ mkSpan `withClass` "w35p listhd rightalign" |<< textP "title"
         +++ mkSpan `withClass` "w10p listhd rightalign" |<< textP "status"
         +++ dividerP)

listrow vmbx l i au ag t s = mkDiv `withClass` "h4p centered" |<< (
             mkSpan `withClass` "w15p listrow centered"   |<< links vmbx t l i
         +++ mkSpan `withClass` "w15p listrow leftalign"  `withTitle` au |<< textP au
         +++ mkSpan `withClass` "w20p listrow centered"   `withTitle` ag |<< textP ag
         +++ mkSpan `withClass` "w35p listrow rightalign" `withTitle` t |<< textP t
         +++ mkSpan `withClass` "w10p listrow rightalign" |<< textP s
         +++ dividerP)

links :: MSGBOX Bool VBRCTL -> String -> JsonNode -> String -> Widget

links vmbx ttl lks did = 
  let condAtt img att tip = 
        if (hasValueWithName att lks) 
          then mkSpan |<< (imageP img tip +++ active (viewAttA ttl (dbase, did, att) vmbx))
          else nowidget
      condLink img att tip =
        if (hasValueWithName att lks)
          then mkSpan |<< anchorI `withTarget` "_blank"
                                  `withHref` (wpref dbase did att) |<<
                 imageP img tip
          else nowidget
  in  mkSpan |<< (imageP "hssrc.png" "view Haskell source" 
              +++ active (viewFldA ttl (dbase, did, "source")  vmbx))
      +++ mkSpan |<< (imageP "edit.png" "edit source code"
                  +++ active (jmpBrowseA "click" ("NewEntry.html#" ++ dbase ++ "/" ++ did)))
      +++ condAtt "error.png" "error_log" "view error log"
      +++ condAtt "log.png" "compile_log" "view compilation log"
      +++ condLink "webpage.png" "webpage.html" "view compiled page"

wpref dbase did att = "/" ++ dbase ++ "/" ++ did ++ "/" ++ att

viewFldA :: String -> (String, String, String) -> MSGBOX Bool VBRCTL -> ACTF

viewFldA ttl ddf mbx par = waitFor par "click" $ \e ->
  evt2ConstU (VIEWFLD ttl ddf) e $ \m ->
  sendMsg_ mbx m |>>| viewFldA ttl ddf mbx par

viewAttA :: String -> (String, String, String) -> MSGBOX Bool VBRCTL -> ACTF

viewAttA ttl ddf mbx par = waitFor par "click" $ \e ->
  evt2ConstU (VIEWATT ttl ddf) e $ \m ->
  sendMsg_ mbx m |>>| viewAttA ttl ddf mbx par


