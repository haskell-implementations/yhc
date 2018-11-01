-- CouchDB client program in Haskell

module CouchTest where

import CPS
import UnsafeJS

import DOM.Level2.XMLHTTP
import DOM.Level2.XMLHttpRequest

import CDOM.Level2.DomUtils
import CDOM.Level2.Events
import Control.Concurrent.JSThreads
import DOM.Level2.Dom
import DOM.Level2.Node
import DOM.Level2.Events
import DOM.Level2.KeyEvent
import DOM.Level2.Html2
import DOM.Level2.HTMLDivElement
import DOM.Level2.HTMLSpanElement
import qualified DOM.Level2.HTMLInputElement as I
import Graphics.UI.HsWTK

shellC :: String -> Widget -> Widget

shellC ns w = mkDiv `withClass` "shell" |<<
       (mkDiv `withClass` "title" `withTitle` ns |<< textP ns +++
        mkDiv `withClass` "client" |<< w)

gapP = passive (mkDiv `withClass` "gap")

inputI c = container I.mkInput c

main = docBodyC mainW

mainW = gapP
    +++ shellC "CouchDB Test" cdbtest

cdbtest = msgBox $ \mbx ->
          mkDiv `withClass` "banner" |<<
            textP "Type a word, see the value retrieved via XMLHTTP"
     +++  mkSpan `withClass` "btn" |<< inputI << active (acti mbx)
     +++  mkSpan `withClass` "display" |<< stringD "0" << active (actd mbx)
     ++|  mkDiv `withClass` "divider"

acti mb par = (waitFor par "keydown" :: CPS Bool TKeyEvent) $ \e ->
  get'keyCode e $ \c ->
  let cont z | z == cDOM_VK_ENTER = 
               (unsafeToSelf par :: CPS Bool THTMLInputElement) $ \sel ->
               I.get'value sel $ \v -> 
               sendMsg_ mb v |>>| 
               I.set'value "" sel |>>| acti mb par
             | otherwise = acti mb par
  in cont c

actd mb par = recvMsg mb $ \[s] ->
  httpGet s $ \rs ->
  updateD par rs $ \_ ->
  actd mb par

updateD par s = 
  (get'ownerDocument par :: CPS z THTMLDocument) $ \doc ->
  (get'firstChild par :: CPS y TNode) $ \old ->
  mkText doc s $ \new ->
  (replaceChild par new old :: CPS x TNode)

mkXMLHttpRequest :: CPS z TXMLHttpRequest

mkXMLHttpRequest k = k $! (mkXMLHttpRequest' 0) where
  mkXMLHttpRequest' a = unsafeJS "return newXmlHttpRequest();"

sendNull :: (CXMLHttpRequest this) => this -> CPS c ()
sendNull a = toCPE (sendNull' a)
sendNull' a
  = unsafeJS "return((exprEval(a)).send(null));"


httpGet :: String -> CPS Bool String

httpGet url k = msgBox $ \hmb ->
  forkThread (xthr hmb) $
  recvMsg hmb $ \[s] -> k s where
  xthr mb = mkXMLHttpRequest $ \hrq ->
            setEventHandler "readystatechange" (wt hrq mb :: TEvent -> Bool) hrq $ \_ ->
            openAsync hrq "GET" url True $ \_ -> 
            sendNull hrq $ const True where
              wt x mb e =
                get'readyState x $ \st ->
                case st of
                  4 -> get'responseText x $ \rsp -> sendMsg_ mb rsp $ const True
                  _ -> True

