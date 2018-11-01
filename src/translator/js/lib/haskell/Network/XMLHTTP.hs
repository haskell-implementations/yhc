-----------------------------------------------------------------------------
-- |
-- Module      :  Network.XMLHTTP
-- Copyright   :  (c) Dmitry Golubovsky, 2007       
-- License     :  BSD-style
-- 
-- Maintainer  :  Dmitry Golubovsky <golubovsky@gmail.com>
-- Stability   :  experimental
-- Portability :  Non-portable (requires Yhc/Javascript backend)
--
-- Support functions for XML HTTP protocol
--
-----------------------------------------------------------------------------

module Network.XMLHTTP (
  module DOM.Level2.XMLHttpRequest
  -- $xmlhttp
  -- * Bare XML HTTP Level
 ,mkXMLHttpRequest
 ,sendNull
 ,splitHeaders
  -- * Data Structures
 ,XHRBody  (..)
 ,XHRequest (..)
 ,XHResponse (..)
  -- * Generic requests
 ,genericGET
 ,genericPOST
 ,withHeader
 ,withBody
  -- * Perform HTTP action
 ,runHTTP
  -- * Split an URI into parts.
 ,splitURI
  -- * Extract a JSON node out of the response.
 ,rsp2JSON
) where 

import CPS
import UnsafeJS
import Data.Char
import Data.JsonNode
import DOM.Level2.Dom
import DOM.Level2.XMLHTTP
import DOM.Level2.XMLHttpRequest
import DOM.Level2.Events
import CDOM.Level2.Events
import Control.Concurrent.JSThreads
import qualified Data.Map as M

{- $xmlhttp
This module contains generic support functions to access the XMLHTTP facility
provided by modern web browsers, in addition to automatically compiled module
"DOM.Level2.XMLHttpRequest".

In order to perform a XML HTTP operation, a pseudo-thread is created, It instantiates
a XML HTTP request object, binds an event handler to it, and sends information
to the server. When the request object notifies of operation completion,
the information received is sent back to the requesting thread via message box.
-}

-- |Instantiate a XML HTTP request object.

mkXMLHttpRequest :: CPS z TXMLHttpRequest
    
mkXMLHttpRequest k = k $! (mkXMLHttpRequest' 0) where
  mkXMLHttpRequest' a = unsafeJS "return newXmlHttpRequest();"

-- |Send a @null@ Javascript value. The 'sendString' method provided by the
-- "DOM.Level2.XMLHttpRequest" module would not work in some browsers if
-- an empty string is to be sent (like with the @GET@ request). An empty string
-- (@\"\"@) when marshalled to Javascript to Haskell becomes an empty list,
-- that is, a Javascript object different from String. This causes error in
-- Microsoft Internet Explorer, while Firefox treats this normally.
-- So, additional function is provided, which sends a @null@ value which is
-- also acceptable with @GET@ request.

sendNull :: (CXMLHttpRequest this) => this -> CPS c ()
sendNull a = toCPE (sendNull' a) where
  sendNull' a = unsafeJS "return((exprEval(a)).send(null));"

-- |Retrieve response headers separated with newlines and convert
-- into a list of tuples.

splitHeaders :: XHResponse -> [(String, String)]

splitHeaders rsp = map (flip shs "") (lines $ rspHeaders rsp) where
  shs "" r = (reverse r, "")
  shs (':':zz) l = (reverse l, dropWhile isSpace zz)
  shs (z:zz) l = shs zz (z:l)

-- |Possible variants of a request\/response body

data XHRBody = NullBody            -- ^empty body (as in "GET")
             | StringBody String   -- ^send a String
             | XMLBody TDocument   -- ^send XML
             | XHException String  -- ^exception occurred

-- |Request structure to perform a XML HTTP operation. Modeled after
-- GHC\'s Network.HTTP module.

data XHRequest =
     XHRequest { rqURI :: String                  -- ^an URI to retrieve
                ,rqMethod :: String               -- ^method: "GET", "POST" etc.
                ,rqHeaders :: M.Map String String -- ^headers to insert
                ,rqPreferText :: Bool             -- ^prefer responseText even if responseXML
                                                  -- also available
                ,rqBody :: XHRBody                -- ^request body
               }

-- |Response structure to hold results of the XML HTTP operation
-- while being able to release the request itself.

data XHResponse = 
     XHResponse { rspCode :: Int        -- ^from 'get\'Status'
                 ,rspReason :: String   -- ^from 'get\'statusText'
                 ,rspContType :: String -- ^separately extracted \"Content-type\" header
                 ,rspHeaders :: String  -- ^from 'getAllResponseHeaders': all headers
                                        -- separated by newlines. Use 'splitHeaders'
                                        -- to obtain the list of tuples which in turn
                                        -- may be converted into a 'M.Map'.
                 ,rspBody :: XHRBody    -- ^response body, same variants as request body has
                }

emptyRequest = XHRequest { rqURI = ""
                          ,rqMethod = ""
                          ,rqHeaders = M.empty
                          ,rqPreferText = True
                          ,rqBody = NullBody
                         }

-- |Generic GET request.

genericGET :: String    -- ^URI to retrieve
           -> XHRequest

genericGET s = emptyRequest { rqURI = s
                             ,rqMethod = "GET"
                            }

-- |Generic POST request.

genericPOST :: String    -- ^URI to post to
            -> XHRequest

genericPOST s = emptyRequest { rqURI = s
                              ,rqMethod = "POST"
                             }


-- |Insert a HTTP header into a request

withHeader :: XHRequest -> (String, String) -> XHRequest

withHeader xhr (hn, hv) = xhr {rqHeaders = M.insert hn hv (rqHeaders xhr)}

-- |Specify a body for a request (POST, PUT, etc.)

withBody :: XHRequest -> XHRBody -> XHRequest

withBody xhr bd = bd `seq` xhr {rqBody = bd}

-- |Actually start a XML HTTP request

runHTTP :: XHRequest -> CPS Bool XHResponse

runHTTP rrq k = msgBox $ \hmb ->
  forkThread (catchJS (http hmb rrq) (exBody hmb)) $
  recvMsg hmb $ \[rsp] -> k rsp where
    exBody mb e = let r = XHResponse {
                            rspCode = -1
                           ,rspReason = "XML HTTP Exception"
                           ,rspHeaders = []
                           ,rspContType = ""
                           ,rspBody = XHException (unsafeToString e id)
                          }
                  in  sendMsg_ mb r $ const True
    http mb rq = let onehdr r (h, v) = setRequestHeader r h v $ const True
                     sethdrs r hvs = toCPE (all id $ map (onehdr r) hvs)
                     t = rqPreferText rq
                 in  mkXMLHttpRequest $ \hrq ->
                     openAsync hrq (rqMethod rq) (rqURI rq) True $ \_ ->
                     sethdrs hrq (M.toList $ rqHeaders rq) $ \_ ->
                     setEventHandler "readystatechange" 
                                     (w t hrq mb :: TEvent -> Bool) hrq $ \_ ->
                     case rqBody rq of
                       NullBody -> sendNull hrq $ const True
                       StringBody "" -> sendNull hrq $ const True
                       StringBody s -> sendString hrq s $ const True
                       XMLBody x -> sendDocument hrq x $ const True
                     where w t x m e = get'readyState x $ \st ->
                                     case st of
                                       4 -> let r = XHResponse { 
                                                      rspCode = get'status x id
                                                     ,rspReason = get'statusText x id
                                                     ,rspHeaders = getAllResponseHeaders x id
                                                     ,rspContType = getResponseHeader x
                                                                      "Content-Type" id
                                                     ,rspBody = if t then getText x
                                                                     else getBody x
                                                    }
                                            in  sendMsg_ mb r $ const True
                                       _ -> True
                           getText x = unsafeCheckProperty "responseText" x $ \b ->
                             if b then get'responseText x StringBody
                                  else getBody x
                           getBody x = unsafeCheckProperty "responseXML" x $ \b1 ->
                             if b1 then get'responseXML x XMLBody
                                   else unsafeCheckProperty "responseText" x $ \b2 ->
                                     if b2 then get'responseText x StringBody
                                           else NullBody
                 
-- |Parse an URI into a JSON node. This is a binding to this URI splitter:
-- <http://blog.stevenlevithan.com/archives/parseuri>

splitURI :: (Monad m)     --  monadic context is just for convenience; failure never occurs
            => String     -- ^source URI
            -> m JsonNode -- ^JSON object containing parts.

splitURI uri = return $ split uri where
  split a = unsafeJS "return parseUri(exprEval(a));"

-- |If a HTTP operation response contains a JSON node, return it otherwise fail.
-- JSON parser to be invoked will pass error condition, so failure will never be
-- caused by HTTP operation failure.

rsp2JSON :: (Monad m)
         => XHResponse  -- ^XML HTTP operation response
         -> m JsonNode  -- ^JSON object retrieved from the response

rsp2JSON rsp | rspContType rsp /= "application/json" = fail "incorrect content-type"
             | otherwise = case rspBody rsp of
                 StringBody s -> parseJSONErr s
                 _ -> fail "incorrect body type"

