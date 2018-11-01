---------------------------------------------------------------------------
-- |
-- Module      :  YWSWidgets
-- Copyright   :  (c) Dmitry Golubovsky, 2007       
-- License     :  BSD-style
-- 
-- Maintainer  :  Dmitry Golubovsky <golubovsky@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (needs Yhc Javascript backend)
--
-- YHC Web Service: commonly used widgets. Usage of YHCWS.css is assumed.
--
-------------------------------------------------------------------------------

module YWSWidgets (
  module Graphics.UI.HsWTK
 ,module DOM.Level2.Html2
 ,module DOM.Level2.HTMLDivElement
 ,module DOM.Level2.HTMLSpanElement
 ,module DOM.Level2.HTMLAnchorElement
 ,module DOM.Level2.HTMLPreElement
 ,module CDOM.Level2.DomUtils
 ,shellC
 ,gapP
 ,bannerP
 ,dividerP
 ,blacklnP
 ,anchorI
 ,loadTextA
 ,jmpBrowseA
 ,withHref
 ,withTarget
 ,withRows
 ,withCols
) where

import CPS

import Graphics.UI.HsWTK

import Network.XMLHTTP

import DOM.Level2.Html2
import DOM.Level2.HTMLDivElement
import DOM.Level2.HTMLSpanElement
import DOM.Level2.HTMLAnchorElement
import DOM.Level2.HTMLTextAreaElement
import CDOM.Level2.DomUtils
import CDOM.Level2.Events
import DOM.Level2.HTMLPreElement

-- |Display a "shell" a frame with a titlebar, holding elements
-- of a web application inside.

shellC :: String -> Widget -> Widget

shellC ns w = mkDiv `withClass` "shell" |<<
       (mkDiv `withClass` "title" `withTitle` ns |<< textP ns +++
        mkDiv `withClass` "client" |<< w)

-- |Produce a horizontal gap.

gapP :: Widget

gapP = passive (mkDiv `withClass` "gap")

-- |Produce a text banner showing a given string.

bannerP :: String -> Widget

bannerP s = mkDiv `withClass` "banner" |<< textP s

-- |Produce a divider (DIV with clear:both style and zero height).

dividerP :: Widget

dividerP = passive (mkDiv `withClass` "divider")

-- |Produce a black line divider (DIV with clear:both style and 1px height).

blacklnP :: Widget

blacklnP = passive (mkDiv `withClass` "divider blackln")

-- |Anchor element (<A>)

anchorI :: THTMLDocument -> CPS a THTMLAnchorElement

anchorI = mkA

-- |Decorator for an Anchor element

withHref :: (CHTMLAnchorElement b) => ((a -> ((b -> (CPS c b)) -> d)) -> (String -> (a -> d)))

withHref = decorate set'href

-- |Decorator for an Anchor element

withTarget :: (CHTMLAnchorElement b) => ((a -> ((b -> (CPS c b)) -> d)) -> (String -> (a -> d)))

withTarget = decorate set'target

 

-- |An one-shot activator pattern which loads a text from the given URI into
-- the parent node.

loadTextA :: String -> ACTF

loadTextA uri par =  
  runHTTP (genericGET uri `withHeader` ("Accept", "application/json, *")) $ \rs ->
  let r = case rspCode rs of
        200 -> case rspBody rs of
                 StringBody s -> s
                 _ -> "Non-text response"
        _ -> show (rspCode rs) ++ ": " ++ rspReason rs
  in  updateU par r $ const True

-- |Jump browser to the location pointed to by the given URI string upon a given event.

jmpBrowseA :: String -- ^event type (e. g. \"click\")
           -> String -- ^new location to jump to
           -> ACTF   -- ^this function does not really return

jmpBrowseA evt loc par = 
  waitFor par evt $ \e ->
  evt2ConstU 0 e $ \_ ->
  jumpLocation loc

-- |Decorator for a TEXTAREA element: number of columns

withCols :: (CHTMLTextAreaElement b) => ((a -> ((b -> (CPS c b)) -> d)) -> (Int -> (a -> d)))

withCols = decorate set'cols

-- |Decorator for a TEXTAREA element: number of rows

withRows :: (CHTMLTextAreaElement b) => ((a -> ((b -> (CPS c b)) -> d)) -> (Int -> (a -> d)))

withRows = decorate set'rows




