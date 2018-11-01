-----------------------------------------------------------------------------
-- |
-- Module      :  CDOM.Level2.DomUtils
-- Copyright   :  (c) Dmitry Golubovsky, 2007       
-- License     :  BSD-style
-- 
-- Maintainer  :  Dmitry Golubovsky <golubovsky@gmail.com>
-- Stability   :  stable
-- Portability :  non-portable (needs Yhc Javascript backend)
--
-- Hand-written utilities to operate on DOM objects
--
-----------------------------------------------------------------------------


module CDOM.Level2.DomUtils (
-- $domutils
-- *Access to Global DOM Elements
   getLocation
 , jumpLocation
 , getDocument
 , getHTMLDocument
 , documentBody
 , documentHead
-- *Type-constrained Manipulation with DOM Objects
 , insertChild
 , addChild
 , mkText
 , inlineStyle
 , inlineStyleDecl
 , parentElement
 , targetElement
-- *Special Constants
 , nodeNothing
-- *Casts
 , eventTarget
 , elementNode
 , htmlElement
) where

import DOM.Level2.Dom
import DOM.Level2.Document
import DOM.Level2.Html2
import DOM.Level2.HTMLDocument
import DOM.Level2.Node
import DOM.Level2.Events
import DOM.Level2.Css
import UnsafeJS
import CPS

{- $domutils
Although most of DOM access functions are auto-generated from Web Consortium\'s
IDL specifications, some useful functions need to be hand-written, just for the sake
of convenience. Many DOM access functions have type constraints on their return value
specified as class instance rather than a type. This, while adds a lot of flexibility
in some cases, also leads to inconvenience of writing explicit type signatures
in other situations. Thus, some type-constrained versions of these functions may be
introduced, which have convenient type signatures, sometimes better order of arguments,
etc.
-}

-- |Access the root Document: same as if /window.document/ were accessed from
-- bare Javascript.

getDocument :: CPS b TDocument

getDocument = length "zz" `seq` toCPE getDocument'
getDocument' = unsafeJS "return window.document;"

-- |Access the current location: same as if /window.location/ were accessed from
-- bare Javascript.

getLocation :: CPS b String   

getLocation c = c $! (getLocation' 0)
getLocation' a = unsafeJS "return new String(window.location);"

-- |Jump to another location. This function does not return.

jumpLocation :: String -> Bool

jumpLocation a = unsafeJS "window.location.href=String(exprEval(a)); return true";

-- |Same as 'getDocument', but return value is cast to the 'THTMLDocument'
-- type rather than 'TDocument'. This is more convenient when implementing
-- HTML-oriented algorithms.

getHTMLDocument :: CPS b THTMLDocument

getHTMLDocument = length "zz" `seq` toCPE getHTMLDocument'
getHTMLDocument' = unsafeJS "return window.document;"

-- |Add a child node to the end of the parent node\'s children list.

addChild :: (CNode newChild, CNode parent) 
         => newChild -- ^new child node to add
         -> parent -- ^parent node
         -> CPS b parent -- ^the function returns the same parent node reference.
                         -- This allows to chain calls to 'addChild' if multiple
                         -- children are to be added:
                         -- @... (addChild c1) (addChild c2) (...@ etc.

addChild child parent k = 
  (appendChild parent child :: CPS b TNode) $ \p -> k parent

-- |Insert a new child node before the given child node.

insertChild :: (CNode refChild, CNode newChild, CNode parent) 
            => refChild -- ^ existing child: insert new child before it
            -> newChild -- ^new child node to add
            -> parent -- ^parent node
            -> CPS b parent -- ^the function returns the same parent node reference.
                            -- This allows to chain calls to 'insertChild' if multiple
                            -- children are to be inserted:
                            -- @... (insertChild r1 c1) (insertChild r2 c2) (...@ etc.

insertChild before child parent k = 
  (insertBefore parent child before :: CPS b TNode) $ \p -> k parent

-- |Create a text node. This is necessary to insert any text inside
-- @<DIV>@ or any other HTML element. The return value is cast to
-- 'TText'. Since 'TText' is an instance of 'CNode', it may be supplied
-- to functions like 'addChild' or 'insertChild'.

mkText :: (CHTMLDocument doc) => doc -> String -> CPS b TText
mkText = createTextNode

-- |Access the @<BODY>@ node of the current HTML document.

documentBody :: (CHTMLDocument doc) => doc -> CPS b THTMLBodyElement
documentBody = get'body

-- |Access the @<HEAD>@ node of the current HTML document.

documentHead :: (CHTMLDocument doc) => doc -> CPS b THTMLHeadElement
documentHead = unsafeGetProperty "head"

-- |'Nothing', properly typed to 'Maybe' 'TNode'.

nodeNothing :: Maybe TNode
nodeNothing = Nothing

-- |Obtain an EventTarget interface on a node; basically itself

eventTarget :: (CNode a) => a -> CPS b TEventTarget
eventTarget = unsafeToSelf

-- |Cast a Node subclass to Node: basically itself

elementNode :: (CNode a) => a -> CPS b TNode
elementNode = unsafeToSelf

-- |Cast a HTMLElement subclass to HTMLElement: basically itself

htmlElement :: (CHTMLElement a) => a -> CPS b THTMLElement
htmlElement = unsafeToSelf

-- |Obtain an inline style ('TCSS2Properties') interface of an object

inlineStyle :: (CHTMLElement a) => a -> CPS b TCSS2Properties
inlineStyle = unsafeGetProperty "style"

-- |Obtain an inline style ('TCSSStyleDeclaration') interface of an object

inlineStyleDecl :: (CHTMLElement a) => a -> CPS b TCSSStyleDeclaration
inlineStyleDecl = unsafeGetProperty "style"

-- |Obtain a reference to the parent element

parentElement :: (CHTMLElement a, CHTMLElement b) => a -> CPS z b
parentElement a = ((elementNode a get'parentNode) :: CPS z TNode)  unsafeToSelf 

-- |Given a 'CEvent', obtain a 'THTMLElement' which is event\'s target

targetElement :: (CEvent e) => e -> CPS b THTMLElement
targetElement e = toCPE (targetElement' e)
targetElement' a = unsafeJS "return getEventTarget(a);"

