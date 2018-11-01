-- Handwritten utilities to work with DOM Level1

#ifndef __HADDOCK__

module CDOM.Level1.DomUtils (
  getDocument, getHTMLDocument, insertChild,
  addChild, mkText, documentBody, nodeNothing
) where

import DOM.Level1.Dom
import DOM.Level1.Document
import DOM.Level1.Html
import DOM.Level1.HTMLDocument
import DOM.Level1.Node
import UnsafeJS
import CPS

getDocument :: CPS b TDocument

getDocument = toCPE getDocument'

getDocument' = unsafeJS "return document;"

getHTMLDocument :: CPS b THTMLDocument

getHTMLDocument = toCPE getHTMLDocument'

getHTMLDocument' = unsafeJS "return document;"

addChild :: (CNode newChild, CNode zz) =>
              newChild -> zz -> CPS b zz

addChild child parent k = 
  (appendChild parent child :: CPS b TNode) $ \p -> k parent

insertChild :: (CNode refChild, CNode newChild, CNode parent) =>
  refChild -> newChild -> parent -> CPS b parent

insertChild before child parent k = 
  (insertBefore parent child before :: CPS b TNode) $ \p -> k parent

mkText :: (CHTMLDocument a) => a -> String -> CPS b TText
mkText = createTextNode

documentBody :: (CHTMLDocument a) => a -> CPS b THTMLBodyElement
documentBody = get'body

nodeNothing = Nothing :: Maybe TNode

#endif

