#ifndef __HADDOCK__

module CDOM.Level1.Events (
  set'on) where

-- This module contains the implementation of Level 1 specification of events
-- also known as on-event handlers. This implementation is handwritten because
-- IDL definitions for DOM events are available starting at Level 2 only.

import UnsafeJS
import CPS
import DOM.Level1.Dom

-- Set an event handler of the specified kind. Event handlers may be set on Element's
-- and their subclasses.

set'on :: (CElement zz) => String -> (a -> Bool) -> zz -> CPS c zz

set'on str hndlr elt = toCPE (set'on' str hndlr elt)

set'on' a b c = unsafeJS
  "exprEval(c)['on'+exprEval(a)]=function(e){if(!e){e=window.event;};var rr = exprEval(exprEval(b)._ap([e])); return exprEval(rr)._t;};return c;"

#endif

