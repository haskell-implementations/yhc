-----------------------------------------------------------------------------
-- |
-- Module      :  CDOM.Level2.Events
-- Copyright   :  (c) Dmitry Golubovsky, 2007       
-- License     :  BSD-style
-- 
-- Maintainer  :  Dmitry Golubovsky <golubovsky@gmail.com>
-- Stability   :  stable
-- Portability :  non-portable (needs Yhc Javascript backend)
--
-- Events Handling in DOM Level 2
--
---------------------------------------------------------------------------------

module CDOM.Level2.Events (
-- $eventdoc
-- *setEventHandler
  setEventHandler
-- $waitfor
-- *waitFor
 ,waitFor) where

import UnsafeJS
import CPS
import DOM.Level2.Dom
import DOM.Level2.Events
import Control.Concurrent.JSThreads

#ifdef __HADDOCK__
import DOM.Level2.KeyEvent
#endif

{- $eventdoc
This module provides support for handling events that a program executed
in a Web browser may receive. The Javascript backend uses mixed model of
event handling. Although methods like 'DOM.Level2.EventTarget.addEventListener'
are available via automated W3C IDL conversion, it is known that not all browsers
consistently support this model of event handling. On the other hand, older
model of setting event handlers as /on-/ attributes of DOM HTML element objects
has been widely supported by many generations of Web browsers.

In the same time, attributes of Event object (see "DOM.Level2.Events", "DOM.Level2.Event",
"DOM.Level2.KeyEvent", "DOM.Level2.MouseEvent", etc.) are available even with this
model of setting event handlers. So, this is possible to write:

>            get'keyCode e $ \kci ->
>            if kci == cDOM_VK_ENTER
>              then
>              ...
>              else
>              ...

Other methods of Event objects family are also available, but their support may
vary across different web browsers.

There are two paradigms of handling DOM events. One, similar to the \"traditional\"
method of setting a permanent event handler on a DOM object is represented by the
'setEventHandler' function. Another, oriented to using pseudo-threads (see the
"Control.Concurrent.JSThreads" module description), is represented by the 'waitFor'
function.
-}

-- |Set an event handler of the specified type. Event handlers may be set on Element's
-- and their subclasses. This function sets the most general event handler. The handler
-- is expected to receive an instance of the 'CEvent' class as its argument, and
-- return a 'Bool' value. The rules covering the interpretation of handler's return
-- value are same as in traditional Javascript programming: 'True' means proceed 
-- with default action; 'False' means prevent default action.

setEventHandler :: (CElement zz, CEvent a) 
                => String -- ^type of an event (e. g. \"click\")
                -> (a -> Bool) -- ^event handler, should 
                               --  return a Boolean value
                -> zz -- ^the DOM element upon which the event handler is set
                -> CPS c zz -- ^the return value: the sane element

setEventHandler str hndlr elt = toCPE (set'on' str hndlr elt)

set'on' a b c = unsafeJS
  "exprEval(c)['on'+exprEval(a)]=function(e){if(!e){e=window.event;};var rr = exprEval(exprEval(b)._ap([e])); return exprEval(rr)._t;};return c;"

-- Set an event handler of the specified kind. Event handlers may be set on Element's
-- and their subclasses. This function sets the most general event handler which
-- clears itself at the target object right upon message arrival.

secl :: (CElement zz, CEvent a) => String -> (a -> Bool) -> zz -> CPS c zz

secl str hndlr elt = toCPE (set'on'c str hndlr elt)

set'on'c a b c = unsafeJS
  "exprEval(c)['on'+exprEval(a)]=function(e){if(!e){e=window.event;};getEventTarget(e)['on'+e.type]=null;var rr = exprEval(exprEval(b)._ap([e])); return exprEval(rr)._t;};return c;"

{- $waitfor
The 'waitFor' function sets the event handler on a given DOM object so that
it invokes the current continuation. Right after the event fires, the same 
event handler on the DOM object is removed (set to /null/). So, between calls
to 'waitFor' the DOM object remains irresponsive to users actions.
Threads using 'waitFor' usually pass events received to other threads
via message boxes. Below is an example of such thread used in an activator
for a Button element.

> actb v mb par = (waitFor par "click" :: CPS Bool TMouseEvent) $ \e ->
>   sendMsg_ mb v |>>| actb v mb par

The thread contains an infinite loop which waits for an event, sends a message
using 'sendMsg_', then repeats again.

Return valus of 'waitFor' depends on whether any other thread has already been
waiting for the same event from this DOM element. If not analyzed, this may alter
the handler's default action. Use 'waitForB' to explicitly set the 
handler return value.
-}

-- |Wait for an event from a DOM element. Execution of the continuation given
-- is resumed as the event has been received. True is returned if no other thread
-- was waiting for the same event on the same element, False otherwise.

waitFor :: (CEvent e, CElement c) 
        => c -- ^the DOM element upon which the event handler is set
        -> String -- ^type of an event (e. g. \"click\")
        -> (e -> Bool) -- ^event handler, should return a Boolean value
        -> Bool -- ^return value

waitFor elt evid k = waitFor' elt evid $ \e -> yieldMs 50 $ k e

waitFor' elt evid k = 
  unsafeCheckProperty ("on" ++ evid) elt $ \h -> case h of
    True ->  False
    False -> secl evid k elt $ \_ -> True


-- |Same as 'waitFor', but the handler return value can be explicitly set.

waitForB :: (CEvent e, CElement c) 
         => c -- ^the DOM element upon which the event handler is set
         -> String -- ^type of an event (e. g. \"click\")
         -> Bool -- ^value which the current handler should return
         -> (e -> Bool) -- ^event handler, should return a Boolean value
         -> Bool -- ^return value

waitForB elt evid rv k = 
  let w = waitFor elt evid k
  in  w `seq` rv

