module Echo where

-- This program demonstrates the lowest possible level of interaction 
-- with Javascript. No extra libraries, no programming paradigms:
-- just some helper functions defined directly via unsafeJS.

-- Import the UnsafeJS module to get access to the low-level Javascript,
-- that is, ability to wrap arbitrary Javascript expressions into
-- a Haskell-callable function. See a special section of ycr2js 
-- Programmers guide where usage of unsafeJS is discussed in details.

import UnsafeJS
import Char

-- Import this module for decimal to Roman and Roman to decimal conversions.
-- This also serves as a primitive benchmark, and shows that third-party code
-- may also be brought into web browser.

import Roman

-- Import this module to be able to define a monad which provides
-- correct ordering of Javascript execution.

import Control.Monad

-- Opaque type for Javascript objects. No values of this type are constructed
-- in the Haskell code. But every object returned from Javascript belongs to
-- this type.

newtype JSObject = JSObject ()

-- A simple monad used for enforcing actions order only.
-- Both its bind operations (>>=, >>) should evaluate their
-- left-hand-side expression to make sure all Javascript computations
-- associated to that expression have been completed before execution
-- goes to the right-hand-side expression.
-- Even >> should evaluate its LHS argument although it is "lost"
-- for the RHS expression. This is achieved via pattern-matching.
-- The same holds for return.

data JS a = JS a

instance Monad JS where

-- bind: execute the LHS expression, extract result from the monad,
-- pass on to the RHS expression.

  (JS a) >>= fn = fn a

-- anomymous bind: execute the LHS expression, then the RHS expression
-- that is, enforce correct execution order only.

  (JS a) >> fn = fn

-- The return function must evaluate its argument before wrapping it into
-- monad. This is important e. g. for exception handlers, otherwise
-- nothing may be evaluated within exception handler, and actual exception
-- will occur outside of the exception handler and will not be caught properly.

  return a = a `seq` (JS a)

-- Here are the functions to access web-browser's DOM structures.
-- In the absence of the standardized framework, these functions 
-- are coded on ad-hoc basis, and their interface may be imperfect,
-- but this is a good demonstration what kind of stuff may be needed
-- for a real framework.

-- Get the document interface reference

getDocument :: JS JSObject

-- This (and other) examples show how to return data (including monadic) values
-- from Javascript to Haskell. The HSData constructor finction is used allover
-- the Javascript code generated form Haskell source to create data objects.
-- Its first argument is data constructor name index, and its second argument
-- is an array of data object member values in proper order. To obtain
-- data constructor name index, one has to refer to the conIdx global variable
-- which is indexed by constructor qualified name. Since the JS monad is defined
-- in this module named Echo, qualified name of the constructor is Echo.JS.
-- Web browser DOM interface defines a global name for the document interface
-- access, that is, `document'. Its value is wrapped into the JS monad
-- and returned.

getDocument = unsafeJS "return new HSData(conIdx['Echo.JS'],[document]);"

-- Obtain a property of an object

getProperty :: JSObject -> String -> JS JSObject

-- Similarly, return a value wrapped into the IO monad. The `a' argument
-- is the reference to a Javascript object, and the `b' argument is
-- a string containing name of the property to be retrieved. The function
-- evaluates both of its arguments and retrieves the property name of `a'
-- as an associative array element using the value of `b'.

getProperty a b = unsafeJS
  "return new HSData(conIdx['Echo.JS'],[exprEval(a)[exprEval(b).toString()]]);"

-- Modify a property of an object

setProperty :: JSObject -> String -> a -> JS ()

-- Set property named `b' of the object refered to as `a' to the value of `c'.
-- The third argument may be of any type, but its evaluation should terminate.

-- As a side note: this function returns a `void' value (unit, ()). Its type signature
-- might be rearranged, like this:
--
-- String -> a -> JSObject -> JS JSObject -- that is, returns the same object with
--                                        -- property modified
--
-- then operations of setting object property might be chained in the JS monad:
--
-- obj' <- setProperty "prop1" val1 obj >>= 
--         setProperty "prop2" val2 >>= 
--         setProperty "prop3" val3

setProperty a b c = unsafeJS
  "exprEval(a)[exprEval(b).toString()]=exprEval(c);return new HSData(conIdx['Echo.JS'],[]);"

-- Invoke a method of an object

runMethod :: JSObject -> String -> a -> JS JSObject

-- This function obtains reference to an object's method similarly 
-- to obtaining reference to a property, using the method name string.
-- Its third argument `c' must be a list. It is converted into an array
-- of function arguments and the method is applied to this array.
-- This function uses `cbrApply' rather than the Javascript function object's
-- `apply' method for cross-browser compatibility: MSIE DOM functions do not have
-- the `apply' method, so cbrApply detects that and works around this issue.
-- Evaluation of expressions in `c' is done by the `toArray' method which is properly
-- overloaded for Haskell lists as they are unmarshalled to Javascript.

runMethod a b c = unsafeJS
  "var a1=exprEval(a); return new HSData(conIdx['Echo.JS'],[cbrApply(a1[exprEval(b).toString()],a1,exprEval(c)._toArray())]);"

-- Output string representation of an object into the status line

putStatLn :: a -> JS ()

-- This is a helper function which sets the `window.status' global object to
-- the stringified value of the function's argument. Note how a unit value ()
-- is returned: an empty array is used as the second argument to HSData. While
-- to be exact, one should retrieve an index of the () data constructor and
-- construct another data object, the practice shown is acceptable in most cases,
-- except for the situation when pattern matching is done on the unit value.

putStatLn a = unsafeJS "window.status=exprEval(a).toString(); return new HSData(conIdx['Echo.JS'],[]);"

-- Output source representation of an object into the status line

putStatLnSrc :: a -> JS ()

-- Similarly to the function above, but `toSource' method is used.

putStatLnSrc a = unsafeJS "window.status=exprEval(a).toSource(); return new HSData('Echo_46JS',[]);"


-- Register an event handler (old DOM0 style)

regEventHandler :: JSObject -> String -> (JSObject -> JS ()) -> JS ()

-- This function attaches an old-style event handler `c' to a DOM object
-- (such as an input field) `a'. Name of the event is passed in `b' as string
-- (e. g. "onkeypress"). This function works around browser incompatibility.
-- Mozilla/Netscape/FireFox pass event information as an event handler's
-- argument. MSIE passes it in a global object named `event'. This function
-- wrap the Haskell handler function into an anonymous function which checks
-- for its argument, and if unavailable, tries to get the information
-- from the global `event' object.

regEventHandler a b c = unsafeJS
  "exprEval(a)[exprEval(b).toString()]=function(e){if(!e){e=window.event;}; return exprEval(exprEval(c)._ap([e]));}; return new HSData(conIdx['Echo.JS'],[]);"

-- Get the numeric representation of an object

asInt :: JSObject -> JS Int

-- This function (ab)uses the untypedness of Javascript and coerces
-- any object to a numeric value.

asInt a = unsafeJS
  "return new HSData(conIdx['Echo.JS'],[new Number(exprEval(a))]);"

-- Get the String representation of an object

asString :: JSObject -> JS String

-- This function (ab)uses the untypedness of Javascript and coerces
-- any object to a stringified value.

asString a = unsafeJS
  "return new HSData(conIdx['Echo.JS'],[new String(exprEval(a))]);"

-- Catch exceptions using Javascript exception machinery

catchJS :: JS a -> (JSObject -> JS a) -> JS a

-- This function installs an exception handler `b' while executing an expression
-- in `a'. If an exception occurs, it will be passed to the handler which should
-- return a "replacement" value.

catchJS a b = unsafeJS
  "try {return exprEval(a);} catch(_e) {return exprEval(b)._ap([_e]);}"

-- Get current time in ms since 1970 (for performance measurements)

getTime :: Int -> JS Int

-- This function obtains current time from the browser. The first argument
-- is not used but is necessary in order to have this function evaluated
-- more than once. In its absence, evaluation result will be stored in the
-- getTime's function descriptor, and it will always return time of the first
-- evaluation.

getTime a = unsafeJS
  "return new HSData(conIdx['Echo.JS'],[(new Date()).getTime()]);"


-- Key press handler

--       element     event       void
inkey :: JSObject -> JSObject -> JS ()

-- An event handler function has its first argument
-- to pass the reference to the object which generates an event. Thus,
-- although this builds a circular structure with a DOM object involved (may be harmful
-- for MSIE garbage collection), this works around some incompatibilities
-- betwenn browsers regarding how event source is encoded in the event.
-- As an alternative, object "id" property value might be passed instead of
-- reference to an object itself. This would avoid creation of a circular link,
-- but would make execution of the event handler slightly longer.

-- As a side note: Event handler's type signature might be modified to return
-- a modified event. Using this approach, event handlers might be chained
-- using the monadic `bind' function which passes output from its LHS expression
-- to its RHS expression.

-- This handler analyzes code of the key pressed.
-- If it is Enter (13) then the input value is placed into a
-- dynamically created div and inserted above the input field.

inkey o e = do

-- Obtain numeric code of the key pressed

  kcs <- getProperty e "keyCode" >>= asInt

-- If `Enter' was pressed

  when (kcs == (13::Int)) $ do

-- Obtain the time when `Enter' was pressed

    t1 <- getTime 0

-- Get the document interface

    doc <- getDocument

-- Get the document body element interface

    body <- getProperty doc "body"

-- Create a div dynamically

    mydiv <- runMethod doc "createElement" ["div"]

-- Obtain the string typed: it is the value of the input element

    v <- getProperty o "value" >>= asString

-- If the string was not empty

    when (length v > 0) $ do

-- Conversion functions of the Roman module raise error 
-- when conversion cannot be done. In this case, empty string
-- should be returned.

      rom <- catchJS (return $ (show . fromRoman) v) (\_ -> return "")
      dec <- catchJS (return $ toRoman (read v)) (\_ -> return "")

-- Obtain the time after possible conversion is done

      t2 <- getTime 0

-- Format the result

      let dt = t2 - t1
      let vr = v ++ " " ++ rom ++ " " ++ dec ++ " " ++ show dt ++ " ms"

-- Output the result into the div element

      setProperty mydiv "innerHTML" vr

-- Make the div visible right above the input element

      runMethod body "insertBefore" [mydiv, o]

-- Clear the input element

      setProperty o "value" ""

-- Return from the handler

      return ()
    return ()
  return ()

-- Main program. As in "traditional" Haskell programming, it has a monadic type.
-- But since there is no "traditional" input/output in a web browser, the IO monad
-- is useless here; use the JS monad.

main :: JS ()

main = do

-- Get the document interface

  doc <- getDocument

-- Get the document body element interface

  body <- getProperty doc "body"

-- Create the input field.

  inp <- runMethod doc "createElement" ["input"]

-- Set the "id" property. It is unused in this demo, but
-- might be helpful in general.

  setProperty inp "id" "input-echo"

-- Append the input element ot the list of body's children
-- thus visualizing it.

  runMethod body "appendChild" [inp]

-- Register an event handler on the input field. Note that
-- (inkey inp) is passed as an event handler; thus, its closure
-- will contain reference to the input element that generates an event.

  regEventHandler inp "onkeypress" (inkey inp)

-- Set input focus.

  runMethod inp "focus" []
  return ()
