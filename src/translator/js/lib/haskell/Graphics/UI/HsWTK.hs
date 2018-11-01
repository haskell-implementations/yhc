-----------------------------------------------------------------------------
-- |
-- Module      :  Graphics.UI.HsWTK
-- Copyright   :  (c) Dmitry Golubovsky, 2007       
-- License     :  BSD-style
-- 
-- Maintainer  :  Dmitry Golubovsky <golubovsky@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (needs Yhc Javascript backend)
--
-- Haskell Web Toolkit         
--
-----------------------------------------------------------------------------

module Graphics.UI.HsWTK (
-- * Data Types
  Widget
 ,ECRF
 ,ACTF
 ,FwdValueMsg (..)
 ,CSSProp (..)
-- * Infix Operations: Nesting and Composing Widgets
 ,(+++)
 ,(<<)
 ,(|<<)
 ,(++|)
-- * Static Interface Elements
 ,container
 ,passive
 ,nowidget
 ,active
-- * Decorators
 ,decorate
 ,withClass
 ,withID
 ,withTitle
 ,withLang
 ,withDir
 ,withSrc
 ,withAlt
 ,withValue
 ,withStyle
 ,set'style
-- * Widgets and Utility Functions
-- $docwid
 ,docBodyC
 ,textP
 ,imageP
 ,iconoptP
 ,stringD
 ,inputI
 ,buttonI
 ,txtAreaI
 ,updateU
 ,updContainerU
 ,evt2ConstU
 ,askValueU
 ,readTargetU
-- * Activator Patterns
 ,readLineA
 ,evtBCastA
 ,fwdValueA
 ,updMapA
 ,passMapStateA
 ,tabIndexA
) where

import CPS
import UnsafeJS
import Data.Char
import Data.Maybe
import DOM.Level2.Dom
import DOM.Level2.Html2
import DOM.Level2.Node
import CDOM.Level2.DomUtils
import CDOM.Level2.Events
import DOM.Level2.Element
import DOM.Level2.HTMLElement
import DOM.Level2.HTMLDivElement
import DOM.Level2.Events
import DOM.Level2.KeyEvent
import DOM.Level2.Css
import DOM.Level2.CSSStyleDeclaration
import qualified DOM.Level2.HTMLInputElement as I
import qualified DOM.Level2.HTMLButtonElement as B
import qualified DOM.Level2.HTMLImageElement as J
import qualified DOM.Level2.HTMLOptionElement as O
import qualified DOM.Level2.HTMLSelectElement as S
import qualified DOM.Level2.HTMLParamElement as P
import qualified DOM.Level2.HTMLTextAreaElement as A
import Control.Concurrent.JSThreads

-- ---------------------------------------------------------------------------
-- Data types

-- |The Widget data type corresponds to an abstract element of browser-based
-- GUI. Each widget is defined by a function which takes the owner document
-- which the widget is a part of, and a HTML element which is widget's parent.
-- Top-level widgets have document's BODY element as their parent. The function
-- defining a widget returns a boolean value: usually True, but False does not
-- bear any special meaning here. The choice of a boolean return type was
-- dictated by the fact that Javascript event handlers are expected return
-- True by default (False might mean prevention of default action on an event).

type Widget =  THTMLDocument -> THTMLElement -> Bool

-- |Element creation function type. The n argument really should be constrained
-- to be an instance of 'CHTMLElement' class, but Yhc language limitations
-- do not allow to have constraints in type declarations.
-- An element creation function creates a HTML element (tagged node) within
-- the given document, but does not place it inside of a parent element.
-- Examples of such functions are 'mkDiv' (defined in the module 
-- 'DOM.Level2.HTMLDivElement'), 'J.mkImg' (defined in the module 
-- 'DOM.Level2.HTMLImageElement'), etc. Usually name of such function 
-- is name of corresponding HTML tag with first letter capitalized, having \"mk\" prepended.

type ECRF n = (THTMLDocument -> CPS Bool n)

-- |Widget activation function type. It takes 'THTMLElement' as its first argument
-- which is sufficient for style setting, or attachment of an event handler.
-- If actions more specific to certain HTML element type are required,
-- explicit cast from 'THTMLElement' is necessary.

type ACTF = THTMLElement -> Bool

-- |Message type to manipulate the forwarding of element values.

data FwdValueMsg =
     FwdCurr           -- ^forward the current value, not changing it
    |FwdCurrSet String -- ^forward the current value, update with new value
    |FwdUpdSet  String -- ^update element\'s value with new value, forward the new value
    |FwdCurrTo  (MSGBOX Bool String) -- ^like 'FwdCurr' but to a specified MsgBox
    |FwdCurrSetTo (MSGBOX Bool String) String -- ^like 'FwdCurrSet' but to a specified MsgBox
    |FwdUpdSetTo (MSGBOX Bool String) String -- ^like 'FwdUpdSet' but to a specified MsgBox

-- ---------------------------------------------------------------------------
-- Combinators

-- |The two inline combinators, '+++', and '<<' defined by the Toolkit act similarly to
-- those in the standard Text.Html package.

infixr 2 ++|, +++
infixr 7 <<,  |<<

-- |Compose two widgets. The two widgets are placed one after another
-- (left-to-right) within the same parent element. (NB: should we take
-- document direction into consideration?)
(+++) :: Widget -> Widget -> Widget

x +++ y = f x y where 
  f x y doc par =
    let x' = x doc par 
        y' = y doc par
    in  x' `seq` y' 

-- |Nest a widget into a container. The left widget exposing a container
-- interface (via the container combinator) becomes a parent for the
-- right widget.
--
-- Correct usage of '+++' and '<<' is as follows:
--
-- @C1 '<<' C2 '<<' W1@: - W1 nests inside C2, C2 nests inside C1
--
-- @W1 '+++' W2@: W1 displays first, then W2 (in left-right, top-bottom order)
--             within the same parent widget
-- 
-- @C1 '<<' (W1 '+++' W2)@: W1 and W2 are child widgets of container C1
--
-- @C1 '<<' W1 '+++' C2 '<<' W2@: W1 is nested in C1, and W2 in C2;
--                         C1 displays first, then C2 (in left-right, 
--                         top-bottom order)
--
-- An example of a container is a DIV element (which is a natural container
-- for other elements):
-- cont_div w = container 'mkDiv' w - the argument of cont_div is necessary 
-- to avoid CAF. 'mkDiv' is an auto-generated (by the DOM IDL conveter) function
-- which creates a DIV element.
(<<) :: (Widget -> Widget) -> Widget -> Widget

x << y = x y

-- | @c '|<<' d@ is equivalent to @'container' c '<<' d@
(|<<) :: (CHTMLElement n) => (THTMLDocument -> CPS Bool n) -> Widget -> Widget

c |<< d = container c << d

-- |@a '++|' b@ is equivalent to @a '+++' 'passive' b@
(++|) :: (CNode n) => Widget -> (ECRF n -> Widget)  

a ++| b = a +++ passive b


-- ---------------------------------------------------------------------------
-- Static interface elements

-- |Construct a container widget. This combinator takes a HTML element
-- creation function (crf) and returns an incomplete application of another
-- function, which, given a child widget (w), returns another widget
-- which is the container with the child widget nested into.

container :: (CHTMLElement n) => ECRF n -> Widget -> Widget

container crf w doc par =
  crf doc $ \celt ->
  addChild celt par $ \_ ->
  htmlElement celt $ \celn ->
  w doc celn

-- |Construct a passive widget. Passive widgets cannot have anything nested.
-- An example of such a passive widget is TEXT element which usually nests into
-- a DIV or SPAN (or, less frequently, a BUTTON or an OPTION).

passive :: (CNode n) => ECRF n -> Widget 

passive crf doc par =
  crf doc $ \celt ->
  addChild celt par $ \_ -> True

-- |A simpliest posible passive widget, basically a NOP in the widgets world.
-- Use it to nest inside a container when the container does not need to
-- nest anything else. It also can be used as a "final" element in the 'foldr'
-- based constructs: @'foldr' ('+++') 'nowidget' widgetlist@ if a list of widgets
-- needs to be nested.

nowidget :: Widget

nowidget _ _ = True

-- |A generic widget activator. Activators add responsiveness of widgets
-- to user's action delivered via browser's events, or simply perform
-- some one-time actions such as setting up widget's style, etc.
-- The first argument of active is a function that given a 'THTMLElement',
-- will perform some actions upon.
-- Activators are attached to widgets by insertion, so the widget has to expose
-- the container interface in order to have activators.
-- So, in order to make a DIV clickable, one has to write:
-- @'mkDiv' '|<<' 'active' clickhandler@
-- or, perhaps, assuming that the DIV has some other visible content:
-- @'mkDiv' '|<<' (content '+++' 'active' clickhandler)@
-- where content is something to be inserted into the DIV. Note parentheses:
-- if more than one widgets are nested, their composition has to be in
-- parentheses.

active :: ACTF -> Widget
  
active actf doc par =
  forkAfter 0 (actf par) True


-- ---------------------------------------------------------------------------
-- Activator patterns

-- |This activator is suitable in text input fields. It allows editing
-- of field value, and sends the input value to the supplied message box
-- as a 'String'.

readLineA :: (MSGBOX Bool String) -> ACTF

readLineA mb par = (waitFor par "keydown" :: CPS Bool TKeyEvent) $ \e ->
  get'keyCode e $ \c ->
  let cont z | z == cDOM_VK_ENTER =
               (unsafeToSelf par :: CPS Bool THTMLInputElement) $ \sel ->
               I.get'value sel $ \v ->
               sendMsg_ mb v |>>|
               I.set'value "" sel |>>| readLineA mb par
             | otherwise = readLineA mb par
  in cont c


-- |Map an event to a value and broadcast it over multiple message boxes.

evtBCastA :: (CEvent e) 
          => String            -- ^event type (\"click\", etc.)
          -> (e -> CPS Bool v) -- ^event mapper function
          -> [MSGBOX Bool v]   -- ^list of MsgBoxes to broadcast (may be just one)
          -> ACTF

evtBCastA evid evf mbs par = waitFor par evid $ \e -> 
  evf e $ \v -> 
  broadCastMsg_ v mbs |>>| evtBCastA evid evf mbs par

-- |Upon reception of a message via input message box (of type 'FwdValueMsg'), 
-- forward the element's value to the output message box. Modify the element's value
-- accordingly.

fwdValueA :: MSGBOX Bool FwdValueMsg -- ^input message box
          -> MSGBOX Bool String      -- ^output message box
          -> ACTF

fwdValueA mbin mbout par = recvMsg mbin $ \[msg] ->
  get'value par $ \val ->
  case msg of
    FwdCurr -> sendMsg_ mbout val |>>| fwdValueA mbin mbout par
    FwdCurrSet nval -> 
               set'value nval par |>>| sendMsg_ mbout val |>>| fwdValueA mbin mbout par
    FwdUpdSet nval -> 
               set'value nval par |>>| sendMsg_ mbout nval |>>| fwdValueA mbin mbout par
    FwdCurrTo mb -> sendMsg_ mb val |>>| fwdValueA mbin mbout par
    FwdCurrSetTo mb nval -> 
               set'value nval par |>>| sendMsg_ mb val |>>| fwdValueA mbin mbout par
    FwdUpdSetTo mb nval -> 
               set'value nval par |>>| sendMsg_ mb nval |>>| fwdValueA mbin mbout par

-- |Receive any value, map it to 'String', and update associated display element.
-- This activator is to be used with display widgets to update the value displayed.

updMapA :: String        -- ^initial value to display
        -> (a -> String) -- ^function to map the received value to displayed value
        -> MSGBOX Bool a -- ^input message box
        -> ACTF

updMapA init mapf mbin par = 
  updateU par init |>>| recvMsg mbin $ \[msg] ->
  updMapA (mapf msg) mapf mbin par

-- |Passing activator that receives messages from one 'MSGBOX', maintains
-- internal state transitions depending upon messages received, and
-- maps internal state to messages sent to another 'MSGBOX'.
-- If 'sendMsg' fails on the output message box, state change will not
-- be remembered.

passMapStateA :: s             -- ^initial state
              -> (s -> a -> s) -- ^state transition function
              -> (s -> b)      -- ^mapping of internal state to output messages
              -> MSGBOX Bool a -- ^input message box
              -> MSGBOX Bool b -- ^output message box
              -> ACTF

passMapStateA init strans stmap mbin mbout par = recvMsg mbin $ \[msg] ->
  let nwst = strans init msg
  in  sendMsg mbout (stmap nwst) $ \rc ->
      case rc of
        Nothing -> passMapStateA init strans stmap mbin mbout par
        Just () -> passMapStateA nwst strans stmap mbin mbout par

-- |Force the \'tabIndex\' attribute on the parent element to the given
-- value.

tabIndexA :: Int -> ACTF

tabIndexA i par = unsafeSetProperty "tabIndex" 0 par $ const True

-- ---------------------------------------------------------------------------
-- Decorators

-- |The 'decorate' combinator modifies element creation function so that it
-- involves setter for certain property invoked after the element has been
-- created. This combinator\'s purpose is to create so called with-functions
-- that may be chained together to modify properties of widget\'s HTML element
-- as it is created. For example, to create a DIV element with desired class name,
-- one may write: @'mkDiv' `withClass` \"myclass\"@.
-- To add the title property to the created DIV element, one may write:
-- @'mkDiv' `withClass` \"myclass\" `withTitle` \"mytitle\"@.

decorate :: (a -> b) -> ((c -> (b -> d)) -> (a -> (c -> d)))
decorate f e s = \d -> e d (f s)

withClass :: (CHTMLElement b) => ((a -> ((b -> (CPS c b)) -> d)) -> (String -> (a -> d)))
withClass = decorate set'className

withID :: (CHTMLElement b) => ((a -> ((b -> (CPS c b)) -> d)) -> (String -> (a -> d)))
withID = decorate set'id

withTitle :: (CHTMLElement b) => ((a -> ((b -> (CPS c b)) -> d)) -> (String -> (a -> d)))
withTitle = decorate set'title

withLang :: (CHTMLElement b) => ((a -> ((b -> (CPS c b)) -> d)) -> (String -> (a -> d)))
withLang = decorate set'lang

-- |These five functions set generic properties of HTMLElement:
-- class, id, title, lang, dir.

withDir :: (CHTMLElement b) => ((a -> ((b -> (CPS c b)) -> d)) -> (String -> (a -> d)))
withDir = decorate set'dir

withSrc :: (CHTMLImageElement b) => ((a -> ((b -> (CPS c b)) -> d)) -> (String -> (a -> d)))
withSrc = decorate J.set'src

-- |Setters specific to the IMG element: src (URI) and alt.

withAlt :: (CHTMLImageElement b) => ((a -> ((b -> (CPS c b)) -> d)) -> (String -> (a -> d)))
withAlt = decorate J.set'alt

class (CHTMLElement e) => ValueSettable e where
  set'value :: String -> e -> CPS x e

instance ValueSettable THTMLButtonElement where
  set'value = B.set'value

instance ValueSettable THTMLInputElement where
  set'value = I.set'value

instance ValueSettable THTMLOptionElement where
  set'value = O.set'value

instance ValueSettable THTMLParamElement where
  set'value = P.set'value

instance ValueSettable THTMLSelectElement where
  set'value = S.set'value

instance ValueSettable THTMLTextAreaElement where
  set'value = A.set'value

class (CHTMLElement e) => ValueGettable e where
  get'value :: e -> CPS x String

instance ValueGettable THTMLButtonElement where
  get'value = B.get'value

instance ValueGettable THTMLInputElement where
  get'value = I.get'value

instance ValueGettable THTMLOptionElement where
  get'value = O.get'value

instance ValueGettable THTMLParamElement where
  get'value = P.get'value

instance ValueGettable THTMLSelectElement where
  get'value = S.get'value

instance ValueGettable THTMLTextAreaElement where
  get'value = A.get'value


-- A hack indeed: THTMLElement also needs to be an instance of
-- ValueGettable and ValueSettable. This is done by runtime
-- check of element tag name and using Input\'s 'I.get\'value' and
-- 'I.set\'value' to do the job.

taglist = ["button", "input", "option", "param", "select", "textarea"]

instance ValueSettable THTMLElement where
  set'value s e k = get'tagName e $ \tag ->
    if (map toLower tag) `elem` taglist
      then (unsafeToSelf e :: CPS a THTMLInputElement) $ \inp ->
           I.set'value s inp $ \_ -> k e
      else k e

instance ValueGettable THTMLElement where
  get'value e k = get'tagName e $ \tag ->
    if (map toLower tag) `elem` taglist
      then (unsafeToSelf e :: CPS a THTMLInputElement) $ \inp ->
           I.get'value inp $ \v -> k v
      else k ""

-- |Elements for which the value (of type 'String') property is defined,
-- can use a special decorator 'withValue' to set initial value upon creation.
-- Thee elements are: BUTTON, INPUT, OPTION, PARAM, SELECT, TEXTAREA.

withValue :: (ValueSettable b) => (a -> ((b -> (CPS c b)) -> d)) -> (String -> (a -> d))
withValue = decorate set'value

-- |Data type for building inline style assignment expressions.

data CSSProp = String := String

-- |Setter for a Element's inline style (typed like DOM setters).

set'style :: (CHTMLElement zz) => [CSSProp] -> zz -> CPS c zz
set'style ps b k = inlineStyleDecl b (sps ps) $ \_ -> k b
  where sps [] d k = k ()
        sps (p:ps) d k = sps' p d $ \_ -> sps ps d k
          where sps' (n := v) d = setProperty d n v ""

-- |Setter for a HTMLElement\'s inline style. It takes an argument composed of
-- names and values of CSS properties like this:
-- @[\"background-color\" := \"green\", \"font-weight\" := \"bold\"]@

withStyle :: (CHTMLElement b) => ((a -> ((b -> (CPS c b)) -> d)) -> ([CSSProp] -> (a -> d)))
withStyle = decorate set'style

-- ---------------------------------------------------------------------------
-- Widgets

-- $docwid Naming conventions: although not strictly enforced, it is recommended that
-- passive widgets have /P/ as their names' last letter, input widgets have /I/,
-- pure containers have /C/, display elements have /D/, combined widgets have /W/.
-- Also, there are utility functions whose names end with /U/.

{-|
Document body. This is the parent of all toplevel widgets.

Recommended usage:

>
>    main = docBodyC mainW -- declare the main function which brings up the
>                          -- document's contents
>    mainW = top1W         -- place all toplevel widgets composed here
>      +++ top2W
>      +++ ...
>
-}

docBodyC :: Widget -> Bool

docBodyC w =
  getHTMLDocument $ \doc ->
  documentBody doc htmlElement $ \par ->
  w doc par

-- |Passive text label. Decorations will be provided by the parent element.

textP :: String -> Widget

textP txt = passive (flip mkText txt)

-- |Passive image. Parameters are: image URL and alternative text.

imageP :: String -> String -> Widget

imageP src alt = passive (J.mkImg `withSrc` src `withAlt` alt `withTitle` alt)

-- |An OPTION element (to use within a SELECT element) with an icon image
-- appearing left of the option text. May not work in all browsers
-- (MSIE shows only text)

iconoptP :: String -> String -> Widget

iconoptP uri text =
  let img = case uri of
              "" -> nowidget
              _ -> imageP uri text
  in  (O.mkOption `withValue` text) |<< (img +++ textP text)

-- |String display. This element displays a string that is given as
-- a parameter. Decorations will be provided by the parent element.
-- This element needs an updating activator to be nested in order
-- to become an updateable display.

stringD :: String -> (Widget -> Widget)

stringD txt x = mkDiv |<< (textP txt +++ x)

-- |Input field. Decorations will be provided by the parent element.
-- This element needs an activator in order to communicate the input
-- value to other elements.

inputI :: THTMLDocument -> CPS a THTMLInputElement

inputI = I.mkInput 

-- |Button. Decorations will be provided by the parent element.
-- This element needs an activator in order to communicate the input
-- value to other elements.

buttonI :: THTMLDocument -> CPS a THTMLButtonElement

buttonI = B.mkButton 

-- |Editable text area. Decorations will be provided by the parent element.
-- This element needs an activator in order to communicate the input
-- value to other elements.

txtAreaI :: THTMLDocument -> CPS a THTMLTextAreaElement

txtAreaI = A.mkTextarea


-- |Utility function to update displayed value. It can be used with any
-- widget based on an element capable of having child nodes.

updateU :: (CNode a) => a -> String -> CPS b TNode

updateU par s =
  (get'ownerDocument par :: CPS z THTMLDocument) $ \doc ->
  (get'firstChild par :: CPS y TNode) $ \old ->
  mkText doc s $ \new ->
  hasChildNodes par $ \hc ->
  if hc
   then
     (replaceChild par new old :: CPS x TNode)
   else
     (appendChild par new :: CPS x TNode)


-- |Utility function to replace container\'s nested widget. It is assumed that
-- the container has only one nested widget. If it has more, only the first
-- (as in get\'firstChild) nested widget will be replaced.

updContainerU :: THTMLElement -> Widget -> CPS b THTMLElement

updContainerU par new =
  (get'ownerDocument par :: CPS z THTMLDocument) $ \doc ->
  (get'firstChild par :: CPS y TNode) $ \old ->
  hasChildNodes par $ \hc ->
  if hc
   then
     (removeChild par old :: CPS x TNode) $ \n ->
     new doc par `seq` toCPS par
   else
     new doc par `seq` toCPS par



-- |Use this with elements whose events always map to conctant values.

evt2ConstU :: a -> TEvent -> CPS Bool a

evt2ConstU a e = toCPS a

-- |Ask an element for its value. It is assumed that an element has a 'fwdValueA'
-- activator listening on the given MsgBox. The function creates a MsgBox dynamically,
-- and sends a 'FwdCurrTo' request to the element. The element is expected to
-- send its current value back.

askValueU :: MSGBOX Bool FwdValueMsg  -- ^a 'MSGBOX' known to be listened by a 'fwdValueA'
                                      --  activator.
          -> CPS Bool (Maybe String)  -- ^ returned value of the element

askValueU mbi k = msgBox $ \mbo ->
  sendMsg mbi (FwdCurrTo mbo) $ \rsnd ->
  case rsnd of
    Nothing -> k Nothing
    Just () -> recvMsg mbo $ \s -> k s


-- |Get a value from an event\'s target element.
-- This may be useful with 'evtBCastA' as an event mapping function
-- to get the value of the HTML element upon which the event occurred.

readTargetU :: TEvent         -- ^an event that was received
            -> (String -> a)  -- ^what to map the target value to
            -> CPS Bool a     -- ^mapped value of the event target element

readTargetU e f k = targetElement e get'value $ \v -> k $ f v

