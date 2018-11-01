-- The early demo of Haskell Web ToolKit (HsWTK) features.
-- This demo is to be used to test browsers for memory leaks.

module  HsWTKDemo where

import UnsafeJS

import CPS
import Data.JSRef
import DOM.Level2.Dom
import DOM.Level2.Node
import DOM.Level2.Html2
import CDOM.Level2.DomUtils
import CDOM.Level2.Events
import DOM.Level2.Event
import DOM.Level2.Events
import DOM.Level2.Document
import DOM.Level2.Element
import DOM.Level2.HTMLElement
import DOM.Level2.HTMLDivElement
import DOM.Level2.HTMLSpanElement
import qualified DOM.Level2.HTMLInputElement as I
import qualified DOM.Level2.HTMLButtonElement as B
import qualified DOM.Level2.HTMLImageElement as J
import qualified DOM.Level2.HTMLOptionElement as O
import qualified DOM.Level2.HTMLSelectElement as S
import DOM.Level2.KeyEvent
import DOM.Level2.CSS2Properties
import Debug.Profiling
import Control.Concurrent.JSThreads

type Widget = THTMLDocument -> THTMLElement -> Bool

docBodyC :: Widget -> Bool

docBodyC w =
  getHTMLDocument $ \doc ->
  documentBody doc htmlElement $ \par ->
  w doc par

container :: (CHTMLElement n) => (THTMLDocument -> CPS Bool n) -> Widget -> Widget

container crf w doc par =
  crf doc $ \celt ->
  addChild celt par $ \_ ->
  htmlElement celt $ \celn ->
  w doc celn

nowidget :: Widget

nowidget _ _ = True

active :: (THTMLElement -> Bool) -> Widget

active actf doc par =
  forkAfter 0 (actf par) True

passive :: (CNode n) => (THTMLDocument -> CPS Bool n) -> Widget

passive crf doc par =
  crf doc $ \celt ->
  addChild celt par $ \_ -> True

--

decorate f e s = \d -> e d (f s)

withClass = decorate set'className

withID = decorate set'id

withTitle = decorate set'title

withLang = decorate set'lang

withDir = decorate set'dir

withSrc = decorate J.set'src
withAlt = decorate J.set'alt
withSize = decorate S.set'size

class (CHTMLElement e) => ValueSettable e where
  set'value :: String -> e -> CPS x e

instance ValueSettable THTMLButtonElement where
  set'value = B.set'value

instance ValueSettable THTMLInputElement where
  set'value = I.set'value

instance ValueSettable THTMLOptionElement where
  set'value = O.set'value
{--
instance ValueSettable THTMLParamElement where
  set'value = P.set'value
--}
instance ValueSettable THTMLSelectElement where
  set'value = S.set'value
{--
instance ValueSettable THTMLTextAreaElement where
  set'value = A.set'value
--}
withValue = decorate set'value


--

(|>>>) :: Widget -> Widget -> Widget

x |>>> y = f x y where
  f x y doc par =
    let x' = x doc par
        y' = y doc par
    in  x' `seq` y'

--

textP :: String -> Widget

textP txt = passive (flip mkText txt)

imageP :: String -> String -> Widget

imageP src alt = passive (J.mkImg `withSrc` src `withAlt` alt)

shellC :: String -> Widget -> Widget

shellC ns w = container (mkDiv `withClass` "shell") $
       container (mkDiv `withClass` "title") (textP ns) 
  |>>> container (mkDiv `withClass` "client") w

gapP = passive (mkDiv `withClass` "gap")

buttonI txt x = container B.mkButton $ textP txt |>>> x

stringD txt x = container mkDiv $ textP txt |>>> x

selectI h = container (S.mkSelect `withSize` h)

inputI c = container I.mkInput c

iconoptP uri text = 
  let img = case uri of
              "" -> nowidget
              _ -> imageP uri text
  in  container (O.mkOption `withValue` text) $ img |>>> (textP text)

--

data Btn = Up | Reset | Down

upcntW = msgBox $ \mbx ->
         container (mkDiv `withClass` "banner") 
           (textP "Press the \"Up\" button to increase the counter")
    |>>> container (mkSpan `withClass` "btn") (buttonI "Up" $ active (actb Up mbx))
    |>>> container (mkSpan `withClass` "display") (stringD "0" $ active (actd 0 mbx))
    |>>> passive  (mkDiv `withClass` "divider")

factW  = msgBox $ \mbx ->
         container (mkDiv `withClass` "banner")
           (textP "Enter a decimal number, press \"Enter\", see the factorial calculated")
    |>>> container (mkSpan `withClass` "btn") (inputI $ active (acti mbx))
    |>>> container (mkSpan `withClass` "display") (stringD "0" $ active (actf mbx))
    |>>> passive  (mkDiv `withClass` "divider")

timerW = msgBox $ \mbx ->
         container (mkDiv `withClass` "banner")
           (textP $ "Enter interval in ms, press \"Enter\"; " ++ 
                    "while waiting, play with other widgets")
    |>>> container (mkSpan `withClass` "btn") (inputI $ active (acti mbx))
    |>>> container (mkSpan `withClass` "display") (stringD "0" $ active (actt mbx))
    |>>> passive  (mkDiv `withClass` "divider")


tbcntW = msgBox $ \mbx ->
         container (mkDiv `withClass` "banner")
           (textP "Use \"Down\", \"Reset\", \"Up\" buttons to change the counter")
    |>>> let btn v i t = container (mkSpan `withClass` "btn") $
                         buttonI t  (imageP i t |>>> active (actb v mbx))
         in       btn Down  "icons/go-down.png"    "Down"
             |>>> btn Reset "icons/go-bottom.png"  "Reset"
             |>>> btn Up    "icons/go-up.png"      "Up"
    |>>> container (mkSpan `withClass` "display") (stringD "0" $ active (actd 0 mbx))
    |>>> passive  (mkDiv `withClass` "divider")


selicnW = msgBox $ \mbx ->
          container (mkDiv `withClass` "banner")
            (textP $ "Select an option from drop-down menu, and see it displayed. " 
                  ++ "Not every browser displays options' icons")
     |>>> container (mkSpan `withClass` "btn") (
            selectI 1 ((iconoptP "icons/mail.png" "Mail" 
                  |>>>  iconoptP "icons/edit.png" "Edit"
                  |>>>  iconoptP "icons/help-about.png" "Help") |>>> active (acts mbx)))
     |>>> container (mkSpan `withClass` "display") (stringD "None" $ active (actu mbx))
     |>>> passive  (mkDiv `withClass` "divider")


acti mb par = (waitFor par "keydown" :: CPS Bool TKeyEvent) $ \e ->
  get'keyCode e $ \c ->
  get'charCode e $ \ch ->
  let cont z | z == cDOM_VK_ENTER = 
               (unsafeToSelf par :: CPS Bool THTMLInputElement) $ \sel ->
               I.get'value sel $ \v -> 
               sendMsg_ mb v |>>| 
               I.set'value "" sel |>>| acti mb par
             | otherwise = acti mb par
  in cont c

fac 0 = 0
fac 1 = 1
fac n = n * (fac (n - 1))

mfac :: String -> String
mfac s = catchJS ((show . fac . read) s) (\_ -> "--")

actf mb par = recvMsg mb $ \[s] ->
  updateD par (mfac s) $ \_ ->
  actf mb par 

actt mb par = recvMsg mb $ \[s] ->
  let sn = catchJS (read s) (\_ -> -1) in
  getTimeStamp $ \t1 ->
  updateD par "Start..." |>>|
  yieldMs sn $
  getTimeStamp $ \t2 ->
  updateD par ("Stop: " ++ show (t2 - t1) ++ " ms") |>>|
  actt mb par

acts mb par = (waitFor par "change" :: CPS Bool TEvent) $ \e ->
  (unsafeToSelf par :: CPS Bool THTMLSelectElement) $ \sel ->
  S.get'value sel $ \v ->
  sendMsg_ mb v |>>| 
  acts mb par

actu mb par = recvMsg mb $ \[s] ->
  updateD par s |>>|
  actu mb par

actb v mb par = (waitFor par "click" :: CPS Bool TMouseEvent) $ \e ->
  sendMsg_ mb v |>>|  actb v mb par

actd n mb par = recvMsg mb $ \[s] -> 
    let f = case s of
               Up -> succ
               Down -> pred
               Reset -> const 0
        n' = f n
    in  updateD par (show n') |>>|
        actd n' mb par


updateD par s = 
  (get'ownerDocument par :: CPS z THTMLDocument) $ \doc ->
  (get'firstChild par :: CPS y TNode) $ \old ->
  mkText doc s $ \new ->
  (replaceChild par new old :: CPS x TNode)

--

--  Continuations that may be saved in a mailbox
{--
ignore = \_ -> False

mkMsgBox k = newJSRef ignore $ \mb -> k mb

send mb msg k = readJSRef mb $ \cont -> 
  let res = cont msg
  in  res `seq` (k res)

recv mb k = readJSRef mb $ \prev ->
  writeJSRef mb (writeJSRef mb prev $ \_ -> k ) $ \_ -> True
--}
--
--

main = docBodyC mainW 

mainW = gapP
   |>>> shellC "Up Counter" upcntW
   |>>> gapP
   |>>> shellC "Three Button Counter" tbcntW 
   |>>> gapP
   |>>> shellC "Select Element with Icons" selicnW
   |>>> gapP
   |>>> shellC "Factorial" factW
   |>>> gapP
   |>>> shellC "Measured Time Interval" timerW

