-- A program similar to the Echo program, written
-- without monads as the first step to Fudgets adoption.
-- The program also demonstrates use of the DOM Level1 framework
-- also implemented in CPS style.

module  EchoCPS2 where

import UnsafeJS

import CPS
import Roman
import DOM.Level2.Dom
import DOM.Level2.Html2
import CDOM.Level2.DomUtils
import CDOM.Level2.Events
import DOM.Level2.Events
import DOM.Level2.Document
import DOM.Level2.HTMLElement
import DOM.Level2.HTMLDivElement
import DOM.Level2.HTMLSpanElement
import DOM.Level2.HTMLInputElement
import DOM.Level2.KeyEvent
import DOM.Level2.CSS2Properties
import Debug.Profiling


insert = flip addChild

sty x = x (set'color "lightgreen")
          (set'fontFamily "lucida console, courier new, fixed, monospace") 
          (set'fontWeight "bold")
          (set'fontSize "14pt")
          (set'width "95%")
          (set'backgroundColor "black")         
 

main = getHTMLDocument $ \doc ->
       mkInput doc
         (set'id "input-echo") $ \inp ->
       mkDiv doc $ \scr ->
       mkSpan doc $ \spn ->
       documentBody doc $ \body -> 
       toCPS body
         (mkDiv doc 
            (DOM.Level2.HTMLDivElement.set'align "center")
            (mkText doc "*** The Echo Benchmark ***" . insert)
          . insert) 
         (toCPS scr . insert)
         (toCPS spn
           (mkText doc ">" . insert)
           (toCPS inp 
              (setEventHandler "keypress" (inkey scr inp))
            . insert)
          . insert) $ \_ ->
         inlineStyle inp
           (set'borderWidth "0px") $ \_ ->
         sty (inlineStyle scr) $ \_ ->
         sty (inlineStyle spn) $ \_ ->
         sty (inlineStyle inp) $ \_ ->
         sty (inlineStyle body) $ \_ ->
         focus inp $ id



romdec :: String -> (String, String)

romdec v =
  let rom = (catchJS ((show . fromRoman) v) (\_ -> ""))
      dec = (catchJS ((toRoman . read) v) (\_ -> ""))
  in (rom, dec)

inkey :: THTMLDivElement -> THTMLInputElement -> TKeyEvent -> Bool
       
inkey d o e = 
            get'keyCode e $ \kci ->
            if kci == cDOM_VK_ENTER
              then
                get'value o $ \v ->
                if length v > 0
                  then
                    getTimeStamp $ \t1 -> 
                    toCPE (romdec v) $ \(rom, dec) ->
                    rom `seq` dec `seq` getTimeStamp $ \t2 ->
                    getHTMLDocument $ \doc ->
                    let ln =("=" ++ v ++ " " ++ rom ++ " " ++ dec ++ " " ++ 
                             show (t2 - t1) ++ " ms")
                    in toCPS d 
                         (mkDiv doc
                           (mkText doc ln . insert)
                         . insert) $ \_ ->
                    set'value "" o $ \_ ->
                    True
                  else  
                    True
              else True


