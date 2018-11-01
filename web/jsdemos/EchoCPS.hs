-- A program similar to the Echo program, written
-- without monads as the first step to Fudgets adoption.
-- The program also demonstrates use of the DOM Level1 framework
-- also implemented in CPS style.

module  EchoCPS where

import UnsafeJS

import CPS
import Roman
import DOM.Level1.Dom
import DOM.Level1.Html
import CDOM.Level1.DomUtils
import CDOM.Level1.Events
import DOM.Level1.Document
import DOM.Level1.HTMLElement
import DOM.Level1.HTMLDivElement
import DOM.Level1.HTMLInputElement
import Debug.Profiling


putLine s mbb c = getHTMLDocument $ \doc ->
                  documentBody doc $ \body ->
                  mkDiv doc $ \dv ->
                  mkText doc s $ \tx ->
                  addChild tx dv $ \ch ->
                  let iac = case mbb of
                              Nothing -> addChild dv
                              Just b -> insertChild b dv
                  in  iac body $ \ct -> 
                  c ct


main = getHTMLDocument $ \doc ->
       documentBody doc $ \body ->
       mkInput doc $ \inp ->
       addChild inp body $ \_ ->
       set'id "input-echo" inp $ \_ ->
       set'on "keypress" (inkey inp) inp $ \_ ->
       focus inp $ id

romdec :: String -> (String, String)

romdec v =
  let rom = (catchJS ((show . fromRoman) v) (\_ -> ""))
      dec = (catchJS ((toRoman . read) v) (\_ -> ""))
  in (rom, dec)

inkey :: THTMLInputElement -> a -> Bool
       
inkey o e = unsafeGetProperty "keyCode" e $ \kcs ->
            unsafeToNum kcs $ \kci ->
            if kci == 13
              then
                get'value o $ \val ->
                unsafeToString val $ \v ->
                if length v > 0
                  then
                    getTimeStamp $ \t1 -> 
                    toCPE (romdec v) $ \(rom, dec) ->
                    rom `seq` dec `seq` getTimeStamp $ \t2 ->
                    putLine (v ++ " " ++ rom ++ " " ++ dec ++ " " ++ show (t2 - t1) ++ " ms") 
                            (Just o) $ \_ ->
                    set'value "" o $ \_ ->
                    True
                  else  
                    True
              else True


