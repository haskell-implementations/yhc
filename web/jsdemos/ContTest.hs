-- Test of Control.Monad.Cont in Javascript

module ContTest where

import CPS
import UnsafeJS
import Data.JSRef
import Control.Concurrent.JSThreads
import CDOM.Level2.DomUtils
import DOM.Level2.Dom
import DOM.Level2.Html2
import DOM.Level2.HTMLElement
import DOM.Level2.HTMLDivElement
import Debug.Profiling

putLineTm = putLine0 True

putLine = putLine0 False

putLine0 tmf cls txt doc par = 
  getTimeStamp $ \tm ->
  mkText doc ((if tmf then (show tm ++ ": ") else "") ++ txt) $ \t ->
  mkDiv doc (set'className cls) $ \d ->
  addChild t d $ \_ ->
  addChild d par

main = getHTMLDocument $ \doc ->
       documentBody doc $ \body ->
       putLine "title" "Simple Concurrency Test with Plain CPS" doc body $ \_ ->
       forkThread (step1 doc body) $
       forkThread (step3 doc body) $
       msgBox $ \mb ->
       forkThread (thr1 doc body mb) $ 
       forkThread (thr2 doc body mb) $ 
       True


step1 doc body =
  putLineTm "" "Line 1-5" doc body |>>|
  putLineTm "" "Line 1-6" doc body |>>|
  getTimeStamp $ \t1 ->
  yieldMs 1000 $
  getTimeDiff t1 $ \tmm ->
  putLineTm "" ("Actual timeout was " ++ show tmm ++ "ms") doc body |>>|
  putLineTm "" "Line 1-7" doc body |>>|
  putLineTm "" "Line 1-8" doc body |>>|
  True

step3 doc body =
  putLineTm "" "Line 3-9" doc body |>>|
  putLineTm "" "Line 3-A" doc body |>>|
  yieldMs 500 $
  putLineTm "" "Line 3-B" doc body |>>|
  putLineTm "" "Line 3-C" doc body |>>|
  True



showresp r doc body =  case r of
    Nothing -> putLine "" "Failed" doc body
    Just m -> putLine "" ("Success") doc body
 
showmsg t m doc body =
  case m of
    Nothing -> putLine "" (t ++ " " ++ "No message") doc body
    Just m' -> putLine "" (t ++ " " ++ "Message: " ++ show m') doc body

thr1 doc body mb =
  putLine "" "Thread 1 started" doc body |>>|
  putLine "" "Thread 1 waiting" doc body |>>|
  recvMsg mb $ \m ->
  showmsg "T1:" m doc body |>>|
  putLine "" "Thread 1 resumed" doc body |>>|
  putLine "" "Thread 1 sending" doc body |>>|
  sendMsg mb "123" $ \x ->
  showresp x doc body |>>|
  putLine "" "Thread 1 finishing" doc body |>>|
  True

thr2 doc body mb =
  putLine "" "Thread 2 started" doc body |>>|
  putLine "" "Thread 2 sending" doc body |>>|
  sendMsg mb "abc" $ \x ->
  showresp x doc body |>>|
  putLine "" "Thread 2 has sent message" doc body |>>|
  putLine "" "Thread 2 waiting" doc body |>>|
  recvMsg mb $ \m ->
  showmsg "T2:" m doc body |>>|
  putLine "" "Thread 2 finishing" doc body |>>|
  True


