-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Concurrent.JSThreads
-- Copyright   :  (c) Dmitry Golubovsky, 2007       
-- License     :  BSD-style
-- 
-- Maintainer  :  Dmitry Golubovsky <golubovsky@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable (needs Yhc Javascript backend)
--
-- Implementation of Javascript Pseudo-Threads
--
-------------------------------------------------------------------------------


module Control.Concurrent.JSThreads (
-- * Forking threads
  forkThread
-- * Yielding execution to other threads
 ,yieldMs
-- * Sequencing actions with side effects without passing results
 ,(|>>|)
-- * Exchange of messages between threads
-- ** Creation of message boxes
 ,msgBox
-- ** Sending messages
 ,sendMsg
 ,sendMsg_
 ,broadCastMsg_
-- ** Receiving messages
 ,recvMsg
-- ** Related data structures
 ,MBStatus (..)
 ,MSGBOX
) where

import UnsafeJS
import CPS
import Data.JSRef

-- | Fork a new thread. The supplied expression will be evaluated
-- in a new thread. Execution of the current (parent) thread will be yielded,
-- so the new \"child\" thread may start running.

forkThread :: a  -> (b -> b)

forkThread x = forkAfter 0 x 

-- | Yield execution for a given amount of time (in milliseconds).
-- Specifying zero time interval means minimal possible interval.
-- Accuracy of the actual time the execution resumes after highly
-- depends on the web browser, and cannot be guaranteed in most cases.
-- Behavior is not specified if negative interval length is supplied.

yieldMs :: Prelude.Int -> (a -> Bool)

yieldMs n k = forkAfter n k True

infixl 0 |>>|

-- | Execute two CPS actions sequentially without passing result from the first
-- to the second. Use of this combinator results in more efficient Javascript
-- generated.

(|>>|) :: ((a -> a) -> b) -> (c -> c)
a |>>| b = (a $ id) `seq` b

-- | Data type to reflect the state of message box

data MBStatus a b = MBIdle -- ^ message box is idle: no message available for pickup,
                           --   no thread is waiting for a message
                  | MBWaiting a -- ^ a thread is waiting: its remainder of computation
                                --  is stored in the message box
                  | MBMessage b -- ^ a message is available: it will be picked up
                                --   by any thread that calls 'recvMsg'

-- | Create a message box, initially in idle state
-- Message box is a mutable object that can be in one of three states:
-- idle, waiting, or message-ready. The two functions, sendMsg and recvMsg
-- create a finite state machine with the following state transitions table:
--  

-- |@   ST0                    ST1 @

-- |@                sendMsg       recvMsg @

-- |@  Idle          Msg. Ready    Idle * @

-- |@  Waiting       Msg. Ready    Waiting ** @

-- |@  Msg. Ready    Msg. Ready ** Idle @

-- |@ @

-- |@  *  "No message" indicator will be returned @

-- |@  ** "Deadlock avoided" indicator will be returned. Message box status @

-- |@      will not change. @

--  
-- | A message box cannot be attached to a specific thread to restrict
-- operations that other threads may perform on it. However, message boxes
-- are not global; they are usually created by a parent thread and passed to
-- child threads as arguments, so number af threads that may access particular
-- message box is limited anyway.


msgBox :: CPS x (JSRef (MBStatus a b))

msgBox k = newJSRef MBIdle $ \mb -> k mb

-- | A type for a message box.

type MSGBOX a b = JSRef (MBStatus a b)

-- | Send a message via message box. The function returns either `return' or `fail'
-- in a given monad (usually a list or Maybe). That is, if sending a message fails
-- (message box was neither Idle ir Waiting) then Nothing, or [] will be returned.
-- If a message was sent, Just () or [()] will be returned.

sendMsg :: (Monad m) => JSRef (MBStatus Bool b) -> b -> CPS Bool (m ())

sendMsg mb msg k =
  readJSRef mb $ \c ->
  case c of
    MBWaiting k2 -> 
      writeJSRef mb (MBMessage msg) |>>|
      forkAfter 0 (k $ return ()) (k2)
    MBIdle -> 
      writeJSRef mb (MBMessage msg) |>>| 
      yieldMs 0 $ 
      k (return ())
    MBMessage _ -> k (fail "Deadlock avoided")

-- | Send a message via message box, ignoring the result.

sendMsg_ :: JSRef (MBStatus Bool b) -> b -> CPS Bool Bool

sendMsg_ mb v k = sendMsg mb v $ \x ->
  case x of
    Nothing -> k True
    Just _ -> k True

-- | Broadcast a message to multiple message boxes. Result will be ignored
-- (hence the trailing underscore in name).

broadCastMsg_ :: a               -- ^message type to send
              -> [MSGBOX Bool a] -- ^list of MsgBoxes to broadcast over
              -> CPS Bool Bool   -- ^result (continuation always gets True)

broadCastMsg_ v [] k = k True
broadCastMsg_ v (m:ms) k = sendMsg_ m v $ \_ -> broadCastMsg_ v ms k


-- | Receive a message from a message box. The function returns either `return' or `fail'
-- in a given monad (usually a list or Maybe). That is, if there was no message,
-- then Nothing, or [] will be returned. If a message was received, Just x or [x]
-- will be returned, where x is the message received.

recvMsg :: (Monad m) => JSRef (MBStatus Bool b) -> CPS Bool (m b)

recvMsg mb k =
  readJSRef mb $ \m ->
  case m of
    MBMessage b -> k (return b)
    MBWaiting a -> k (fail "Deadlock avoided")
    MBIdle -> do
      writeJSRef mb (MBWaiting $ kk) |>>| True
      where kk = readJSRef mb $ \c ->
                 c `seq` writeJSRef mb MBIdle |>>|
                 case c of
                   MBIdle -> k (fail "No message")
                   MBWaiting a -> k (fail "Deadlock avoided")
                   MBMessage b -> k (return b)



