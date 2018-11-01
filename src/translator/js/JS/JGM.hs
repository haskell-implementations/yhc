module JS.JGM where

import JS.OptOpt
import Yhc.Core
import Control.Monad
import Control.Monad.State

data JG = JG {
   stateCnt :: Int 
--   ,dollarFunc :: String
  ,coreRef :: Core
  ,optOpt  :: JSOptFlags 
  ,analSat :: CoreExpr -> Bool
  ,analSel :: CoreExpr -> Int
  ,analStrict :: CoreFuncName -> [Bool]
}               
                
getCnt :: JGM Int
                
getCnt = do     
  c <- gets stateCnt
  st <- get       
  put st {stateCnt = c + 1}
  return c
    
    
type JGM a = State JG a


