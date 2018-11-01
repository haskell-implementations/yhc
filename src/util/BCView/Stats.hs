-- | Utility function to display interesting statistics about 
--   as sequence of bytecode declarations
module ByteCode.Stats(bcStats) where

import ByteCode.ByteCode
import ByteCode.Show(strIns)
import AssocTree
import List

type Stats = AssocTree Ins Int

-- | Print bytecode statistics to standard out
bcStats :: [BCDecl] -> IO ()
bcStats ds = putStrLn str
    where
    str   = concatMap (\(i,n) -> strIns i ++ " = " ++ show n ++ "\n") list'
    list  = filter (\(i,_) -> hasArg i) (listAT st')
    list' = sortBy (\(_,n) (_,m) -> compare m n) list
    st'   = fst $ mapAccumL (\ st d -> (stDecl d st, ())) st ds
    st    = initAT


stDecl :: BCDecl -> Stats -> Stats
stDecl (Fun name pos arity args code consts prim stack) st = stCode code st
stDecl x                                                st = st
    
stCode :: Code -> Stats -> Stats
stCode bs st = fst $ mapAccumL (\ st b -> (stBlock b st,())) st bs

stBlock :: Block -> Stats -> Stats
stBlock (BLinear is) st = fst $ mapAccumL (\ st i -> (stIns i st,())) st is

stIns :: (Ins,UseSet) -> Stats -> Stats
stIns (i,_) st = 
    let i' = normIns i 
    in case Map.lookup i' st of
        Nothing -> addAT st const i' 1
        Just x -> Map.update (Just . (+1)) i' st


hasArg :: Ins -> Bool
hasArg (PUSH _) = True
hasArg (PUSH_ZAP _) = True
hasArg (ZAP_STACK _) = True
hasArg (PUSH_ARG _) = True
hasArg (PUSH_ZAP_ARG _) = True
hasArg (ZAP_ARG _) = True
hasArg (PUSH_INT _) = True
hasArg (PUSH_CHAR _) = True
hasArg (PUSH_CONST _) = True
hasArg (MK_AP _ _) = True
hasArg (MK_PAP _ _) = True
hasArg (MK_CON _ _) = True
hasArg (APPLY _) = True
hasArg (SLIDE _) = True
hasArg (POP _) = True
hasArg (SELECT _) = True
hasArg _          = False

normIns :: Ins -> Ins
normIns (MK_AP f n)         = MK_AP f 0
normIns (MK_CON c n)        = MK_CON c 0
normIns (UNPACK n)          = UNPACK 0
normIns (TABLE_SWITCH _)    = TABLE_SWITCH []
normIns (LOOKUP_SWITCH _ _) = LOOKUP_SWITCH [] Nothing
normIns (INT_SWITCH _ _)    = INT_SWITCH [] Nothing
normIns (JUMP_FALSE _)      = JUMP_FALSE 0
normIns (JUMP _)            = JUMP 0
normIns (LABEL _)           = LABEL 0
normIns i                   = i

