-- | Function to turns bytecode graph structure back into a linear list of instructions
module ByteCode.Flatten(bcFlatten) where

import ByteCode.Type
import ByteCode.Graph
import Control.Monad.State
import qualified Data.Set as Set
import List(sortBy)

--------------------------------------------------------------------------------------------------------

type Flattener a = GraphMonad (Set.Set GLabel) a

flMark :: GLabel -> Flattener Bool
flMark m = gWriteX $ \ s -> (Set.insert m s, m `Set.member` s)

flIsMarked :: GLabel -> Flattener Bool
flIsMarked m = gReadX $ \ s -> m `Set.member` s

--------------------------------------------------------------------------------------------------------

-- | Turn bytecode represented as a graph into bytecode represented as a linear sequence
--   of instructions
bcFlatten :: [BCDecl] -> [BCDecl]
bcFlatten ds = map flDecl ds

-- flatten a single declaration
flDecl :: BCDecl -> BCDecl
flDecl (Fun n p z as cs cn pr st nd fl) = Fun n p z as (flCode cs) cn pr st nd fl
flDecl x                                = x

-- flatten a code block
flCode :: Code -> Code
flCode (CGraph start graph jumps) = CLinear (is ++ [(END_CODE, emptyUS)])
    where
    st = GState start graph jumps Set.empty
    is = evalState (flGraph start) st

-- flatten a program graph into a linear list of instructions
flGraph :: GLabel -> Flattener [UseIns]
flGraph label =
    do marked <- flMark label
       if marked then
          return [(JUMP (toLabel label), emptyUS)]
        else
          do node <- gGetNode label
             ret <- case node of
                        GLinear ins eval next ->
                           flLinear label ins eval next

                        GIf true false ->
                           do ts <- flGraph true
                              fs <- flGraph false
                              return $ (JUMP_FALSE (toLabel false),emptyUS) : ts ++ fs

                        GCase int alts mdef ->
                           do ass <- mapM (\(t,j) -> flGraph j) alts
                              ds <- maybe (return []) flGraph mdef
                              let alts'  = map (\(t,j) -> (t, toLabel j)) alts
                                  mdef'  = maybe Nothing (Just . toLabel) mdef
                                  sw     = switch int alts' mdef'

                              return $ (sw,emptyUS) : concat ass ++ ds
                        GReturn -> return [(RETURN,emptyUS)]
                        GDead   -> error $ "flGraph: somehow reached dead code! "++show label
             return $ (LABEL (toLabel label),emptyUS) : ret

-- flatten a linear block of code, checks whether it
-- must end up in a return and inserts appropriate instructions. Otherwise
-- if the next block is marked then it jumps to it, or if it's not marked
-- then it flattens and appends it.
flLinear :: GLabel -> [UseIns] -> Bool -> GLabel -> Flattener [UseIns]
flLinear label isus eval next =
    do ret <- gAlwaysReturns next
       rest <- if ret then
                 if eval then return [(RETURN_EVAL,emptyUS)] else return [(RETURN,emptyUS)]
                else
                 do jumps <- gGetJumpers next
                    let jumps' = Set.delete label jumps
                    markeds <- mapM flIsMarked (Set.toList jumps')
                    -- if all the nodes that jump to next are marked ...
                    if and markeds then
                      -- then this must be the last one, so it's safe to inline
                      flGraph next
                     else
                      -- otherwise insert a jump and let a later one deal with it.
                      return [(JUMP (toLabel next), emptyUS)]
       return $ isus ++ rest

-- converts a graph label to a normal label
toLabel :: GLabel -> Label
toLabel (GLabel label) = label

-- choose the right swith instruction
switch :: Bool -> [(Tag,Label)] -> Maybe Label -> Ins
switch True  alts (Just def) = INT_SWITCH alts def
switch False alts (Just def) = LOOKUP_SWITCH alts def
switch False alts Nothing    = TABLE_SWITCH alts'
    where
    alts' = map snd $ sortBy (\(t,x) (u,y) -> compare t u) alts
