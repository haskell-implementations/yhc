-- | Do very simple peephole optimisations, mostly to do with slides and pops
module ByteCode.Peep(bcPeep) where

import ByteCode.Type

-- | Simple peephole optimizer
bcPeep :: [BCDecl] -> [BCDecl]
bcPeep ds = map peepDecl ds


peepDecl :: BCDecl -> BCDecl
peepDecl (Fun n p z as cs cn pr sk nd fl) = Fun n p z as (peepCode cs) cn pr sk nd fl
peepDecl x                                = x

peepCode :: Code -> Code
peepCode (CLinear is) = CLinear is'
    where
    (cs,us) = unzip is
    cs'     = peepIns cs
    is'     = map (\i -> (i,emptyUS)) cs'

peepIns :: [Ins] -> [Ins]
peepIns (NEED_HEAP n:NEED_HEAP m:is)  = peepIns (NEED_HEAP (n+m):is)
peepIns (NEED_HEAP n:NEED_STACK m:is) = peepIns (NEED_HEAP n:is)
peepIns (SLIDE 0:is)                  = peepIns is
peepIns (SLIDE n:SLIDE m:is)          = peepIns (SLIDE (n+m):is)
peepIns (SLIDE n:RETURN:is)           = peepIns (RETURN:is)
peepIns (SLIDE n:RETURN_EVAL:is)      = peepIns (RETURN_EVAL:is)
peepIns (POP 0:is)                    = peepIns is
peepIns (POP n:POP m:is)              = peepIns (POP (n+m):is)
peepIns (POP n:RETURN:is)             = peepIns (RETURN:is)
peepIns (POP n:RETURN_EVAL:is)        = peepIns (RETURN_EVAL:is)
peepIns (EVAL:RETURN:is)              = peepIns (RETURN_EVAL:is)
peepIns (EVAL:RETURN_EVAL:is)         = peepIns (RETURN_EVAL:is)
peepIns (EVAL:EVAL:is)                = peepIns (EVAL:is)
peepIns (i:is)                        = i : peepIns is
peepIns []                            = []
