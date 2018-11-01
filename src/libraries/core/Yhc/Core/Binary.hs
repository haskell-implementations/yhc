module Yhc.Core.Binary where
import Yhc.Core.Type
import Yhc.Core.Internal.Binary
import Control.Monad

instance Binary Core
    where put_ bh x = case x of
                          Core x1 x2 x3 x4 -> do if useTag then putByte bh 0 else return ()
                                                 put_ bh x1
                                                 put_ bh x2
                                                 put_ bh x3
                                                 put_ bh x4
                   where useTag = (>) 1 1
          get bh = do h <- if useTag then getByte bh else return 0
                      case h of
                          0 -> do x1 <- get bh
                                  x2 <- get bh
                                  x3 <- get bh
                                  x4 <- get bh
                                  return (Core x1 x2 x3 x4)
                          _ -> fail "invalid binary data found"
                  where useTag = (>) 1 1

instance Binary CoreData
    where put_ bh x = case x of
                          CoreData x1 x2 x3 -> do if useTag then putByte bh 0 else return ()
                                                  put_ bh x1
                                                  put_ bh x2
                                                  put_ bh x3
                   where useTag = (>) 1 1
          get bh = do h <- if useTag then getByte bh else return 0
                      case h of
                          0 -> do x1 <- get bh
                                  x2 <- get bh
                                  x3 <- get bh
                                  return (CoreData x1 x2 x3)
                          _ -> fail "invalid binary data found"
                  where useTag = (>) 1 1

instance Binary CoreCtor
    where put_ bh x = case x of
                          CoreCtor x1 x2 -> do if useTag then putByte bh 0 else return ()
                                               put_ bh x1
                                               put_ bh x2
                   where useTag = (>) 1 1
          get bh = do h <- if useTag then getByte bh else return 0
                      case h of
                          0 -> do x1 <- get bh
                                  x2 <- get bh
                                  return (CoreCtor x1 x2)
                          _ -> fail "invalid binary data found"
                  where useTag = (>) 1 1

instance Binary CoreFunc
    where put_ bh x = case x of
                          CoreFunc x1 x2 x3 -> do if useTag then putByte bh 0 else return ()
                                                  put_ bh x1
                                                  put_ bh x2
                                                  put_ bh x3
                          CorePrim x1 x2 x3 x4 x5 x6 -> do if useTag
                                                            then putByte bh 1
                                                            else return ()
                                                           put_ bh x1
                                                           put_ bh x2
                                                           put_ bh x3
                                                           put_ bh x4
                                                           put_ bh x5
                                                           put_ bh x6
                   where useTag = (>) 2 1
          get bh = do h <- if useTag then getByte bh else return 0
                      case h of
                          0 -> do x1 <- get bh
                                  x2 <- get bh
                                  x3 <- get bh
                                  return (CoreFunc x1 x2 x3)
                          1 -> do x1 <- get bh
                                  x2 <- get bh
                                  x3 <- get bh
                                  x4 <- get bh
                                  x5 <- get bh
                                  x6 <- get bh
                                  return (CorePrim x1 x2 x3 x4 x5 x6)
                          _ -> fail "invalid binary data found"
                  where useTag = (>) 2 1

instance Binary CoreExpr
    where put_ bh x = case x of
                          CoreCon x1 -> do if useTag then putByte bh 0 else return ()
                                           put_ bh x1
                          CoreVar x1 -> do if useTag then putByte bh 1 else return ()
                                           put_ bh x1
                          CoreFun x1 -> do if useTag then putByte bh 2 else return ()
                                           put_ bh x1
                          CoreApp x1 x2 -> do if useTag then putByte bh 3 else return ()
                                              put_ bh x1
                                              put_ bh x2
                          CoreLam x1 x2 -> do if useTag then putByte bh 4 else return ()
                                              put_ bh x1
                                              put_ bh x2
                          CoreCase x1 x2 -> do if useTag then putByte bh 5 else return ()
                                               put_ bh x1
                                               put_ bh x2
                          CoreLet x1 x2 -> do if useTag then putByte bh 6 else return ()
                                              put_ bh x1
                                              put_ bh x2
                          CorePos x1 x2 -> do if useTag then putByte bh 7 else return ()
                                              put_ bh x1
                                              put_ bh x2
                          CoreLit x1 -> do if useTag then putByte bh 8 else return ()
                                           put_ bh x1
                   where useTag = (>) 9 1
          get bh = do h <- if useTag then getByte bh else return 0
                      case h of
                          0 -> do x1 <- get bh
                                  return (CoreCon x1)
                          1 -> do x1 <- get bh
                                  return (CoreVar x1)
                          2 -> do x1 <- get bh
                                  return (CoreFun x1)
                          3 -> do x1 <- get bh
                                  x2 <- get bh
                                  return (CoreApp x1 x2)
                          4 -> do x1 <- get bh
                                  x2 <- get bh
                                  return (CoreLam x1 x2)
                          5 -> do x1 <- get bh
                                  x2 <- get bh
                                  return (CoreCase x1 x2)
                          6 -> do x1 <- get bh
                                  x2 <- get bh
                                  return (CoreLet x1 x2)
                          7 -> do x1 <- get bh
                                  x2 <- get bh
                                  return (CorePos x1 x2)
                          8 -> do x1 <- get bh
                                  return (CoreLit x1)
                          _ -> fail "invalid binary data found"
                  where useTag = (>) 9 1

instance Binary CoreLit
    where put_ bh x = case x of
                          CoreInt x1 -> do if useTag then putByte bh 0 else return ()
                                           put_ bh x1
                          CoreInteger x1 -> do if useTag then putByte bh 1 else return ()
                                               put_ bh x1
                          CoreChr x1 -> do if useTag then putByte bh 2 else return ()
                                           put_ bh x1
                          CoreStr x1 -> do if useTag then putByte bh 3 else return ()
                                           put_ bh x1
                          CoreFloat x1 -> do if useTag then putByte bh 4 else return ()
                                             put_ bh x1
                          CoreDouble x1 -> do if useTag then putByte bh 5 else return ()
                                              put_ bh x1
                   where useTag = (>) 6 1
          get bh = do h <- if useTag then getByte bh else return 0
                      case h of
                          0 -> do x1 <- get bh
                                  return (CoreInt x1)
                          1 -> do x1 <- get bh
                                  return (CoreInteger x1)
                          2 -> do x1 <- get bh
                                  return (CoreChr x1)
                          3 -> do x1 <- get bh
                                  return (CoreStr x1)
                          4 -> do x1 <- get bh
                                  return (CoreFloat x1)
                          5 -> do x1 <- get bh
                                  return (CoreDouble x1)
                          _ -> fail "invalid binary data found"
                  where useTag = (>) 6 1

instance Binary CorePat
    where put_ bh x = case x of
                          PatCon x1 x2 -> do if useTag then putByte bh 0 else return ()
                                             put_ bh x1
                                             put_ bh x2
                          PatLit x1 -> do if useTag then putByte bh 1 else return ()
                                          put_ bh x1
                          PatDefault -> if useTag then putByte bh 2 else return ()
                   where useTag = (>) 3 1
          get bh = do h <- if useTag then getByte bh else return 0
                      case h of
                          0 -> do x1 <- get bh
                                  x2 <- get bh
                                  return (PatCon x1 x2)
                          1 -> do x1 <- get bh
                                  return (PatLit x1)
                          2 -> return PatDefault
                          _ -> fail "invalid binary data found"
                  where useTag = (>) 3 1
